library(readxl)
library(minpack.lm)
library(dplyr)
library(zoo)
library(purrr)
library(ggplot2)
library(reticulate)
library(webshot2)
library(processx)
library(nlstools) 
library(plotly)
library(RColorBrewer)
library(shiny)
library(tidyr)

##############
# Laden Sie den DataFrame aus der RDS-Datei
Bedingungen_data <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/Bedingungen_data.rds")
Energieanteile_data <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/Energieanteile_data.rds")
EPOC_data_df_short <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/EPOC_data_df_short.rds")

# Arbeitsverzeichnis einstellen
setwd("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm")

# Pfad zum Ordner, der alle .xlsm Dateien enthält
folder_path <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm"

# Liste aller xlsm-Dateien im Ordner
file_list <- list.files(pattern = "\\.xlsm$")
file_list <- file_list[!grepl("^~\\$", file_list)] # Temporäre Excel-Dateien ausschließen

# Leere Listen für verschiedene Zwecke
VO2_list_roh <- list()
proband_test_list <- list()
start_vals_list <- list()
tau_list <- list()

#########################################

extract_data_to_list <- function(file_path) {
  # Liste zur Aufnahme der Daten aus den Tabellenblättern
  data_list <- list()
  
  # Extrahiere die ersten zwei Ziffern des Dateinamens
  file_name <- basename(file_path)
  prefix <- substr(file_name, 1, 2)
  
  # Liste der Tabellennamen im Excel-File
  sheet_names <- excel_sheets(file_path)
  
  # Filtern nach Tabellenblättern mit "_W_PCR_" im Namen
  relevant_sheets <- grep("_W_PCR", sheet_names, value = TRUE)
  
  # Schleife durch relevante Tabellenblätter
  for (sheet_name in relevant_sheets) {
    # Daten aus Spalte E bis Zeile 19 lesen (Anpassung der Zeilenzahl)
    data <- read_excel(file_path, sheet = sheet_name, range = "E14:E19", col_types = c("numeric"))
    
    # Entfernen der zweiten und vierten Zeile
    data <- data[-c(2, 4), ]
    
    # Daten in einen Vektor umwandeln und benennen
    data_vector <- c(A = data[1, 1], TauA = 45, B = data[2, 1], TauB = 360, C = data[3, 1])
    
    # Daten zur Liste hinzufügen, unter Verwendung des Prefix im Namen
    data_list[[paste0(prefix, "_", substr(sheet_name, start = 1, stop = 1))]] <- structure(data_vector, names = c("A", "TauA", "B", "TauB", "C"))
  }
  
  return(data_list)
}

# Aufrufen der Funktion und Speichern der Liste von "double"-Vektoren
data_list <- extract_data_to_list(file_list[1]) # Hier wird die Funktion mit der ersten Datei aufgerufen

# Leere Liste für die Startwerte initialisieren
start_vals_list <- list()

# Schleife durch alle Dateien im Ordner
for (file_path in file_list) {
  start_vals <- extract_data_to_list(file_path)
  # Verwende das Prefix im Namen anstelle des Dateipfades
  start_vals_list <- c(start_vals_list, start_vals)
}

#######################

# Schleife durch jede Datei im Ordner um VO2_list_roh zu erstellen
for (file_name in file_list) {
  file_path <- file.path(getwd(), file_name) # Vollständiger Pfad zur Datei
  
  # Alle Blätter lesen, die "_W_PCR" im Namen enthalten
  sheets <- excel_sheets(file_path)
  w_pcr_sheets <- grep("_W_PCR", sheets, value = TRUE)
  
  # Jedes relevante Blatt durchgehen
  for (sheet_name in w_pcr_sheets) {
    # Die ersten beiden Ziffern aus dem Datei- und Tabellenblattnamen extrahieren
    prefix <- substr(file_name, start = 1, stop = 2)
    tabellenblatt_prefix <- substr(sheet_name, start = 1, stop = 1)
    
    # Lese die Spalten U und V
    df <- read_excel(path = file_path, sheet = sheet_name, range = cell_cols(c("U:V")))
    
    # NA-Werte entfernen
    df <- na.omit(df)
    
    # Namen der Spalten setzen
    names(df) <- c("t_s", "VO2_l_min")
    
    # Hinzufügen zum Datensatz-Liste
    VO2_list_roh[[length(VO2_list_roh) + 1]] <- df
    
    # Proband und Testnummer zur Liste hinzufügen
    proband_test_list[[length(proband_test_list) + 1]] <- c(as.numeric(prefix), as.numeric(tabellenblatt_prefix))
    # Umbenennen des Dataframes in VO2_list_roh
    new_name <- paste(prefix, "_", tabellenblatt_prefix, sep = "")
    names(VO2_list_roh)[length(VO2_list_roh)] <- new_name
  }
}

# Funktion, um Daten bis t_s <= 900 zu filtern
filter_data <- function(df) {
  df <- df[df$t_s <= 900, ]
  return(df)
}
# Anwenden der Funktion auf jedes Element in VO2_list_roh
VO2_list_roh <- lapply(VO2_list_roh, filter_data)

# Probanden 02 und 03 entfernen
start_vals_list <- start_vals_list[!grepl("^(02|03)", names(start_vals_list))]
VO2_list_roh <- VO2_list_roh[!grepl("^(02|03)", names(VO2_list_roh))]

# Funktion zur Entfernung von Ausreißern und Glättung der Daten
replace_outliers <- function(df, df_name, column, start_vals, initial_window = 8, increment = 0.33334, iqr_factor = 2.0) {
  len <- length(df[[column]])
  rolling_q1 <- numeric(len)
  rolling_q3 <- numeric(len)
  
  # Ruhe-VO2-Wert (C) aus den Startwerten abrufen
  c_value <- start_vals[[df_name]][["C"]]
  
  # Werte unterhalb des Ruhe-VO2 mit NA ersetzen
  is_below_c <- df[[column]] < c_value 
  df[[column]][is_below_c] <- NA
  
  # Funktion zur Überprüfung der Differenzen und Ersetzung mit gleitendem Mittelwert
  replace_with_moving_average <- function(x, window_size = 5) {
    n <- length(x)
    to_replace <- logical(n)
    
    # Jeden Datenpunkt auf starke Abweichungen zu Nachbarpunkten prüfen
    for (i in 1:n) {
      prev_val <- if (i > 1) x[i-1] else NA
      next_val <- if (i < n) x[i+1] else NA
      
      # Prüfen, ob der aktuelle Wert oder beide Nachbarn NA sind
      if (is.na(x[i]) || (is.na(prev_val) && is.na(next_val))) {
        to_replace[i] <- TRUE
      } else {
        # Prozentuale Differenzen zu Nachbarpunkten berechnen
        diff_prev <- if (!is.na(prev_val) && !is.na(x[i])) abs(x[i] - prev_val) / prev_val else NA
        diff_next <- if (!is.na(next_val) && !is.na(x[i])) abs(x[i] - next_val) / next_val else NA
        
        # Prüfen, ob beide Differenzen über 30% liegen
        if (!is.na(diff_prev) && !is.na(diff_next) && diff_prev > 0.30 && diff_next > 0.30) {
          to_replace[i] <- TRUE
        } else {
          to_replace[i] <- FALSE
        }
      }
    }
    
    # Identifizierte Punkte durch gleitenden Mittelwert ersetzen
    for (i in which(to_replace)) {
      start <- max(1, i - floor(window_size/2))
      end <- min(n, i + floor(window_size/2))
      surrounding_vals <- x[start:end]
      if (all(is.na(surrounding_vals))) {
        x[i] <- NA
      } else {
        x[i] <- mean(surrounding_vals, na.rm = TRUE)
      }
    }
    
    return(x)
  }
  
  # Differenzüberprüfung und Ersetzung anwenden
  df[[column]] <- replace_with_moving_average(df[[column]])
  
  # Log-Transformation der Daten für weitere Verarbeitung
  df[[column]] <- log(df[[column]])
  
  # Berechnung der gleitenden Quartile mit variabler Fenstergröße
  for (i in 1:len) {
    window_size <- ceiling(initial_window + increment * (i - 1))
    window_start <- max(1, i - floor(window_size/2))
    window_end <- min(len, i + floor(window_size/2))
    window_vals <- df[[column]][window_start:window_end]
    window_vals <- na.omit(window_vals)  # NA-Werte ignorieren
    if (length(window_vals) > 0) {
      rolling_q1[i] <- quantile(window_vals, 0.25, na.rm = TRUE)
      rolling_q3[i] <- quantile(window_vals, 0.75, na.rm = TRUE)
    } else {
      rolling_q1[i] <- NA
      rolling_q3[i] <- NA
    }
  }
  
  # Berechnung des gleitenden Interquartilsabstands (IQR)
  rolling_iqr <- rolling_q3 - rolling_q1
  
  # Identifizieren und Ersetzen von Ausreißern basierend auf IQR
  lower_bound <- rolling_q1 - iqr_factor * rolling_iqr
  upper_bound <- rolling_q3 + iqr_factor * rolling_iqr
  is_outlier <- df[[column]] < lower_bound | df[[column]] > upper_bound
  df[[column]][is_outlier] <- NA  # Ausreißer mit NA ersetzen
  
  # Rücktransformation der Daten aus dem Log-Bereich
  df[[column]] <- exp(df[[column]])
  
  return(df)
}

# Anwenden der Funktion auf jede VO2_list_roh Komponente
VO2_list <- mapply(function(df, df_name) replace_outliers(df, df_name, "VO2_l_min", start_vals_list), VO2_list_roh, names(VO2_list_roh), SIMPLIFY = FALSE)

# Anwenden der Funktion auf jede VO2_list Komponente
VO2_list <- lapply(VO2_list, function(df) {
  df <- df[df$t_s <= 600, ]  # Nur die Wertepaare behalten, bei denen t_s kleiner gleich 600 ist
  return(df)
})

plots_list <- list() # Eine leere Liste, um die Plots zu speichern
# Schleife über die Länge der VO2_list (oder start_vals_list, da sie gleich lang sein sollten)
for(i in 1:length(VO2_list)) {
  current_VO2_data <- VO2_list[[i]] # Aktuelle VO2 Daten für die Iteration
  
  # Erstellung des Plotly-Diagramms für das aktuelle VO2_data
  current_plot <- plot_ly(data = current_VO2_data, x = ~t_s, y = ~VO2_l_min, type = 'scatter', mode = 'markers') %>%
    layout(title = sprintf("EPOC für %s", names(start_vals_list)[i]),
           margin = list(t = 40),xaxis = list(title = "Zeit [s]"), yaxis = list(title = "VO2 (l/min)", range = c(0, 6.5))) %>%
    add_trace(text = ~paste("Zeit: ", t_s, "s<br>VO2: ", VO2_l_min, "l/min"),
              hoverinfo = "text")
  
  # Hinzufügen des aktuellen Plots zur Liste
  plots_list[[i]] <- current_plot
}
plots_list[24]
#plots_list[25:30]

########
# Initialisierung der Listen
plots_list <- list()
plots_list_smoothed <- list()

# Funktion zur Glättung der Daten
smooth_data <- function(data) {
  n <- length(data)
  smoothed <- numeric(n)
  for (i in 1:n) {
    start <- max(1, i - 3)
    end <- min(n, i + 3)
    smoothed[i] <- mean(data[start:end], na.rm = TRUE)
  }
  return(smoothed)
}

# Schleife über die Länge der VO2_list
for(i in 1:length(VO2_list)) {
  current_VO2_data <- VO2_list[[i]] # Aktuelle VO2 Daten für die Iteration
  
  # Erstellung des Plotly-Diagramms für das aktuelle VO2_data
  current_plot <- plot_ly(data = current_VO2_data, x = ~t_s, y = ~VO2_l_min, type = 'scatter', mode = 'markers') %>%
    layout(title = sprintf("EPOC für %s", names(start_vals_list)[i]),
           margin = list(t = 40),xaxis = list(title = "Zeit [s]"), yaxis = list(title = "VO2 (l/min)", range = c(0, 6.5))) %>%
    add_trace(text = ~paste("Zeit: ", t_s, "s<br>VO2: ", VO2_l_min, "l/min"),
              hoverinfo = "text")
  
  # Hinzufügen des aktuellen Plots zur Liste
  plots_list[[i]] <- current_plot
  
  # Glättung der Daten
  smoothed_VO2 <- smooth_data(current_VO2_data$VO2_l_min)
  
  # Erstellung des Plotly-Diagramms für die geglätteten Daten
  smoothed_plot <- plot_ly(x = current_VO2_data$t_s, y = smoothed_VO2, type = 'scatter', mode = 'markers') %>%
    layout(title = sprintf("Geglättete EPOC für %s", names(start_vals_list)[i]),
           margin = list(t = 40),xaxis = list(title = "Zeit [s]"), yaxis = list(title = "Geglättete VO2 (l/min)", range = c(0, 6.5))) %>%
    add_trace(text = paste("Zeit: ", current_VO2_data$t_s, "s<br>Geglättete VO2: ", round(smoothed_VO2, 3), "l/min"),
              hoverinfo = "text")
  
  # Hinzufügen des geglätteten Plots zur Liste
  plots_list_smoothed[[i]] <- smoothed_plot
  
  # Überschreiben der rohen VO2-Daten mit den geglätteten Daten
  VO2_list[[i]]$VO2_l_min <- smoothed_VO2
}

# Print VO2_list Eintrag
#print(VO2_list[[24]] %>% mutate(across(everything(), ~ formatC(.x, format = "f", digits = 3))), n = 161)

plots_list[24]
plots_list_smoothed[24]
######

# Dateipfad festlegen
#file_path <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Berechnung_Energieanteile 1.9..xlsm"

# Lesen der spezifischen Spalten U und V aus dem Tabellenblatt 'W_PCR'
#df_VO2 <- read_excel(path = file_path, sheet = "W_PCR", range = cell_cols(c("U:V")))
# Entfernen von #NV oder NA-Werten
#df_VO2 <- na.omit(df_VO2) 

# Namen der Spalten setzen
#names(df_VO2) <- c("t_s", "VO2_l_min")

# Modellgleichung definieren
model_equation <- VO2_l_min ~ A * exp(-t_s/TauA) + B * exp(-t_s/TauB) + C

# CE_max festlegen
CE_max <- 20.926

# Leere Liste zum Speichern der erstellten Plotly-Diagramme initialisieren
plotly_list <- list()
plotly_list_tau <- list()
plotly_list_slow <- list()

# model_fast als Funktion definieren, um die Integration durchzuführen
model_fast_func <- function(t_s) {
  coef_estimates['A'] * exp(-t_s / coef_estimates['TauA'])
}

# Leere Liste für die Startwerte initialisieren
start_vals_list <- list()

# Schleife durch alle Dateien im Ordner
for (file_path in file_list) {
  start_vals <- extract_data_to_list(file_path)
  # Verwende das Prefix im Namen anstelle des Dateipfades
  start_vals_list <- c(start_vals_list, start_vals)
}

# Leere Liste für die unteren und oberen Grenzen für C initialisieren
lower_bounds_list <- list()
upper_bounds_list <- list()

# Entfernen von Einträgen, die mit "02" oder "03" beginnen
start_vals_list <- start_vals_list[!grepl("^(02|03)", names(start_vals_list))]

# C_Referenz aus Bedingungen_data hinzufügen
# Funktion zum Extrahieren der Proband- und Nr-Werte aus dem Namen
extract_proband_nr <- function(name) {
  parts <- strsplit(name, "_")[[1]]
  return(list(Proband = parts[1], Nr = as.numeric(parts[2])))
}

# Schleife durch jedes Element in start_vals_list
for (name in names(start_vals_list)) {
  # Extrahiere Proband und Nr aus dem Namen
  info <- extract_proband_nr(name)
  
  # Finde den entsprechenden Eintrag in Bedingungen_data
  matching_row <- Bedingungen_data[Bedingungen_data$Proband == info$Proband & 
                                     Bedingungen_data$Nr == info$Nr, ]
  
  # Wenn eine übereinstimmende Zeile gefunden wurde
  if (nrow(matching_row) > 0) {
    # Füge C_Referenz zum entsprechenden Eintrag in start_vals_list hinzu
    start_vals_list[[name]]$C_Referenz <- matching_row$VO2_Referenz[1]
  } else {
    # Wenn keine Übereinstimmung gefunden wurde, setze C_Referenz auf NA
    start_vals_list[[name]]$C_Referenz <- NA
    warning(paste("Keine Übereinstimmung gefunden für", name))
  }
}

# Aussuchen ob C oder C_Referenz für Modellierung verwendet werden soll --> Überschreiben von C mit C_Referenz
# Schleife durch jedes Element in start_vals_list
for (name in names(start_vals_list)) {
  # Überschreibe C mit C_Referenz, wenn C_Referenz vorhanden ist
  if (!is.na(start_vals_list[[name]]$C_Referenz)) {
    start_vals_list[[name]]$C <- start_vals_list[[name]]$C_Referenz
  }
  
  # Lösche C_Referenz
  start_vals_list[[name]]$C_Referenz <- NULL
}


#saveRDS(start_vals_list, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/start_vals_list.rds")
#saveRDS(VO2_list, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/VO2_list.rds")


############################################

# Leereen df für die EPOC Werte initialisieren
EPOC_data_df <- data.frame(
  Proband = numeric(0),
  Nr = character(0),
  VO2_fast = numeric(0),
  WPCR = numeric(0),
  R_squared = numeric(0),
  tau_off = numeric(0),
  A = numeric(0),
  TauA = numeric(0),
  B = numeric(0),
  TauB = numeric(0),
  C = numeric(0)
)

# Initialisierung von coef_estimates 
coef_estimates <- list('A' = NA, 'TauA' = NA, 'B' = NA, 'TauB' = NA, 'C' = NA)

# Ordner für die Plot-Dateien definieren
plot_folder <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/images/"

#########Tau bestimmen#############

# Schleife durch die Kombinationen in start_vals_list und VO2_list
for (i in 1:length(start_vals_list)) {
  tryCatch({
    # Aktuelle Startwerte und Daten verwenden
    start_vals <- start_vals_list[[i]]
    current_VO2_data <- VO2_list[[i]]
    
    # Filtern der Daten für den spezifischen Bereich für B und Tau
    plot_ly(data = current_VO2_data, x = ~t_s, y = ~VO2_l_min, type = 'scatter', mode = 'markers') %>%
      layout(title = sprintf("EPOC für %s", names(start_vals_list)[i]),
             margin = list(t = 40),xaxis = list(title = "Zeit [s]"), yaxis = list(title = "VO2 (l/min)", range = c(0, 6.5))) %>%
      add_trace(text = ~paste("Zeit: ", t_s, "s<br>VO2: ", VO2_l_min, "l/min"),
                hoverinfo = "text")
    
    # Berechnung des Startwerts für B mit Ignorieren von NA Werten
    max_x_start <- max(current_VO2_data$VO2_l_min[1:10], na.rm = TRUE)
    
    # Fitten des B * exp(-t_s/Tau) + C Terms mit dem Startwert für B und C
    model_tau <- nlsLM(VO2_l_min ~ x * exp(-t_s/Tau) + C, 
                       data = current_VO2_data, 
                       start = list(x = max_x_start, Tau = 45, C = max_x_start * 1/3), 
                       lower = c(x = -Inf, Tau = 10, C = -Inf),  
                       upper = c(x = Inf, Tau = 600, C = Inf),  
                       control = nls.control(maxiter = 1024))
    
    # Extrahieren der geschätzten Werte für B, Tau und C
    estimates_tau <- coef(model_tau)
    
    # Ergebnisse anzeigen
    model_summary <- summary(model_tau)
    print(model_summary)
    
    cat(sprintf("Modellanpassung für i = %d war erfolgreich\n", i))
    
    # Modellwerte berechnen
    current_VO2_data$model_tau <- predict(model_tau, newdata = current_VO2_data)
    current_VO2_data$model_C <- rep(estimates_tau['C'], nrow(current_VO2_data))
    
    # Parameter aus dem Modell extrahieren und die festen Werte hinzufügen
    coef_estimates <- coef(model_tau)
    
    # Teilmodelle berechnen
    current_VO2_data$model_tau <- coef_estimates['x'] * exp(-current_VO2_data$t_s / coef_estimates['Tau']) + coef_estimates['C']
    
    # Gleichungstext zusammenstellen
    eq_text_tau <- sprintf("V̇O₂(t) = %.2f * e<sup>-(t / %.2f)</sup> + %.2f", 
                           coef_estimates['x'], coef_estimates['Tau'],
                           coef_estimates['C'])
    
    # Berechnung von R^2 unter Ignorierung von NA Werten
    SSR <- sum(resid(model_tau)^2, na.rm = TRUE)
    SST <- sum((current_VO2_data$VO2_l_min - mean(current_VO2_data$VO2_l_min, na.rm = TRUE))^2, na.rm = TRUE)
    R_squared <- 1 - (SSR / SST)
    
    # Berechnung von tau2
    tau <- estimates_tau['Tau']
    tau2 <- 2 * estimates_tau['Tau']
    tau8 <- 6 * estimates_tau['Tau']
    
    # Berechnung des VO2-Wertes bei t_s = 0 und t_s = tau unter Verwendung der Modellfunktion ohne C
    VO2_model_0 <- coef_estimates['x'] 
    VO2_model_tau1 <- coef_estimates['x'] * exp(-coef_estimates['Tau'] / coef_estimates['Tau']) 
    VO2_model_tau2 <- coef_estimates['x'] * exp(-2 * coef_estimates['Tau'] / coef_estimates['Tau']) 
    VO2_model_tau3 <- coef_estimates['x'] * exp(-3 * coef_estimates['Tau'] / coef_estimates['Tau']) 
    VO2_model_tau4 <- coef_estimates['x'] * exp(-4 * coef_estimates['Tau'] / coef_estimates['Tau']) 
    VO2_model_tau8 <- coef_estimates['x'] * exp(-6 * coef_estimates['Tau'] / coef_estimates['Tau'])
    
    # Tau - Modellwerte berechnen
    Tau <- coef_estimates['Tau']
    tau1_model <- (VO2_model_tau1 / VO2_model_0) * 100
    tau2_model <- (VO2_model_tau2 / VO2_model_0) * 100
    tau3_model <- (VO2_model_tau3 / VO2_model_0) * 100
    tau4_model <- (VO2_model_tau4 / VO2_model_0) * 100
    tau8_model <- (VO2_model_tau8 / VO2_model_0) * 100
    # Mit dem natürlichen Logarithmus von 2, ln(2) = 0.693 die Halbwerszeit berechnen
    t_halb <- Tau * log(2) 
    
    # Erstellen eines benannten Vektors mit allen Werten
    tau_values <- c(tau1 = tau1_model, tau2 = tau2_model, tau3 = tau3_model, tau4 = tau4_model, tau8 = tau8_model, Tau= Tau, t_halb = t_halb)
    
    # Hinzufügen des Vektors zur tau_list mit dem Namen des Probanden als Schlüssel
    tau_list[[names(start_vals_list)[i]]] <- tau_values
    
    # Anpassung der Plot-Erstellung, um current_VO2_data zu verwenden
    p <- plot_ly() %>%
      add_trace(data = current_VO2_data, x = ~t_s, y = ~VO2_l_min, type = 'scatter', mode = 'markers+lines', 
                name = 'V&#775;O<sub>2</sub>', 
                marker = list(color = 'rgba(38, 131, 198, 0.9)', size = 5.0),
                line = list(color = 'rgba(38, 131, 198, 1.0)', width = 0.65, dash = '4 4')) %>%
      add_trace(data = current_VO2_data, x = ~t_s, y = ~model_tau, type = 'scatter', mode = 'lines', 
                name = 'Modellfunktion', line = list(color = '#EF6F6A')) %>%
      layout(title = sprintf("EPOC: %s", names(start_vals_list)[i]),
             margin = list(t = 40),
             xaxis = list(title = "Zeit [s]", range = c(0, 600), autorange = FALSE),
             yaxis = list(title = "V&#775;O<sub>2</sub> [l&#183;min<sup>-1</sup>]", range = c(0, 6.5)),
             shapes = list(
               list(
                 type = "line", x0 = tau, x1 = tau, y0 = 0, y1 = 6.5,
                 line = list(color = "gray", width = 1, dash = "dash")
               ),
               list(
                 type = "line", x0 = tau2, x1 = tau2, y0 = 0, y1 = 6.5,
                 line = list(color = "gray", width = 1, dash = "dash")
               ),
               list(
                 type = "line", x0 = tau8, x1 = tau8, y0 = 0, y1 = 6.5,
                 line = list(color = "gray", width = 1, dash = "dash")
               )
             ),
             annotations = list(           
               list(
                 x = tau, y = 5.35, text = sprintf("τ: %.1f s", tau), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                 textangle = -90, font = list(size = 11)
               ),
               list(
                 x = tau2, y = 5.35, text = sprintf("2τ: %.1f s", tau2), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                 textangle = -90, font = list(size = 11)
               ),
               list(
                 x = tau8, y = 5.35, text = sprintf("8τ: %.1f s", tau8), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                 textangle = -90, font = list(size = 11)
               ),
               list(
                 x = 400, 
                 y = 5.3, text = sprintf("tau: %.1f", tau), 
                 showarrow = FALSE, 
                 xanchor = "left", 
                 yanchor = "bottom", 
                 font = list(
                   family = "Arial, sans-serif",
                   size = 12,
                   color = "black")
               ),
               list(
                 x = 400, 
                 y = 5.6,  # Angepasste Position für R^2
                 text = paste("R²:", round(R_squared, 2)),
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   family = "Arial, sans-serif",
                   size = 12,
                   color = "black"
                 )
               ),
               list(
                 x = 400, 
                 y = 5.9,  # Angepasste Position für Gleichungstext
                 text = eq_text_tau,  # Dies nutzt die vorher definierte eq_text_tau Variable
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   family = "Arial, sans-serif",
                   size = 12,
                   color = "black"
                 )
               ),
               list(
                 x = 400, 
                 y = 5.0,  
                 text = paste("T<sub>1/2</sub>:", round(t_halb, 2),"s"),
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   family = "Arial, sans-serif",
                   size = 12,
                   color = "black"
                 )
               )
             )
      )
    
    # Plotly-Diagramm zur Liste hinzufügen
    plotly_list_tau[[i]] <- p
    
    # Diagramm als HTML-Datei speichern mit dem Namen aus names(start_vals_list)[i]
    #htmlwidgets::saveWidget(p, file = paste0(plot_folder, names(start_vals_list)[i], "_tau.html"))
    
    # Aktuelles Tau zur start_vals Liste hinzufügen
    start_vals_list[[i]]$Tau <- estimates_tau['Tau']
    
  }, error = function(e) {
    cat(sprintf("Fehler bei der Modellanpassung für i = %d: %s\n", i, e$message))
  }, warning = function(w) {
    cat(sprintf("Warnung bei der Modellanpassung für i = %d: %s\n", i, w$message))
  })
}

# Extrahieren des Namens-Teils
name_part <- names(start_vals_list)[i]
file_name <- paste0(name_part, "_tau.png")  

# Speichern des Plots als svg
#plotly::save_image(
  #plotly_list_tau[[i]],
  #file = paste0(plot_folder, file_name),
  #width = 1100, 
  #height = 450
#)

# Speichern der tau-Plots als PNG
#for (i in 1:length(plotly_list_tau)) {
  #name_part <- names(start_vals_list)[i]
  #file_name <- paste0(name_part, "_tau.png")  # Geändert zu .png
  
  #plotly::save_image(
    #plotly_list_tau[[i]],
    #file = paste0(plot_folder, file_name),
    #width = 1100, 
    #height = 450,
    #scale = 2
  #)
#}

# Erstellen von Tau_df aus tau_list
Tau_df <- data.frame(Name = character(), Tau_off = numeric(), stringsAsFactors = FALSE) # Spalte gleich "Tau_off" genannt

# Iteriere über jedes Element in tau_list
for (element_name in names(tau_list)) {
  # Hole das Element aus tau_list
  element <- tau_list[[element_name]]
  
  # Extrahiere den Wert von Tau und füge ihn zum DataFrame hinzu
  # Überprüfe, ob der Tau-Wert vorhanden ist (manchmal heißt er 'Tau.Tau', manchmal nur 'Tau')
  tau_value_off <- NA # Standardwert, falls nicht gefunden
  if (!is.null(element[["Tau.Tau"]])) {
    tau_value_off <- element[["Tau.Tau"]]
  } else if (!is.null(element[["Tau"]])) {
    tau_value_off <- element[["Tau"]]
  } else {
    warning(paste("Konnte Tau-Wert ('Tau.Tau' oder 'Tau') für", element_name, "in tau_list nicht finden."))
  }
  
  Tau_df <- rbind(Tau_df, data.frame(Name = element_name, Tau_off = tau_value_off, stringsAsFactors = FALSE))
}

Bedingungen_data_for_join <- Bedingungen_data %>%
  mutate(Name = paste0(as.character(Proband), "_", Nr)) %>% # Erstellt z.B. "01_1"
  select(Name, Bedingung, Intensität, tau_on) # Wählt die relevanten Spalten aus

# Füge die Spalten aus Bedingungen_data zu Tau_df hinzu mittels left_join
# Der Join erfolgt über die gemeinsame Spalte "Name"
Tau_df <- dplyr::left_join(Tau_df, Bedingungen_data_for_join, by = "Name")

EPOC_data_df <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/MetabolicAnalytics-Apps.github.io/rds/EPOC_data_df.rds")


if ("Proband" %in% names(EPOC_data_df) && "Nr" %in% names(EPOC_data_df)) {
  
  EPOC_data_for_join <- EPOC_data_df %>%
    mutate(Name = paste0(as.character(Proband), "_", Nr)) %>% # Erstellt z.B. "01_1"
    select(Name, VO2_SS_percent, HR_percent) # Wählt die relevanten Spalten aus
  
  # Füge die Spalten aus EPOC_data_for_join zu Tau_df hinzu mittels left_join
  # Der Join erfolgt über die gemeinsame Spalte "Name"
  Tau_df <- dplyr::left_join(Tau_df, EPOC_data_for_join, by = "Name")
  
} else {
  warning("Die Spalten 'Proband' und/oder 'Nr' wurden nicht in EPOC_data_df gefunden. Die Spalten VO2_SS_percent und HR_percent konnten nicht hinzugefügt werden.")
}

print(Tau_df)

plotly_list_tau[[1]]
plotly_list_tau[[3]]
plotly_list_tau[[6]]

### Regression zwischen VO2_SS_percent und tau_off ###
# 1. Proband-Spalte (falls nicht schon vorhanden) und Daten vorbereiten
Tau_df <- Tau_df %>%
  mutate(Proband = factor(substr(Name, 1, 2))) %>%
  filter(!is.na(Tau_off) & !is.na(VO2_SS_percent)) # NA entfernen

# 2. Lineare Regression berechnen (Tau_off ~ VO2_SS_percent)
lin_reg_vo2 <- lm(Tau_off ~ VO2_SS_percent, data = Tau_df) # ACHSEN GETAUSCHT
summary_lin_reg_vo2 <- summary(lin_reg_vo2)

# 3. Statistik für Annotations formatieren
f_stat_vo2 <- summary_lin_reg_vo2$fstatistic
p_value_vo2_formatted <- "N/A"
f_stat_text_vo2 <- "F-Statistik N/A"

if (!is.null(f_stat_vo2) && all(c("value", "numdf", "dendf") %in% names(f_stat_vo2))) {
  p_value_vo2 <- pf(f_stat_vo2["value"], f_stat_vo2["numdf"], f_stat_vo2["dendf"], lower.tail = FALSE)
  p_value_vo2_formatted <- format.pval(p_value_vo2, digits = 3, eps = 0.001)
  f_stat_text_vo2 <- sprintf("F(%d, %d) = %.2f, p = %s",
                             round(f_stat_vo2["numdf"]),
                             round(f_stat_vo2["dendf"]),
                             round(f_stat_vo2["value"], 2),
                             p_value_vo2_formatted)
}

# Gleichung anpassen (Tau_off ist jetzt Y)
equation_text_vo2 <- sprintf("Tau<sub>off</sub> = %.2f · V̇O₂<sub>SS</sub>%% + %.2f",
                             coef(lin_reg_vo2)["VO2_SS_percent"], # Koeffizient für die NEUE X-Achse
                             coef(lin_reg_vo2)["(Intercept)"])
r_squared_text_vo2 <- sprintf("R² = %.2f", summary_lin_reg_vo2$r.squared)

# 4. Punkte für Regressionslinie berechnen (basierend auf NEUER X-Achse)
vo2_ss_percent_seq <- seq(0, 100, length.out = 100) # Sequenz für NEUE X-Achse
regression_values_vo2 <- predict(lin_reg_vo2, newdata = data.frame(VO2_SS_percent = vo2_ss_percent_seq)) # Vorhersage für Tau_off

# 5. Farbpalette erstellen (falls nicht schon global vorhanden)
n_probanden <- length(levels(Tau_df$Proband))
basis_palette <- brewer.pal(min(n_probanden, 11), "Spectral")
if (n_probanden > 11) {
  color_palette <- colorRampPalette(basis_palette)(n_probanden)
} else {
  color_palette <- basis_palette[1:n_probanden]
}

# 6. Plotly-Diagramm erstellen (Achsen getauscht)
p_TauOff_VO2ss <- plot_ly(data = Tau_df, x = ~VO2_SS_percent, y = ~Tau_off) %>% # ACHSEN GETAUSCHT
  add_markers(type = 'scatter', mode = 'markers',
              color = ~Proband,
              colors = color_palette,
              marker = list(size = 9, opacity = 0.8),
              # Hovertext anpassen
              text = ~paste("Proband:", Proband, "<br>VO2_SS%:", round(VO2_SS_percent,1), "%<br>Tau_off:", round(Tau_off,1), "s"),
              hoverinfo = "text") %>%
  add_lines(x = vo2_ss_percent_seq, y = regression_values_vo2, name = "Regression", # ACHSEN GETAUSCHT
            line = list(color = 'rgb(50, 50, 50)', width = 2, dash="dash"),
            hoverinfo = 'skip',
            showlegend = FALSE) %>%
  layout(title = "Zusammenhang Tau<sub>off</sub> [s] und VO₂<sub>SS</sub> [%]", # Titel angepasst
         margin = list(t = 50, b = 50, l= 50, r=20),
         xaxis = list(title = "VO₂<sub>SS</sub> [%]", # ACHSENBESCHRIFTUNG GETAUSCHT
                      range = c(0, 100),         # BEREICH FÜR NEUE X-ACHSE
                      zeroline = FALSE),
         yaxis = list(title = "Tau<sub>off</sub> [s]",   # ACHSENBESCHRIFTUNG GETAUSCHT
                      range = c(0, 80),           # BEREICH FÜR NEUE Y-ACHSE
                      zeroline = FALSE),
         legend = list(title = list(text='<b> Proband </b>')),
         hovermode = "closest") %>%
  # Annotationen anpassen (Positionen evtl. justieren)
  add_annotations(text = equation_text_vo2, x = 55, y = 75, # Position angepasst
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12)) %>%
  add_annotations(text = r_squared_text_vo2, x = 55, y = 70, # Position angepasst
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12)) %>%
  add_annotations(text = f_stat_text_vo2, x = 55, y = 65, # Position angepasst
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12))

# Plot anzeigen
p_TauOff_VO2ss

### Regression zwischen VO2_SS_percent und tau_off DURCH ??? !!! ###

# HIER DEN GEWÜNSCHTEN FESTEN Y-ACHSENABSCHNITT (INTERCEPT) FESTLEGEN:
fixed_intercept <- 10.0
# ---------------------------------------------------------------------------

# 1. Daten vorbereiten und Response-Variable transformieren
Tau_df_filtered <- Tau_df %>%
  mutate(Proband = factor(substr(Name, 1, 2))) %>%
  filter(!is.na(Tau_off) & !is.na(VO2_SS_percent)) %>%
  # Transformiere y: y' = y - fixed_intercept
  mutate(Tau_off_transformed = Tau_off - fixed_intercept)

# 2. Lineare Regression DURCH DEN URSPRUNG für die transformierte Variable berechnen
# Das Modell ist jetzt: (Tau_off - fixed_intercept) ~ 0 + VO2_SS_percent
# Dies schätzt die Steigung 'a' im ursprünglichen Modell Tau_off = a * VO2_SS_percent + fixed_intercept
lin_reg_vo2_fixed <- lm(Tau_off_transformed ~ 0 + VO2_SS_percent, data = Tau_df_filtered)
summary_lin_reg_vo2_fixed <- summary(lin_reg_vo2_fixed)

# 3. Statistik für Annotations formatieren
coef_summary_fixed <- summary(lin_reg_vo2_fixed)$coefficients
# Die geschätzte Steigung 'a'
slope_estimate_fixed <- coef_summary_fixed["VO2_SS_percent", "Estimate"]
slope_p_value_fixed <- coef_summary_fixed["VO2_SS_percent", "Pr(>|t|)"]
slope_p_formatted_fixed <- format.pval(slope_p_value_fixed, digits = 3, eps = 0.001)

# Gleichung mit geschätzter Steigung und dem FESTEN Intercept
equation_text_vo2_fixed <- sprintf("Tau<sub>off</sub> = %.2f · V̇O₂<sub>SS</sub>%% + %.2f (fixed)",
                                   slope_estimate_fixed,
                                   fixed_intercept) # Verwende den festgelegten Wert

# R-squared bezieht sich auf das transformierte Modell (y-b ~ x durch den Ursprung)
# Seine Interpretation ändert sich.
r_squared_text_vo2_fixed <- sprintf("R² (origin, for y - %.1f) = %.2f",
                                    fixed_intercept, summary_lin_reg_vo2_fixed$r.squared)
slope_sig_text_fixed <- sprintf("Slope p-value = %s", slope_p_formatted_fixed)

# 4. Punkte für Regressionslinie in 1er Schritten berechnen
vo2_ss_percent_int_seq <- seq(0, 100, by = 1) # Integer-Sequenz für X-Achse

# Vorhersage für die *transformierte* Variable (y' = y - b)
predicted_transformed <- predict(lin_reg_vo2_fixed, newdata = data.frame(VO2_SS_percent = vo2_ss_percent_int_seq))

# Rücktransformation zur Vorhersage auf der Originalskala (y = y' + b)
regression_values_vo2_fixed <- predicted_transformed + fixed_intercept

# DataFrame für die Regressionspunkte erstellen
regression_df_fixed <- data.frame(
  VO2_SS_perc = vo2_ss_percent_int_seq,
  Tau_off_pred = regression_values_vo2_fixed # Verwende die rücktransformierten Vorhersagen
)

# 5. Farbpalette erstellen (falls nicht schon global vorhanden)
n_probanden <- length(levels(Tau_df_filtered$Proband))
basis_palette <- brewer.pal(min(n_probanden, 11), "Spectral")
if (n_probanden > 11) {
  color_palette <- colorRampPalette(basis_palette)(n_probanden)
} else {
  color_palette <- basis_palette[1:n_probanden]
}

# 6. Plotly-Diagramm erstellen
p_TauOff_VO2ss_fixed <- plot_ly() %>%
  # Originaldatenpunkte hinzufügen
  add_markers(data = Tau_df_filtered, x = ~VO2_SS_percent, y = ~Tau_off,
              type = 'scatter', mode = 'markers',
              color = ~Proband,
              colors = color_palette,
              marker = list(size = 9, opacity = 0.8),
              text = ~paste("Proband:", Proband, "<br>VO2_SS%:", round(VO2_SS_percent,1), "%<br>Tau_off:", round(Tau_off,1), "s"),
              hoverinfo = "text",
              name = ~Proband,
              legendgroup = ~Proband,
              showlegend = TRUE) %>%
  # Regressionslinie UND Punkte hinzufügen (durchgezogen, mit hoverbaren Punkten)
  add_trace(data = regression_df_fixed, x = ~VO2_SS_perc, y = ~Tau_off_pred,
            type = 'scatter', mode = 'lines+markers', # Linien UND Marker
            line = list(color = 'rgb(50, 50, 50)', width = 2, dash = 'solid'), # Durchgezogene Linie
            marker = list(color = 'rgb(50, 50, 50)', size = 5, symbol = 'circle'), # Kleine Marker für die Linie
            # Hovertext für die Punkte auf der Regressionslinie
            text = ~paste("Model Prediction<br>VO2_SS%:", VO2_SS_perc, "%<br>Predicted Tau_off:", round(Tau_off_pred, 1), "s"),
            hoverinfo = "text",
            name = sprintf("Regression (Intercept=%.1f)", fixed_intercept), # Name für Legende angepasst
            showlegend = TRUE) %>%
  layout(title = sprintf("Zusammenhang Tau<sub>off</sub> [s] und VO₂<sub>SS</sub> [%%] (Intercept fest bei %.1f)", fixed_intercept), # Titel angepasst
         margin = list(t = 50, b = 50, l= 50, r=20),
         xaxis = list(title = "VO₂<sub>SS</sub> [%]",
                      range = c(0, 100),
                      zeroline = TRUE,   # Nulllinie anzeigen
                      zerolinecolor = 'grey',
                      zerolinewidth = 1),
         yaxis = list(title = "Tau<sub>off</sub> [s]",
                      # Y-Achsenbereich ggf. anpassen, um fixed_intercept und Daten gut darzustellen
                      range = c(min(0, fixed_intercept - 5, na.rm=TRUE), max(80, fixed_intercept + 5, na.rm=TRUE)),
                      zeroline = TRUE,   # Nulllinie anzeigen
                      zerolinecolor = 'grey',
                      zerolinewidth = 1),
         legend = list(title = list(text='<b> Proband / Modell </b>')),
         hovermode = "closest") %>%
  # Annotationen anpassen (Positionen evtl. justieren)
  # Positionierung relativ zum Datenbereich
  add_annotations(text = equation_text_vo2_fixed, x = 5, y = max(Tau_df_filtered$Tau_off, na.rm = TRUE) * 0.98,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12)) %>%
  add_annotations(text = r_squared_text_vo2_fixed, x = 5, y = max(Tau_df_filtered$Tau_off, na.rm = TRUE) * 0.93,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12)) %>%
  add_annotations(text = slope_sig_text_fixed, x = 5, y = max(Tau_df_filtered$Tau_off, na.rm = TRUE) * 0.88,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12))

# Plot anzeigen
p_TauOff_VO2ss_fixed

### Feste Regression von x = 0 -> 15 über x = 50 -> 35... ###
# Punkt 1 (x1, y1) -> (VO2_SS_%, Tau_off)
fixed_point1_x <- 0.0
fixed_point1_y <- 15.0

# Punkt 2 (x2, y2) -> (VO2_SS_%, Tau_off)
fixed_point2_x <- 50.0
fixed_point2_y <- 35.0
# ---------------------------------------------------------------------------

# 1. Berechne Steigung (m) und y-Achsenabschnitt (c) der definierten Linie
if (fixed_point2_x == fixed_point1_x) {
  stop("Error: Die x-Koordinaten der festen Punkte dürfen nicht gleich sein.")
}
slope_fixed_line <- (fixed_point2_y - fixed_point1_y) / (fixed_point2_x - fixed_point1_x)
intercept_fixed_line <- fixed_point1_y - slope_fixed_line * fixed_point1_x

# 2. Daten vorbereiten (Originaldatenpunkte)
Tau_df_filtered <- Tau_df %>%
  mutate(Proband = factor(substr(Name, 1, 2))) %>%
  filter(!is.na(Tau_off) & !is.na(VO2_SS_percent)) # NA entfernen

# 3. Berechne R-squared der Daten im Vergleich zur FESTGELEGTEN Linie
#    Dies ist keine statistische Modellanpassung, sondern ein Gütemaß zur definierten Linie.
# 3a. Vorhersagen für die Originaldatenpunkte basierend auf der festgelegten Linie
Tau_df_filtered <- Tau_df_filtered %>%
  mutate(Tau_off_pred_fixed = slope_fixed_line * VO2_SS_percent + intercept_fixed_line)

# 3b. Summe der quadrierten Residuen (SSR) zur festgelegten Linie
ssr_fixed <- sum((Tau_df_filtered$Tau_off - Tau_df_filtered$Tau_off_pred_fixed)^2, na.rm = TRUE)

# 3c. Totale Summe der Quadrate (SST) der Originaldaten
sst_fixed <- sum((Tau_df_filtered$Tau_off - mean(Tau_df_filtered$Tau_off, na.rm = TRUE))^2, na.rm = TRUE)

# 3d. R-squared berechnen
#    Kann negativ sein, wenn die definierte Linie schlechter ist als der Mittelwert!
r_squared_vs_fixed <- NA # Initialisieren
if (sst_fixed > 0) { # Vermeide Division durch Null
  r_squared_vs_fixed <- 1 - (ssr_fixed / sst_fixed)
}


# 4. Punkte für die DEFINIERTE Linie in 1er Schritten berechnen
vo2_ss_percent_int_seq <- seq(0, 100, by = 1)
tau_off_fixed_line_values <- slope_fixed_line * vo2_ss_percent_int_seq + intercept_fixed_line

# DataFrame für die Punkte der definierten Linie erstellen
fixed_line_df <- data.frame(
  VO2_SS_perc = vo2_ss_percent_int_seq,
  Tau_off_calc = tau_off_fixed_line_values
)

# 5. Text für Annotationen erstellen
equation_text_fixed_line <- sprintf("Tau<sub>off</sub> = %.2f · V̇O₂<sub>SS</sub>%% + %.2f (Defined)",
                                    slope_fixed_line,
                                    intercept_fixed_line)
points_text <- sprintf("Line through (%.1f, %.1f) & (%.1f, %.1f)",
                       fixed_point1_x, fixed_point1_y,
                       fixed_point2_x, fixed_point2_y)
r_squared_text_fixed <- sprintf("R² (vs Defined Line) = %.2f", r_squared_vs_fixed)


# 6. Farbpalette erstellen (falls nicht schon global vorhanden)
n_probanden <- length(levels(Tau_df_filtered$Proband))
basis_palette <- brewer.pal(min(n_probanden, 11), "Spectral")
if (n_probanden > 11) {
  color_palette <- colorRampPalette(basis_palette)(n_probanden)
} else {
  color_palette <- basis_palette[1:n_probanden]
}

# 7. Plotly-Diagramm erstellen
p_TauOff_VO2ss_defined_styled <- plot_ly() %>%
  # Originaldatenpunkte hinzufügen
  add_markers(data = Tau_df_filtered, x = ~VO2_SS_percent, y = ~Tau_off,
              type = 'scatter', mode = 'markers',
              color = ~Proband,
              colors = color_palette,
              marker = list(size = 9, opacity = 0.8),
              text = ~paste("Proband:", Proband, "<br>VO2_SS%:", round(VO2_SS_percent,1), "%<br>Tau_off:", round(Tau_off,1), "s"),
              hoverinfo = "text",
              name = ~Proband,
              legendgroup = ~Proband,
              showlegend = TRUE) %>%
  # Definierte Linie UND Punkte hinzufügen (hellgrau, gestrichelt, kleine Punkte)
  add_trace(data = fixed_line_df, x = ~VO2_SS_perc, y = ~Tau_off_calc,
            type = 'scatter', mode = 'lines+markers', # Linien UND Marker
            line = list(color = 'rgb(200, 200, 200)', width = 2, dash = 'dash'), # Hellgrau, Gestrichelt
            marker = list(color = 'rgb(200, 200, 200)', size = 4, symbol = 'circle'), # Hellgrau, Kleine Punkte
            # Hovertext für die Punkte auf der definierten Linie
            text = ~paste("Defined Line<br>VO2_SS%:", VO2_SS_perc, "%<br>Calculated Tau_off:", round(Tau_off_calc, 1), "s"),
            hoverinfo = "text",
            name = "Defined Line", # Name für Legende
            showlegend = TRUE) %>%
  # Die zwei definierenden Punkte hervorheben (dunkelgrau)
  add_markers(x = c(fixed_point1_x, fixed_point2_x), y = c(fixed_point1_y, fixed_point2_y),
              type = 'scatter', mode = 'markers',
              marker = list(color = 'rgb(50, 50, 50)', size = 10, symbol = 'x', line = list(width=2, color='rgb(50, 50, 50)')), # Dunkelgrau
              hoverinfo = 'text',
              text = c(sprintf("Fixed Point 1<br>(%.1f, %.1f)", fixed_point1_x, fixed_point1_y),
                       sprintf("Fixed Point 2<br>(%.1f, %.1f)", fixed_point2_x, fixed_point2_y)),
              name = "Fixed Points",
              showlegend = TRUE) %>%
  layout(title = sprintf("Zusammenhang Tau<sub>off</sub> vs. VO₂<sub>SS</sub>%% mit definierter Linie"),
         margin = list(t = 50, b = 50, l= 50, r=20),
         xaxis = list(title = "VO₂<sub>SS</sub> [%]",
                      range = c(0, 100),
                      zeroline = TRUE, zerolinecolor = 'grey', zerolinewidth = 1),
         yaxis = list(title = "Tau<sub>off</sub> [s]",
                      range = c(min(0, fixed_point1_y, fixed_point2_y, na.rm=TRUE) - 5,
                                max(80, fixed_point1_y, fixed_point2_y, max(Tau_df_filtered$Tau_off, na.rm = TRUE)) + 5),
                      zeroline = TRUE, zerolinecolor = 'grey', zerolinewidth = 1),
         legend = list(title = list(text='<b> Proband / Linie </b>')),
         hovermode = "closest") %>%
  # Annotationen
  add_annotations(text = equation_text_fixed_line, x = 5, y = max(fixed_line_df$Tau_off_calc, na.rm=TRUE) * 1.05,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12)) %>%
  add_annotations(text = points_text, x = 5, y = max(fixed_line_df$Tau_off_calc, na.rm=TRUE) * 0.98,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12)) %>%
  add_annotations(text = r_squared_text_fixed, x = 5, y = max(fixed_line_df$Tau_off_calc, na.rm=TRUE) * 0.91, # Hinzugefügt
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12))


# Plot anzeigen
p_TauOff_VO2ss_defined_styled

### Regression zwischen HR_percent und tau_off ###

# 1. Proband-Spalte (falls nicht schon vorhanden) und Daten vorbereiten
Tau_df <- Tau_df %>%
  mutate(Proband = factor(substr(Name, 1, 2))) %>%
  filter(!is.na(Tau_off) & !is.na(HR_percent)) # NA entfernen

# 2. Lineare Regression berechnen (Tau_off ~ HR_percent)
lin_reg_hr <- lm(Tau_off ~ HR_percent, data = Tau_df) # ACHSEN GETAUSCHT
summary_lin_reg_hr <- summary(lin_reg_hr)

# 3. Statistik für Annotations formatieren
f_stat_hr <- summary_lin_reg_hr$fstatistic
p_value_hr_formatted <- "N/A"
f_stat_text_hr <- "F-Statistik N/A"

if (!is.null(f_stat_hr) && all(c("value", "numdf", "dendf") %in% names(f_stat_hr))) {
  p_value_hr <- pf(f_stat_hr["value"], f_stat_hr["numdf"], f_stat_hr["dendf"], lower.tail = FALSE)
  p_value_hr_formatted <- format.pval(p_value_hr, digits = 3, eps = 0.001)
  f_stat_text_hr <- sprintf("F(%d, %d) = %.2f, p = %s",
                            round(f_stat_hr["numdf"]),
                            round(f_stat_hr["dendf"]),
                            round(f_stat_hr["value"], 2),
                            p_value_hr_formatted)
}

# Gleichung anpassen (Tau_off ist jetzt Y)
equation_text_hr <- sprintf("Tau<sub>off</sub> = %.2f · HR%% + %.2f",
                            coef(lin_reg_hr)["HR_percent"], # Koeffizient für die NEUE X-Achse
                            coef(lin_reg_hr)["(Intercept)"])
r_squared_text_hr <- sprintf("R² = %.2f", summary_lin_reg_hr$r.squared)

# 4. Punkte für Regressionslinie berechnen (basierend auf NEUER X-Achse)
hr_percent_seq <- seq(50, 100, length.out = 100) # Sequenz für NEUE X-Achse
regression_values_hr <- predict(lin_reg_hr, newdata = data.frame(HR_percent = hr_percent_seq)) # Vorhersage für Tau_off

# 5. Farbpalette erstellen (falls nicht schon global vorhanden)
n_probanden <- length(levels(Tau_df$Proband))
basis_palette <- brewer.pal(min(n_probanden, 11), "Spectral")
if (n_probanden > 11) {
  color_palette <- colorRampPalette(basis_palette)(n_probanden)
} else {
  color_palette <- basis_palette[1:n_probanden]
}

# 6. Plotly-Diagramm erstellen (Achsen getauscht)
p_TauOff_HR <- plot_ly(data = Tau_df, x = ~HR_percent, y = ~Tau_off) %>% # ACHSEN GETAUSCHT
  add_markers(type = 'scatter', mode = 'markers',
              color = ~Proband,
              colors = color_palette,
              marker = list(size = 9, opacity = 0.8),
              # Hovertext anpassen
              text = ~paste("Proband:", Proband, "<br>HR%:", round(HR_percent,1), "%<br>Tau_off:", round(Tau_off,1), "s"),
              hoverinfo = "text") %>%
  add_lines(x = hr_percent_seq, y = regression_values_hr, name = "Regression", # ACHSEN GETAUSCHT
            line = list(color = 'rgb(50, 50, 50)', width = 2, dash="dash"),
            hoverinfo = 'skip',
            showlegend = FALSE) %>%
  layout(title = "Zusammenhang Tau<sub>on</sub> [s] und HR [%]", # Titel angepasst
         margin = list(t = 50, b = 50, l= 50, r=20),
         xaxis = list(title = "HR [%]",           # ACHSENBESCHRIFTUNG GETAUSCHT
                      range = c(50, 100),         # BEREICH FÜR NEUE X-ACHSE
                      zeroline = FALSE),
         yaxis = list(title = "Tau<sub>on</sub> [s]",   # ACHSENBESCHRIFTUNG GETAUSCHT
                      range = c(0, 80),           # BEREICH FÜR NEUE Y-ACHSE
                      zeroline = FALSE),
         legend = list(title = list(text='<b> Proband </b>')),
         hovermode = "closest") %>%
  # Annotationen anpassen (Positionen evtl. justieren)
  add_annotations(text = equation_text_hr, x = 55, y = 75, # Position angepasst
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12)) %>%
  add_annotations(text = r_squared_text_hr, x = 55, y = 70, # Position angepasst
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12)) %>%
  add_annotations(text = f_stat_text_hr, x = 55, y = 65, # Position angepasst
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12))

# Plot anzeigen
p_TauOff_HR

### Regression für tau_on ###
# 1. Proband-Spalte (falls nicht schon vorhanden) und Daten vorbereiten
Tau_df <- Tau_df %>%
  mutate(Proband = factor(substr(Name, 1, 2))) %>%
  filter(!is.na(tau_on) & !is.na(VO2_SS_percent)) # NA entfernen

# 2. Lineare Regression berechnen (tau_on ~ VO2_SS_percent)
lin_reg_vo2 <- lm(tau_on ~ VO2_SS_percent, data = Tau_df) # ACHSEN GETAUSCHT
summary_lin_reg_vo2 <- summary(lin_reg_vo2)

# 3. Statistik für Annotations formatieren
f_stat_vo2 <- summary_lin_reg_vo2$fstatistic
p_value_vo2_formatted <- "N/A"
f_stat_text_vo2 <- "F-Statistik N/A"

if (!is.null(f_stat_vo2) && all(c("value", "numdf", "dendf") %in% names(f_stat_vo2))) {
  p_value_vo2 <- pf(f_stat_vo2["value"], f_stat_vo2["numdf"], f_stat_vo2["dendf"], lower.tail = FALSE)
  p_value_vo2_formatted <- format.pval(p_value_vo2, digits = 3, eps = 0.001)
  f_stat_text_vo2 <- sprintf("F(%d, %d) = %.2f, p = %s",
                             round(f_stat_vo2["numdf"]),
                             round(f_stat_vo2["dendf"]),
                             round(f_stat_vo2["value"], 2),
                             p_value_vo2_formatted)
}

# Gleichung anpassen (tau_on ist jetzt Y)
equation_text_vo2 <- sprintf("Tau<sub>on</sub> = %.2f · V̇O₂<sub>SS</sub>%% + %.2f",
                             coef(lin_reg_vo2)["VO2_SS_percent"], # Koeffizient für die NEUE X-Achse
                             coef(lin_reg_vo2)["(Intercept)"])
r_squared_text_vo2 <- sprintf("R² = %.2f", summary_lin_reg_vo2$r.squared)

# 4. Punkte für Regressionslinie berechnen (basierend auf NEUER X-Achse)
vo2_ss_percent_seq <- seq(0, 100, length.out = 100) # Sequenz für NEUE X-Achse
regression_values_vo2 <- predict(lin_reg_vo2, newdata = data.frame(VO2_SS_percent = vo2_ss_percent_seq)) # Vorhersage für tau_on

# 5. Farbpalette erstellen (falls nicht schon global vorhanden)
n_probanden <- length(levels(Tau_df$Proband))
basis_palette <- brewer.pal(min(n_probanden, 11), "Spectral")
if (n_probanden > 11) {
  color_palette <- colorRampPalette(basis_palette)(n_probanden)
} else {
  color_palette <- basis_palette[1:n_probanden]
}

# 6. Plotly-Diagramm erstellen (Achsen getauscht)
p_tauon_VO2ss <- plot_ly(data = Tau_df, x = ~VO2_SS_percent, y = ~tau_on) %>% # ACHSEN GETAUSCHT
  add_markers(type = 'scatter', mode = 'markers',
              color = ~Proband,
              colors = color_palette,
              marker = list(size = 9, opacity = 0.8),
              # Hovertext anpassen
              text = ~paste("Proband:", Proband, "<br>VO2_SS%:", round(VO2_SS_percent,1), "%<br>tau_on:", round(tau_on,1), "s"),
              hoverinfo = "text") %>%
  add_lines(x = vo2_ss_percent_seq, y = regression_values_vo2, name = "Regression", # ACHSEN GETAUSCHT
            line = list(color = 'rgb(50, 50, 50)', width = 2, dash="dash"),
            hoverinfo = 'skip',
            showlegend = FALSE) %>%
  layout(title = "Zusammenhang Tau<sub>on</sub> [s] und VO₂<sub>SS</sub> [%]", # Titel angepasst
         margin = list(t = 50, b = 50, l= 50, r=20),
         xaxis = list(title = "VO₂<sub>SS</sub> [%]", # ACHSENBESCHRIFTUNG GETAUSCHT
                      range = c(0, 100),         # BEREICH FÜR NEUE X-ACHSE
                      zeroline = FALSE),
         yaxis = list(title = "Tau<sub>on</sub> [s]",   # ACHSENBESCHRIFTUNG GETAUSCHT
                      range = c(0, 80),           # BEREICH FÜR NEUE Y-ACHSE
                      zeroline = FALSE),
         legend = list(title = list(text='<b> Proband </b>')),
         hovermode = "closest") %>%
  # Annotationen anpassen (Positionen evtl. justieren)
  add_annotations(text = equation_text_vo2, x = 55, y = 75, # Position angepasst
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12)) %>%
  add_annotations(text = r_squared_text_vo2, x = 55, y = 70, # Position angepasst
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12)) %>%
  add_annotations(text = f_stat_text_vo2, x = 55, y = 65, # Position angepasst
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12))

# Plot anzeigen
p_tauon_VO2ss

### Regression für tau_on mit ??? bei x=0 ###

# HIER DEN GEWÜNSCHTEN FESTEN Y-ACHSENABSCHNITT (INTERCEPT) FESTLEGEN:
fixed_intercept <- 6.0
# ---------------------------------------------------------------------------

# 1. Daten vorbereiten und Response-Variable transformieren
Tau_df_filtered <- Tau_df %>%
  mutate(Proband = factor(substr(Name, 1, 2))) %>%
  filter(!is.na(tau_on) & !is.na(VO2_SS_percent)) %>%
  # Transformiere y: y' = y - fixed_intercept
  mutate(tau_on_transformed = tau_on - fixed_intercept)

# 2. Lineare Regression DURCH DEN URSPRUNG für die transformierte Variable berechnen
# Das Modell ist jetzt: (tau_on - fixed_intercept) ~ 0 + VO2_SS_percent
# Dies schätzt die Steigung 'a' im ursprünglichen Modell tau_on = a * VO2_SS_percent + fixed_intercept
lin_reg_vo2_fixed <- lm(tau_on_transformed ~ 0 + VO2_SS_percent, data = Tau_df_filtered)
summary_lin_reg_vo2_fixed <- summary(lin_reg_vo2_fixed)

# 3. Statistik für Annotations formatieren
coef_summary_fixed <- summary(lin_reg_vo2_fixed)$coefficients
# Die geschätzte Steigung 'a'
slope_estimate_fixed <- coef_summary_fixed["VO2_SS_percent", "Estimate"]
slope_p_value_fixed <- coef_summary_fixed["VO2_SS_percent", "Pr(>|t|)"]
slope_p_formatted_fixed <- format.pval(slope_p_value_fixed, digits = 3, eps = 0.001)

# Gleichung mit geschätzter Steigung und dem FESTEN Intercept
equation_text_vo2_fixed <- sprintf("Tau<sub>on</sub> = %.2f · V̇O₂<sub>SS</sub>%% + %.2f (fixed)",
                                   slope_estimate_fixed,
                                   fixed_intercept) # Verwende den festgelegten Wert

# R-squared bezieht sich auf das transformierte Modell (y-b ~ x durch den Ursprung)
# Seine Interpretation ändert sich.
r_squared_text_vo2_fixed <- sprintf("R² (origin, for y - %.1f) = %.2f",
                                    fixed_intercept, summary_lin_reg_vo2_fixed$r.squared)
slope_sig_text_fixed <- sprintf("Slope p-value = %s", slope_p_formatted_fixed)

# 4. Punkte für Regressionslinie in 1er Schritten berechnen
vo2_ss_percent_int_seq <- seq(0, 100, by = 1) # Integer-Sequenz für X-Achse

# Vorhersage für die *transformierte* Variable (y' = y - b)
predicted_transformed <- predict(lin_reg_vo2_fixed, newdata = data.frame(VO2_SS_percent = vo2_ss_percent_int_seq))

# Rücktransformation zur Vorhersage auf der Originalskala (y = y' + b)
regression_values_vo2_fixed <- predicted_transformed + fixed_intercept

# DataFrame für die Regressionspunkte erstellen
regression_df_fixed <- data.frame(
  VO2_SS_perc = vo2_ss_percent_int_seq,
  tau_on_pred = regression_values_vo2_fixed # Verwende die rücktransformierten Vorhersagen
)

# 5. Farbpalette erstellen (falls nicht schon global vorhanden)
n_probanden <- length(levels(Tau_df_filtered$Proband))
basis_palette <- brewer.pal(min(n_probanden, 11), "Spectral")
if (n_probanden > 11) {
  color_palette <- colorRampPalette(basis_palette)(n_probanden)
} else {
  color_palette <- basis_palette[1:n_probanden]
}

# 6. Plotly-Diagramm erstellen
p_tauon_VO2ss_fixed <- plot_ly() %>%
  # Originaldatenpunkte hinzufügen
  add_markers(data = Tau_df_filtered, x = ~VO2_SS_percent, y = ~tau_on,
              type = 'scatter', mode = 'markers',
              color = ~Proband,
              colors = color_palette,
              marker = list(size = 9, opacity = 0.8),
              text = ~paste("Proband:", Proband, "<br>VO2_SS%:", round(VO2_SS_percent,1), "%<br>tau_on:", round(tau_on,1), "s"),
              hoverinfo = "text",
              name = ~Proband,
              legendgroup = ~Proband,
              showlegend = TRUE) %>%
  # Regressionslinie UND Punkte hinzufügen (durchgezogen, mit hoverbaren Punkten)
  add_trace(data = regression_df_fixed, x = ~VO2_SS_perc, y = ~tau_on_pred,
            type = 'scatter', mode = 'lines+markers', # Linien UND Marker
            line = list(color = 'rgb(50, 50, 50)', width = 2, dash = 'solid'), # Durchgezogene Linie
            marker = list(color = 'rgb(50, 50, 50)', size = 5, symbol = 'circle'), # Kleine Marker für die Linie
            # Hovertext für die Punkte auf der Regressionslinie
            text = ~paste("Model Prediction<br>VO2_SS%:", VO2_SS_perc, "%<br>Predicted tau_on:", round(tau_on_pred, 1), "s"),
            hoverinfo = "text",
            name = sprintf("Regression (Intercept=%.1f)", fixed_intercept), # Name für Legende angepasst
            showlegend = TRUE) %>%
  layout(title = sprintf("Zusammenhang Tau<sub>on</sub> [s] und VO₂<sub>SS</sub> [%%] (Intercept fest bei %.1f)", fixed_intercept), # Titel angepasst
         margin = list(t = 50, b = 50, l= 50, r=20),
         xaxis = list(title = "VO₂<sub>SS</sub> [%]",
                      range = c(0, 100),
                      zeroline = TRUE,   # Nulllinie anzeigen
                      zerolinecolor = 'grey',
                      zerolinewidth = 1),
         yaxis = list(title = "Tau<sub>on</sub> [s]",
                      # Y-Achsenbereich ggf. anpassen, um fixed_intercept und Daten gut darzustellen
                      range = c(min(0, fixed_intercept - 5, na.rm=TRUE), max(80, fixed_intercept + 5, na.rm=TRUE)),
                      zeroline = TRUE,   # Nulllinie anzeigen
                      zerolinecolor = 'grey',
                      zerolinewidth = 1),
         legend = list(title = list(text='<b> Proband / Modell </b>')),
         hovermode = "closest") %>%
  # Annotationen anpassen (Positionen evtl. justieren)
  # Positionierung relativ zum Datenbereich
  add_annotations(text = equation_text_vo2_fixed, x = 5, y = max(Tau_df_filtered$tau_on, na.rm = TRUE) * 0.98,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12)) %>%
  add_annotations(text = r_squared_text_vo2_fixed, x = 5, y = max(Tau_df_filtered$tau_on, na.rm = TRUE) * 0.93,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12)) %>%
  add_annotations(text = slope_sig_text_fixed, x = 5, y = max(Tau_df_filtered$tau_on, na.rm = TRUE) * 0.88,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'top', font = list(size = 12))

# Plot anzeigen
p_tauon_VO2ss_fixed
 

##########B und TauB bestimmen############

# Schleife durch die Kombinationen in start_vals_list und VO2_list
for (i in 1:length(start_vals_list)) {
  tryCatch({
    # Aktuelle Startwerte und Daten verwenden
    start_vals <- start_vals_list[[i]]
    current_VO2_data <- VO2_list[[i]]
    
    # Bestimmen des letzten Datenpunkts
    last_data_point <- max(current_VO2_data$t_s)
  
  # Simulierte Ruhewerte von 601 bis 3599 generieren
  simulierte_ruhewerte_2 <- seq(601, 3599, by = 1)
  
  # Erstellen eines Dataframes mit den neuen t_s Werten und konstanten VO2_l_min Werten basierend auf C
  ruhe_sim2 <- data.frame(
    t_s = simulierte_ruhewerte_2,
    VO2_l_min = rep(NA, length(simulierte_ruhewerte_2))
  )
  
  # Hinzufügen der neuen Datenpunkte zu den aktuellen VO2 Daten
  VO2_data_neu <- rbind(current_VO2_data, ruhe_sim2)
  
  #### EVTL DEN ZEITPUNKT ABHÄNGIG VON DER INTENSITÄT MACHEN ANHAND DER NÄHE ZUR VO2max#####
  # Simulierte Ruhewerte von 3600 bis 4200 generieren
  simulierte_ruhewerte <- seq(3600, 4200, by = 1)
  
  # Erstellen eines Dataframes mit den neuen t_s Werten und konstanten VO2_l_min Werten basierend auf C
  ruhe_sim <- data.frame(
    t_s = simulierte_ruhewerte,
    VO2_l_min = rep(start_vals$C, length(simulierte_ruhewerte))
  )
  
  # Hinzufügen der neuen Datenpunkte zu den aktuellen VO2 Daten
  VO2_data_neu <- rbind(VO2_data_neu, ruhe_sim)
  
  # Aktualisierte Daten zurück in VO2_list einfügen
  VO2_list[[i]] <- VO2_data_neu
  current_VO2_data <- VO2_list[[i]]
  
  # Berechnung des 2x bis 8x Wertes von Tau in start_vals
  tau  <- start_vals_list[[i]]$Tau
  tau2 <- 2 * start_vals_list[[i]]$Tau
  tau3 <- 3 * start_vals_list[[i]]$Tau
  tau4 <- 4 * start_vals_list[[i]]$Tau
  tau6 <- 6 * start_vals_list[[i]]$Tau
  tau8 <- 8 * start_vals_list[[i]]$Tau
  tau_off <- start_vals_list[[i]]$Tau

  # Zählen der tatsächlichen Datenpunkte zwischen tau2 und last_data_point
  actual_data_count <- sum(current_VO2_data$t_s >= tau2 & current_VO2_data$t_s <= last_data_point)
  
  # Simulierte Ruhewerte von 3600 bis 4200 generieren
  ruhe_sim_start <- 3600
  ruhe_sim_end <- 4200
  simulierte_ruhewerte <- seq(ruhe_sim_start, ruhe_sim_end, length.out = actual_data_count)
  
  # Erstellen eines Dataframes mit den neuen t_s Werten und konstanten VO2_l_min Werten basierend auf C
  ruhe_sim <- data.frame(
    t_s = simulierte_ruhewerte,
    VO2_l_min = rep(start_vals$C, length(simulierte_ruhewerte))
  )
  
  # Hinzufügen der neuen Datenpunkte zu den aktuellen VO2 Daten
  VO2_data_neu <- rbind(current_VO2_data, ruhe_sim)
  
  # Aktualisierte Daten zurück in VO2_list einfügen
  VO2_list[[i]] <- VO2_data_neu
  current_VO2_data <- VO2_list[[i]]
  
  # Filtern der Daten für den spezifischen Bereich von tau2 bis last_data_point UND alle Daten ab t_s >= 3600
  filtered_data <- current_VO2_data[current_VO2_data$t_s >= tau2 & (current_VO2_data$t_s <= last_data_point | current_VO2_data$t_s >= ruhe_sim_start), ]
  
  # Berechnung des Startwerts für B
  max_B_start <- max(filtered_data$VO2_l_min[1:10], na.rm = TRUE)
  
  # Fitten des B * exp(-t_s/TauB) + C Terms mit dem Startwert für B und C
  model_B_tauB_C <- nlsLM(VO2_l_min ~ B * exp(-t_s/TauB) + C, 
                      data = filtered_data, 
                      start = list(B = max_B_start, TauB = 100, C = start_vals$C), 
                      lower = c(B = -Inf, TauB = 90, C = start_vals$C),  
                      upper = c(B = Inf, TauB = 6000, C = start_vals$C),  
                      control = nls.control(maxiter = 1024))
  
  # Extrahieren der geschätzten Werte für B, TauB und C
  estimates_B_tauB_C <- coef(model_B_tauB_C)
  
  # Zweiter Schritt: Gesamtmodell anpassen, mit B, TauB und C als festen Werten
  start_vals$B <- estimates_B_tauB_C['B']
  start_vals$TauB <- estimates_B_tauB_C['TauB']
  start_vals$C <- estimates_B_tauB_C['C']  
  
  # Berechnung Modelfunktion ohne C bei t=???
  coef_estimates['B'] * exp(-3600 / coef_estimates['TauB'])
  
  # Berechnung von R^2 unter Ignorierung von NA Werten
  SSR <- sum(resid(model_B_tauB_C)^2, na.rm = TRUE)
  SST <- sum((current_VO2_data$VO2_l_min - mean(current_VO2_data$VO2_l_min, na.rm = TRUE))^2, na.rm = TRUE)
  R_squared <- 1 - (SSR / SST)
  
  # Farben für "slow-Bereich" definieren
  current_VO2_data <- current_VO2_data %>%
    mutate(color = ifelse((t_s >= tau2 & t_s <= last_data_point) | (t_s >= 1800), '#BB7693', 'rgba(38, 131, 198, 0.9)'))
  
  # Berechnung der Modellfunktion und Hinzufügen als neue Spalte zu current_VO2_data
  current_VO2_data$model_slow_c <- estimates_B_tauB_C['B'] * exp(-current_VO2_data$t_s / estimates_B_tauB_C['TauB']) + estimates_B_tauB_C['C'] 
  
  # Farben für "slow-Modellfunktion" definieren
  current_VO2_data <- current_VO2_data %>%
    mutate(color_model = '#BB7693')
  
  # Gleichungstext zusammenstellen
  eq_text_slow <- sprintf("V̇O₂(t) = %.2f * e<sup>-(t / %.2f)</sup> + %.3f", 
                     estimates_B_tauB_C['B'], estimates_B_tauB_C['TauB'],estimates_B_tauB_C['C'])
  
  # Erstellen eines neuen Datenrahmens für den Bereich von 0 bis 4200 mit einem Schritt von 1
  t_s_complete <- seq(0, 4200, by = 1)
  model_slow_c_complete <- data.frame(
    t_s = t_s_complete,
    model_slow_c = coef_estimates['B'] * exp(-t_s_complete / coef_estimates['TauB']) + coef_estimates['C']
  )
  
  # Plot mit einheitlichen Linienfarben aber bedingten Markerfarben
  pSlow <- plot_ly(data = current_VO2_data, x = ~t_s, y = ~VO2_l_min, type = 'scatter', mode = 'markers+lines',
                   marker = list(color = ~color, size = 5), name = 'V&#775;O<sub>2</sub>',
                   line = list(color = "gray", width = 0.65, dash = '4 4')) %>% 
    add_trace(data = current_VO2_data, 
                     x = ~t_s, 
                     y = ~model_slow_c, 
                     type = 'scatter', 
                     mode = 'markers+lines',  
                     marker = list(color = ~color_model, size = 2),
                     name = 'EPOC<sub>slow</sub>',
                     line = list(color = ~color_model, width = 0.5)) %>%
    layout(title = sprintf("EPOC: %s - Slow-Term", names(start_vals_list)[i]),
           margin = list(t = 40),
           xaxis = list(title = "Zeit [s]", range = c(0, 4200), autorange = FALSE),
           yaxis = list(title = "V&#775;O<sub>2</sub> [l&#183;min<sup>-1</sup>]", range = c(0, 6.5)),
           shapes = list(
             list(
               type = "line", x0 = tau2, x1 = tau2, y0 = 0, y1 = 6.5,
               line = list(color = "gray", width = 1, dash = "dash")
             ),
             list(
               type = "line", x0 = last_data_point, x1 = last_data_point, y0 = 0, y1 = 6.5,
               line = list(color = "gray", width = 1, dash = "dash")
             )
           ),
           annotations = list(
             list(
               x = tau2, y = 5.2, text = sprintf("2τ: %.1f s", tau2), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
               textangle = -90, font = list(size = 11)
             ),
             list(
               x = last_data_point, y = 5.2, text = sprintf("t<sub>data_last</sub>: %.1f", last_data_point), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
               textangle = -90, font = list(size = 11)
             ),
             list(
               x = max(current_VO2_data$t_s) * 0.5, y = 5.6, text = paste("R²:", round(R_squared, 2)), showarrow = FALSE,
               xanchor = 'left', yanchor = 'bottom', font = list(family = "Arial, sans-serif", size = 11, color = "black")
             ),
             list(
               x = max(current_VO2_data$t_s) * 0.5, y = 5.9, text = eq_text_slow, showarrow = FALSE,
               xanchor = 'left', yanchor = 'bottom', font = list(family = "Arial, sans-serif", size = 11, color = "black")
             )
           )
    )
  
  # Plotly-Diagramm zur Liste hinzufügen
  plotly_list_slow[[i]] <- pSlow
  
##########A und TauA bestimmen############
  # Berechnung der oberen Grenze für A
  max_x_start <- max(current_VO2_data$VO2_l_min[1:10], na.rm = TRUE)
  upper_A <- max_x_start - estimates_B_tauB_C['C'] - estimates_B_tauB_C['B'] + 
    (0.05 * (max_x_start - estimates_B_tauB_C['C'] - estimates_B_tauB_C['B']))
  
  # Zweiter Schritt: Gesamtmodell anpassen, mit B, TauB und C als festen Werten
  model_equation_fixed <- VO2_l_min ~ A * exp(-t_s/TauA) + estimates_B_tauB_C['B'] * exp(-t_s/estimates_B_tauB_C['TauB']) + estimates_B_tauB_C['C']
  
  # Fitting des Gesamtmodells mit nlsLM, A und TauA sind die zu fittenden Parameter
  model <- nlsLM(model_equation_fixed, 
                 data = current_VO2_data, 
                 start = list(A = start_vals$A, TauA = start_vals$TauA),
                 lower = c(A = 0, TauA = 10),
                 upper = c(A = upper_A, TauA = 90), 
                 algorithm = "port",
                 control = list(maxiter = 1024))
  
  # Ergebnisse anzeigen
  model_summary <- summary(model)
  print(model_summary)
  
  # Modellwerte berechnen
  current_VO2_data$model <- predict(model, newdata = current_VO2_data)
  
  # Parameter aus dem Modell extrahieren und die festen Werte hinzufügen
  coef_estimates <- coef(model)
  coef_estimates['B'] <- start_vals$B
  coef_estimates['TauB'] <- start_vals$TauB
  coef_estimates['C'] <- start_vals$C  

  # Teilmodelle berechnen
  current_VO2_data$model_fast <- coef_estimates['A'] * exp(-current_VO2_data$t_s / coef_estimates['TauA'])
  current_VO2_data$model_slow <- coef_estimates['B'] * exp(-current_VO2_data$t_s / coef_estimates['TauB'])
  current_VO2_data$model_ruhe <- rep(coef_estimates['C'], nrow(current_VO2_data))
  
  # Integrieren von model_fast über die Zeit
  model_fast_func <- function(t_s) coef_estimates['A'] * exp(-t_s / coef_estimates['TauA'])
  integrated_model_fast <- integrate(model_fast_func, lower = min(current_VO2_data$t_s), upper = 4200)
  
  # WPCR und VO2_fast berechnen
  VO2_fast <- integrated_model_fast$value / 60
  WPCR <- VO2_fast * CE_max
  
  # Gleichungstext zusammenstellen
  eq_text <- sprintf("V̇O₂(t) = %.2f * e<sup>-(t / %.2f)</sup> + %.2f * e<sup>-(t / %.2f)</sup> + %.3f", 
                     coef_estimates['A'], coef_estimates['TauA'],
                     coef_estimates['B'], coef_estimates['TauB'],
                     coef_estimates['C'])
  
  # Ribbons erfordern einen Dataframe mit min und max Werten für die Füllung
  ribbon_df_VO2 <- data.frame(t_s = current_VO2_data$t_s, ymin = 0, ymax = current_VO2_data$model_fast)
  
  # Berechnung von R^2 nach Filtern der Daten, bei denen t_s <= 600
  current_VO2_data_R2 <- filter(current_VO2_data, t_s <= 600)
  SSR <- sum(resid(model)^2, na.rm = TRUE)
  SST <- sum((current_VO2_data_R2$VO2_l_min - mean(current_VO2_data_R2$VO2_l_min, na.rm = TRUE))^2, na.rm = TRUE)
  R_squared <- 1 - (SSR / SST)
  
  # Anpassung der Plot-Erstellung, um current_VO2_data zu verwenden
  p <- plot_ly() %>%
    add_trace(data = current_VO2_data, x = ~t_s, y = ~VO2_l_min, type = 'scatter', mode = 'markers+lines', 
              name = 'V&#775;O<sub>2</sub>', 
              marker = list(color = 'rgba(38, 131, 198, 0.9)', size = 5.0),
              line = list(color = 'rgba(38, 131, 198, 1.0)', width = 0.65, dash = '4 4')) %>%
    add_trace(data = current_VO2_data, x = ~t_s, y = ~model, type = 'scatter', mode = 'lines', 
              name = 'Modellfunktion', line = list(color = '#EF6F6A')) %>%
    add_trace(data = current_VO2_data, x = ~t_s, y = ~model_fast, type = 'scatter', mode = 'lines', 
              name = 'EPOC<sub>fast', line = list(color = '#42BA97')) %>%
    add_trace(data = current_VO2_data, x = ~t_s, y = ~model_slow, type = 'scatter', mode = 'lines', 
              name = 'EPOC<sub>slow', line = list(color = '#BB7693')) %>%
    add_trace(data = current_VO2_data, x = ~t_s, y = ~model_ruhe, type = 'scatter', mode = 'lines', 
              name = 'V&#775;O<sub>2,Referenz</sub>', line = list(color = '#1CADE4')) %>%
    add_ribbons(data = ribbon_df_VO2, x = ~t_s, ymin = ~ymin, ymax = ~ymax, 
                name = 'EPOC<sub>fast,Integriert', fillcolor = 'rgba(66,186,151,0.5)', 
                line = list(color = "rgba(0,0,0,0)"))  %>%
    layout(title = sprintf("EPOC: %s", names(start_vals_list)[i]),
           margin = list(t = 40),
           xaxis = list(title = "Zeit [s]", range = c(0, 600), autorange = FALSE),
           yaxis = list(title = "V&#775;O<sub>2</sub> [l&#183;min<sup>-1</sup>]", range = c(0, 6.5)),
           shapes = list(
             list(
               type = "line",
               x0 = tau2,
               x1 = tau2,
               y0 = 0,
               y1 = 6.5,
               line = list(color = "gray", width = 0.5, dash = "dash")
             )
           ),
           annotations = list(
             list(
               x = tau2, y = 5.35, text = sprintf("2τ: %.1f s", tau2), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
               textangle = -90, font = list(size = 11, color = "darkgrey")
             ),
             list(
               x = 600 * 0.5, 
               y = 5.6,  # Position für WPCR
               text = paste("W<sub>PCR</sub>:", round(WPCR, 2), "kJ"),
               showarrow = FALSE,
               xanchor = 'left',
               yanchor = 'bottom',
               font = list(
                 size = 12,
                 color = "black"
               )
             ),
             list(
               x = 600 * 0.5, 
               y = 5.3,  # Angepasste Position für R^2
               text = paste("R²:", round(R_squared, 2)),
               showarrow = FALSE,
               xanchor = 'left',
               yanchor = 'bottom',
               font = list(
                 size = 12,
                 color = "black"
               )
             ),
             list(
               x = 600 * 0.5, 
               y = 5.9,  # Angepasste Position für Gleichungstext
               text = eq_text,
               showarrow = FALSE,
               xanchor = 'left',
               yanchor = 'bottom',
               font = list(
                 size = 12,
                 color = "black"
               )
             )
           ))
 
  
  # Plotly-Diagramm zur Liste hinzufügen
  plotly_list[[i]] <- p
  
  # Diagramm als HTML-Datei speichern mit dem Namen aus names(start_vals_list)[i]
  #htmlwidgets::saveWidget(pSlow, file = paste0(plot_folder, names(start_vals_list)[i], "_slow.html"))
  #htmlwidgets::saveWidget(p, file = paste0(plot_folder, names(start_vals_list)[i], ".html"))

  #plotly::save_image(p,file = paste0(plot_folder, names(start_vals_list)[i], ".svg"),width = 1100, height = 450)
  #plotly::save_image(pSlow,file = paste0(plot_folder, names(start_vals_list)[i], "_slow.svg"),width = 1100, height = 450)
  
  
  # Aktuellen Probandennamen und Nr aus start_vals_list verwenden
  proband_name <- substr(names(start_vals_list)[i], 1, 2)  # Die ersten beiden Ziffern des Namens
  nr_name <- substr(names(start_vals_list)[i], 4, 4)
  
  # Ergebnisse in den DataFrame einfügen
  result_entry <- data.frame(
    Proband = proband_name,
    Nr = nr_name,
    WPCR = WPCR,
    VO2_fast = round(VO2_fast,3),
    R_squared_off = R_squared,
    tau_off = tau_off,
    A = coef_estimates['A'],
    TauA = coef_estimates['TauA'],
    B = coef_estimates['B'],
    TauB = coef_estimates['TauB'],
    C = coef_estimates['C']
  )
  # Setzen Sie den Namen der 'WPCR'-Spalte nachträglich auf 'WPCR [kJ]'
  names(result_entry)[names(result_entry) == "WPCR"] <- "WPCR [kJ]"
  names(result_entry)[names(result_entry) == "VO2_fast"] <- "VO2_fast [l]"
  
  EPOC_data_df <- rbind(EPOC_data_df, result_entry)
  
  cat(sprintf("Modellanpassung für i = %d (%s) war erfolgreich\n", i, names(start_vals_list)[i]))
}, error = function(e) {
  cat(sprintf("Fehler bei der Modellanpassung für i = %d (%s): %s\n", i, names(start_vals_list)[i], e$message))
}, warning = function(w) {
  cat(sprintf("Warnung bei der Modellanpassung für i = %d (%s): %s\n", i, names(start_vals_list)[i], w$message))
})
}
# Anzeigen der Plotly-Diagramme
#for (p in plotly_list) {
#print(p)
#}

plotly_list_slow[47:48]

#plotly_list[12:15]
plotly_list[54]
# Zu hohe PCr-Werte
plotly_list[23] # 13_5
plotly_list[47:48] # 22_5 und 22_6

# Speichern der slow-Plots als svg
#for (i in 1:length(plotly_list_slow)) {
  #name_part <- names(start_vals_list)[i]
  #file_name <- paste0(name_part, "_slow.svg")
  
  #plotly::save_image(
    #plotly_list_slow[[i]],
    #file = paste0(plot_folder, file_name),
    #width = 1100, 
    #height = 450
  #)
#}

# Speichern der slow-Plots als PNG mit optimierter Größe
#for (i in 1:length(plotly_list_slow)) {
  #name_part <- names(start_vals_list)[i]
  #file_name <- paste0(name_part, "_slow.png")
  
  #plotly::save_image(
    #plotly_list_slow[[i]],
    #file = paste0(plot_folder, file_name),
    #width = 1100,    
    #height = 450,   
    #scale = 2     
  #)
#}

# Speichern der Gesamtplots als svg
#for (i in 1:length(plotly_list)) {
  #name_part <- names(start_vals_list)[i]
  #file_name <- paste0(name_part, ".svg")
  
  #plotly::save_image(
    #plotly_list[[i]],
    #file = paste0(plot_folder, file_name),
    #width = 1100, 
    #height = 450
  #)
#}

# Speichern der Gesamtplots als PNG mit gleichen Einstellungen
#for (i in 1:length(plotly_list)) {
  #name_part <- names(start_vals_list)[i]
  #file_name <- paste0(name_part, ".png")  # Geändert zu .png
  
  #plotly::save_image(
    #plotly_list[[i]],
    #file = paste0(plot_folder, file_name),
    #width = 1100,    
    #height = 450,   
    #scale = 2     
  #)
#}

# Berechnung Modelfunktion ohne C
coef_estimates['A'] * exp(-3600 / coef_estimates['TauA']) + coef_estimates['B'] * exp(-3600 / coef_estimates['TauB'])
coef_estimates['C']

# Entferne die Zeilennummern in der linken Spalte
rownames(EPOC_data_df) <- NULL

# Umbenennen der Spalte "WPCR" in "WPCR [kJ]"
if (!"WPCR [kJ]" %in% colnames(EPOC_data_df) && "WPCR" %in% colnames(EPOC_data_df)) {
  colnames(EPOC_data_df)[colnames(EPOC_data_df) == "WPCR"] <- "WPCR [kJ]"
}
if (!"VO2_fast [l]" %in% colnames(EPOC_data_df) && "VO2_fast" %in% colnames(EPOC_data_df)) {
  colnames(EPOC_data_df)[colnames(EPOC_data_df) == "VO2_fast"] <- "VO2_fast [l]"
}
# Konvertiere die Spalte "Nr" in EPOC_data_df in einen numerischen Datentyp
EPOC_data_df$Nr <- as.numeric(EPOC_data_df$Nr)
EPOC_data_df$Proband <- as.numeric(EPOC_data_df$Proband)

# Mittelwert der Spalte "R_squared" berechnen, Zeilen mit Proband 2 oder 3 ausschließen
mean_value <- mean(subset(EPOC_data_df, Proband != 2 & Proband != 3)$R_squared)
print(mean_value)


#saveRDS(plotly_list, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/plotly_list.rds")
#saveRDS(plotly_list_slow, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/plotly_list_slow.rds")
#saveRDS(plotly_list_tau, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/plotly_list_tau.rds")

##################################################################

EPOC_data_df <- EPOC_data_df[!EPOC_data_df$Proband %in% c(2, 3), ]
EPOC_data_df$Proband <- sprintf("%02d", as.numeric(EPOC_data_df$Proband))

# Gemeinsamen Spalten (außer "Proband" und "Nr")
common_cols <- intersect(names(EPOC_data_df), names(Bedingungen_data))
common_cols <- common_cols[!common_cols %in% c("Proband", "Nr")]
# Entfernen der gemeinsamen Spalten aus Bedingungen_data
Bedingungen_data <- Bedingungen_data[, !names(Bedingungen_data) %in% common_cols]
EPOC_data_df <- merge(EPOC_data_df, Bedingungen_data, by = c("Proband", "Nr"), all.x = TRUE)

EPOC_data_df$`VO2_fast [ml/kg]` <- EPOC_data_df$`VO2_fast [l]` / EPOC_data_df$Masse * 1000
EPOC_data_df$P_Tot_kg <- EPOC_data_df$P_Tot / EPOC_data_df$Masse

### Verhältnis versctoffwechselter WPCR und PTOT ###
# Lineare Regression durchführen
model <- lm(`WPCR [kJ]` ~ P_Tot, data = EPOC_data_df)

# Regressionswerte für die Trendlinie berechnen
p_tot_seq <- seq(0, 500, length.out = 100)
regression_values <- predict(model, newdata = data.frame(P_Tot = p_tot_seq))

# Schnittpunkt der Regressionslinie mit der x-Achse berechnen
x_intercept <- -coef(model)[1] / coef(model)[2]

# Regressionsergebnisse extrahieren
equation <- format(coef(model), digits = 3)
equation_text <- paste("y =",equation[1],"+",equation[2],"x")
r_squared <- summary(model)$r.squared
r_squared_text <- paste("R² =", round(r_squared, 3))
f_stat <- summary(model)$fstatistic
f_stat_text <- paste("F-Statistik =", round(f_stat[1], 3))

# Plot erstellen
plot_WPCR_PTOT <- plot_ly() %>%
  add_markers(data = EPOC_data_df, x = ~P_Tot, y = ~`WPCR [kJ]`, type = 'scatter', mode = 'markers',
              color = ~factor(Proband), colors = colorRampPalette(brewer.pal(10, "Spectral"))(length(unique(EPOC_data_df$Proband))),
              marker = list(size = 9, opacity = 0.8)) %>%
  layout(title = "Zusammenhang zwischen W<sub>PCR</sub> [kJ] und P<sub>tot</sub> [Watt]",
         margin = list(t = 40),
         xaxis = list(title = "P<sub>tot</sub> [Watt]", range = c(0, max(460, x_intercept))),
         yaxis = list(title = "W<sub>PCR</sub> [kJ]", range = c(0, 60))) %>%
  add_lines(x = ~p_tot_seq[p_tot_seq >= x_intercept], y = ~regression_values[p_tot_seq >= x_intercept], 
            name = "Regressionslinie", line = list(color = 'darkgrey', width = 2)) %>%
  add_annotations(text = equation_text, x = 10, y = 58,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'bottom') %>%
  add_annotations(text = r_squared_text, x = 10, y = 56,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'bottom') %>%
  add_annotations(text = f_stat_text, x = 10, y = 54,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'bottom')

plot_WPCR_PTOT

#htmlwidgets::saveWidget(plot_WPCR_PTOT, file = paste0(plot_folder, names(start_vals_list)[i], "_plot_WPCR_PTOT.html"))


#######################################################
### Verhältnis VO2_fast [ml/kg] und P_Tot_kg ###
# Lineare Regression durchführen
model <- lm(`VO2_fast [ml/kg]` ~ P_Tot_kg, data = EPOC_data_df)

# Regressionswerte für die Trendlinie berechnen
p_tot_kg_seq <- seq(0, max(EPOC_data_df$P_Tot_kg), length.out = 100)
regression_values <- predict(model, newdata = data.frame(P_Tot_kg = p_tot_kg_seq))

# Schnittpunkt der Regressionslinie mit der x-Achse berechnen
x_intercept <- -coef(model)[1] / coef(model)[2]

# Regressionsergebnisse extrahieren
equation <- format(coef(model), digits = 3)
equation_text <- paste("y =", equation[1], "+", equation[2], "x")
r_squared <- summary(model)$r.squared
r_squared_text <- paste("R² =", round(r_squared, 3))
f_stat <- summary(model)$fstatistic
f_stat_text <- paste("F-Statistik =", round(f_stat[1], 3))

# Plot erstellen
plot <- plot_ly() %>%
  add_markers(data = EPOC_data_df, x = ~P_Tot_kg, y = ~`VO2_fast [ml/kg]`, type = 'scatter', mode = 'markers',
              color = ~factor(Proband), colors = colorRampPalette(brewer.pal(10, "Spectral"))(length(unique(EPOC_data_df$Proband))),
              marker = list(size = 9, opacity = 0.8)) %>%
  layout(title = "VO2_fast [ml/kg] vs. P_Tot_kg",
         margin = list(t = 40),
         xaxis = list(title = "P_Tot_kg", range = c(0, 7, x_intercept)),
         yaxis = list(title = "VO2_fast [ml/kg]", range = c(0, 32))) %>%
  add_lines(x = ~p_tot_kg_seq[p_tot_kg_seq >= x_intercept], y = ~regression_values[p_tot_kg_seq >= x_intercept], 
            name = "Regressionslinie", line = list(color = 'darkgrey', width = 2)) %>%
  add_annotations(text = equation_text, x = 0, y = max(EPOC_data_df$`VO2_fast [ml/kg]`) * 0.95,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'bottom') %>%
  add_annotations(text = r_squared_text, x = 0, y = max(EPOC_data_df$`VO2_fast [ml/kg]`) * 0.9,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'bottom') %>%
  add_annotations(text = f_stat_text, x = 0, y = max(EPOC_data_df$`VO2_fast [ml/kg]`) * 0.85,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'bottom')

plot


########################################################
### Berechnung nach Francescato2003 -> Immer nomiert auf die aktive Muskelmasse ###

# Zeitkonstante der VO2-Anpassung auf alveolärer Ebene in s
tau_p <- 34.3 / 60 # in Minuten

# Aktive Muskelmasse
EPOC_data_df <- EPOC_data_df %>%
  mutate(aktive_Muskelmasse = case_when(
    Proband %in% c(22, 23) & Bedingung == "sitzen" ~ Masse * 0.34 * 0.65,
    Proband %in% c(22, 23) & Bedingung == "stehen" ~ Masse * 0.34 * 0.75,
    Bedingung == "sitzen" ~ Masse * 0.42 * 0.65,
    Bedingung == "stehen" ~ Masse * 0.42 * 0.75
  ))
EPOC_data_df$aktive_Muskelmasse_prozent <- EPOC_data_df$aktive_Muskelmasse / EPOC_data_df$Masse
EPOC_data_df$Watt_Muskelmasse_aktiv <- EPOC_data_df$P_mech / EPOC_data_df$aktive_Muskelmasse

# Mean und SD der Muskelmasse
filtered_data <- EPOC_data_df[EPOC_data_df$Proband %in% c(22, 23), ]
mean_value <- mean(filtered_data$aktive_Muskelmasse)
sd_value <- sd(filtered_data$aktive_Muskelmasse)

# VO2_SS_gross und VO2_SS_gross definieren
EPOC_data_df$VO2_SS_net_liter <- Bedingungen_data$VO2_net_SS
EPOC_data_df$VO2_SS_gross_liter <- Energieanteile_data$VO2_SS_gross_liter
EPOC_data_df$VO2_Ruhe <- EPOC_data_df$`V'O2_Ruhe_ber`

# Auf aktive Muskelmasse normieren
EPOC_data_df$VO2_SS_gross_liter_MM_aktiv <- Energieanteile_data$VO2_SS_gross_liter * EPOC_data_df$aktive_Muskelmasse_prozent
EPOC_data_df$VO2_SS_net_liter_MM_aktiv  <- Bedingungen_data$VO2_net_SS * EPOC_data_df$aktive_Muskelmasse_prozent
EPOC_data_df$VO2_Ruhe_MM_aktiv  <- EPOC_data_df$`V'O2_Ruhe_ber` * EPOC_data_df$aktive_Muskelmasse_prozent

# Venöses Blutvolumen -> 64% des gesamten Blutvolumens, welches wiederum als 7% der Körpermasse ist
EPOC_data_df$VBV <- 0.64 * 0.07 * EPOC_data_df$Mass 

# Cardiac Output nach Astrand 1964 
EPOC_data_df$Q <- ifelse(
  (EPOC_data_df$Proband == 22 | EPOC_data_df$Proband == 23) & EPOC_data_df$VO2_percent > 70.0,
  9.88 + 3.23 * EPOC_data_df$VO2_SS_gross_liter,
  ifelse(
    (EPOC_data_df$Proband == 22 | EPOC_data_df$Proband == 23) & EPOC_data_df$VO2_percent <= 70.0,
    3.66 + 6.81 * EPOC_data_df$VO2_SS_gross_liter,
    ifelse(
      !(EPOC_data_df$Proband == 22 | EPOC_data_df$Proband == 23) & EPOC_data_df$VO2_percent > 70.0,
      6.55 + 4.35 * EPOC_data_df$VO2_SS_gross_liter,
      ifelse(
        !(EPOC_data_df$Proband == 22 | EPOC_data_df$Proband == 23) & EPOC_data_df$VO2_percent <= 70.0,
        3.07 + 6.01 * EPOC_data_df$VO2_SS_gross_liter,
        NA
      )
    )
  )
)
EPOC_data_df$Q_Ruhe <- ifelse(
  EPOC_data_df$Proband == 22 | EPOC_data_df$Proband == 23,
  3.66 + 6.81 * EPOC_data_df$VO2_Ruhe,
  3.07 + 6.01 * EPOC_data_df$VO2_Ruhe
)


## Pulmonaler Sauerstoffspeicher  -> ΔO2Sp
#Eigentlich mit VO2_SS_Amplitude_liter_MM_aktiv statt VO2_SS_net_liter_MM_aktiv
#EPOC_data_df$VO2_SS_Amplitude_liter_MM_aktiv  <- EPOC_data_df$VO2_SS_gross_liter_MM_aktiv - (EPOC_data_df$VO2_Referenz * EPOC_data_df$aktive_Muskelmasse_prozent)

EPOC_data_df$tau_on_min <-  EPOC_data_df$tau_on / 60

EPOC_data_df$O2Sp <- EPOC_data_df$VO2_SS_net_liter * pmax(EPOC_data_df$tau_on_min - tau_p, 0)
EPOC_data_df$O2Sp <- EPOC_data_df$VO2_SS_net_liter_MM_aktiv * pmax(EPOC_data_df$tau_on_min - tau_p, 0) # Normiert auf aktive Muskelmasse
EPOC_data_df$O2Sp

EPOC_data_df$O2Sp <- EPOC_data_df$VO2_SS_gross_liter * pmax(EPOC_data_df$tau_on_min - tau_p, 0)
EPOC_data_df$O2Sp <- EPOC_data_df$VO2_SS_net_liter_MM_aktiv * pmax(EPOC_data_df$tau_on_min - tau_p, 0) # Normiert auf aktive Muskelmasse
EPOC_data_df$O2Sp

#VO2_on_list <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/VO2_on_list.rds")
#print(VO2_on_list[[5]])

#EPOC_data_df$DO2Sp <- ifelse((Bedingungen_data$tau_on - tau_p) < 0, 0, (Bedingungen_data$delta_VO2_SS) * (Bedingungen_data$tau_on - tau_p))
#EPOC_data_df$DO2Sp

## Venöser Sauerstoffspeicher -> ΔO2Sv
EPOC_data_df$O2Sv <- ((EPOC_data_df$VO2_SS_gross_liter/EPOC_data_df$Q)-(EPOC_data_df$VO2_Ruhe/EPOC_data_df$Q_Ruhe)) * EPOC_data_df$VBV 
EPOC_data_df$O2Sv <- ((EPOC_data_df$VO2_SS_gross_liter_MM_aktiv/EPOC_data_df$Q)-(EPOC_data_df$VO2_Ruhe_MM_aktiv/EPOC_data_df$Q_Ruhe)) * EPOC_data_df$VBV # Normiert auf MM aktiv
EPOC_data_df$O2Sv

## Sauerstoffspeicher im Myoglobin -> ΔO2Sm --> WIRD NICHT BERECHNET

## Gesamter Sauerstoffpeicher
EPOC_data_df$O2_Speicher <- EPOC_data_df$O2Sp + EPOC_data_df$O2Sv #+ EPOC_data_df$O2Sm
EPOC_data_df$O2_Speicher
mean(EPOC_data_df$O2_Speicher)

# Berechnete Sauerstoffspeicher in WPCR_corrected einbeziehen

EPOC_data_df$VO2_fast_l_corrected <- EPOC_data_df$`VO2_fast [l]` - EPOC_data_df$O2_Speicher

EPOC_data_df$WPCR_corrected_calc <- ifelse(EPOC_data_df$VO2_fast_l < EPOC_data_df$O2_Speicher, 0, (EPOC_data_df$VO2_fast_l_corrected * CE_max))
EPOC_data_df$WPCR_corrected_calc

EPOC_data_df$VO2_fast_l_corrected <- NULL

WPCR_df <- EPOC_data_df[, c("Proband", "Nr", "WPCR_corrected_calc")]


##########################################################################
### PCR und Normwerte berechnen ###

# Proband als Character in beiden Dataframes definieren
EPOC_data_df$Proband <- as.character(EPOC_data_df$Proband)
Bedingungen_data$Proband <- as.character(Bedingungen_data$Proband)
# Formatiere Proband mit führender Null in beiden Dataframes
Bedingungen_data$Proband <- sprintf("%02d", as.numeric(Bedingungen_data$Proband))
EPOC_data_df$Proband <- sprintf("%02d", as.numeric(EPOC_data_df$Proband))
# Spalte RQ_SS_avg hinzufügen oder aktualisieren
EPOC_data_df <- EPOC_data_df %>%
  select(-any_of("RQ_SS_avg")) %>%  # Entferne existierende Spalte falls vorhanden
  left_join(select(Bedingungen_data, Proband, Nr, RQ_SS_avg), 
            by = c("Proband", "Nr"))


# Konstante für die Umrechnung in kJ
CE_max <- 21.1307796

# Berechnung der P/O-Ratio anhand der Formel mit RQ_SS_avg
EPOC_data_df$P_O_Ratio <- (4.61 * (1.00 - EPOC_data_df$RQ_SS_avg) + 5.0 * (EPOC_data_df$RQ_SS_avg - 0.70)) / 0.30
EPOC_data_df$P_O_Ratio

# Funktion zur Berechnung der Sauerstoffmenge und Energie für PCr-Resynthese
berechne_VO2PCr <- function(aktive_muskelmasse, pcr_konzentration) {
  VO2PCr <- (aktive_muskelmasse * pcr_konzentration * 22.4) / (EPOC_data_df$P_O_Ratio * 1000)
  WPCr <- VO2PCr * CE_max
  return(list(VO2PCr = VO2PCr, WPCr = WPCr))
}

# Berechnung der VO2PCr- und WPCr-Werte für Norm, Min und Max
# PCr-Konzentrationen in mmol/kg
min_values <- berechne_VO2PCr(EPOC_data_df$aktive_Muskelmasse, 15.7)  # 15.7 mmol/kg
norm_values <- berechne_VO2PCr(EPOC_data_df$aktive_Muskelmasse, 20.34)  # 20.2 mmol/kg
max_values <- berechne_VO2PCr(EPOC_data_df$aktive_Muskelmasse, 35)  # 35 mmol/kg

# Hinzufügen der VO2PCr-Werte
EPOC_data_df$VO2PCr_norm <- norm_values$VO2PCr
EPOC_data_df$VO2PCr_min <- min_values$VO2PCr
EPOC_data_df$VO2PCr_max <- max_values$VO2PCr
EPOC_data_df$VO2PCr_norm

# Hinzufügen der WPCr-Werte
EPOC_data_df$WPCr_norm <- norm_values$WPCr
EPOC_data_df$WPCr_min <- min_values$WPCr
EPOC_data_df$WPCr_max <- max_values$WPCr
EPOC_data_df$WPCr_norm

# Berechnen der PCr-Werte
EPOC_data_df$PCr_min <- 15.7
EPOC_data_df$PCr_norm <- 20.2
EPOC_data_df$PCr_max <- 37.7

# VO2_fast_l_corrected berechnen
EPOC_data_df$VO2_fast_l_corrected <- EPOC_data_df$`VO2_fast [l]` - EPOC_data_df$O2_Speicher

# PCr-Konzentration berechnen
EPOC_data_df$PCr_used <- (EPOC_data_df$VO2_fast_l_corrected * EPOC_data_df$P_O_Ratio * 1000) / 
  (EPOC_data_df$aktive_Muskelmasse * 22.4)

EPOC_data_df$PCr_used


### Plot für WPCr ###
# Sortieren des Dataframes basierend auf WPCr_max
EPOC_data_df <- EPOC_data_df %>%
  arrange(WPCr_max)

# Vorbereiten der Daten für das Diagramm
plot_data_WPCr <- EPOC_data_df %>%
  select(Proband, Nr, WPCR_corrected_calc, WPCr_norm, WPCr_min, WPCr_max) %>%
  mutate(Index = row_number()) %>%
  pivot_longer(cols = c(WPCR_corrected_calc, WPCr_norm, WPCr_min, WPCr_max),
               names_to = "Variable", values_to = "WPCr")

# Definieren der Farbpalette
color_palette <- c("WPCR_corrected_calc" = "#9C85C0", 
                   "WPCr_norm" = "#1CADE4", 
                   "WPCr_min" = "#42BA97", 
                   "WPCr_max" = "#EF5350")

# Erstellen des Plotly-Diagramms für WPCr
p_WPCr <- plot_ly()

# Hinzufügen der Linien und Punkte für alle Variablen
for (var in names(color_palette)) {
  data_subset <- subset(plot_data_WPCr, Variable == var)
  
  if (var == "WPCR_corrected_calc") {
    mode <- 'markers'
    symbol <- 'circle'
    size <- 8
    line_style <- NULL
  } else {
    mode <- 'lines+markers'
    line_style <- list(color = color_palette[var], width = 1)
    symbol <- 'circle'
    size <- 4
  }
  
  p_WPCr <- p_WPCr %>% add_trace(
    data = data_subset,
    x = ~Index,
    y = ~WPCr,
    type = 'scatter',
    mode = mode,
    line = line_style,
    marker = list(color = color_palette[var], symbol = symbol, size = size),
    name = var,
    hoverinfo = 'text',
    text = ~paste(round(WPCr, 2), "kJ<br>Proband: ", Proband, "_", Nr)
  )
}

# Hinzufügen der grauen Fläche zwischen WPCr_max und WPCr_min
p_WPCr <- p_WPCr %>%
  add_trace(data = EPOC_data_df, x = ~seq_along(WPCr_max), y = ~WPCr_max, 
            type = 'scatter', mode = 'lines', line = list(color = 'rgba(0,0,0,0)'),
            showlegend = FALSE, name = 'Max Area', hoverinfo = 'none') %>%
  add_trace(data = EPOC_data_df, x = ~seq_along(WPCr_min), y = ~WPCr_min, 
            type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = 'rgba(128,128,128,0.1)',
            line = list(color = 'rgba(0,0,0,0)'), showlegend = FALSE, name = 'Min Area', hoverinfo = 'none')

p_WPCr <- p_WPCr %>% layout(
  title = "Vergleich der anhand der EPOC berechneten WPCr-Werte mit den Normwerten",
  margin = list(l = 60, r = 40, t = 40, b = 60),
  xaxis = list(title = "Durchgänge", range = c(0, max(plot_data_WPCr$Index) + 1)),
  yaxis = list(title = "WPCr [kJ]"),
  hovermode = "closest",
  hoverlabel = list(namelength = -1)
)

### Plot für VO2 ###
# Sortieren des Dataframes basierend auf VO2PCr_max
EPOC_data_df <- EPOC_data_df %>%
  arrange(VO2PCr_max)

# Vorbereiten der Daten für das Diagramm
EPOC_data_df$VO2_fast_l_corrected <- EPOC_data_df$`VO2_fast [l]` - EPOC_data_df$O2_Speicher
plot_data_VO2 <- EPOC_data_df %>%
  select(Proband, Nr, VO2_fast_l_corrected, VO2PCr_norm, VO2PCr_min, VO2PCr_max) %>%
  mutate(Index = row_number()) %>%
  pivot_longer(cols = c(VO2_fast_l_corrected, VO2PCr_norm, VO2PCr_min, VO2PCr_max),
               names_to = "Variable", values_to = "VO2")

# Definieren der Farbpalette für VO2
color_palette_VO2 <- c("VO2_fast_l_corrected" = "#9C85C0", 
                       "VO2PCr_norm" = "#1CADE4", 
                       "VO2PCr_min" = "#42BA97", 
                       "VO2PCr_max" = "#EF5350")

# Erstellen des Plotly-Diagramms für VO2
p_VO2 <- plot_ly()

# Hinzufügen der Linien und Punkte für alle Variablen
for (var in names(color_palette_VO2)) {
  data_subset <- subset(plot_data_VO2, Variable == var)
  
  if (var == "VO2_fast_l_corrected") {
    mode <- 'markers'
    symbol <- 'circle'
    size <- 8
    line_style <- NULL
  } else {
    mode <- 'lines+markers'
    line_style <- list(color = color_palette_VO2[var], width = 1)
    symbol <- 'circle'
    size <- 4
  }
  
  p_VO2 <- p_VO2 %>% add_trace(
    data = data_subset,
    x = ~Index,
    y = ~VO2,
    type = 'scatter',
    mode = mode,
    line = line_style,
    marker = list(color = color_palette_VO2[var], symbol = symbol, size = size),
    name = var,
    hoverinfo = 'text',
    text = ~paste(round(VO2, 2), "l<br>Proband: ", Proband, "_", Nr)
  )
}

# Hinzufügen der grauen Fläche zwischen VO2PCr_max und VO2PCr_min
p_VO2 <- p_VO2 %>%
  add_trace(data = EPOC_data_df, x = ~seq_along(VO2PCr_max), y = ~VO2PCr_max, 
            type = 'scatter', mode = 'lines', line = list(color = 'rgba(0,0,0,0)'),
            showlegend = FALSE, name = 'Max Area', hoverinfo = 'none') %>%
  add_trace(data = EPOC_data_df, x = ~seq_along(VO2PCr_min), y = ~VO2PCr_min, 
            type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = 'rgba(128,128,128,0.1)',
            line = list(color = 'rgba(0,0,0,0)'), showlegend = FALSE, name = 'Min Area', hoverinfo = 'none')

p_VO2 <- p_VO2 %>% layout(
  title = "Vergleich der anhand der EPOC berechneten VO<sub>2,fast</sub>-Werte mit den Normwerten",
  margin = list(l = 60, r = 40, t = 40, b = 60),
  xaxis = list(title = "Durchgänge", range = c(0, max(plot_data_VO2$Index) + 1)),
  yaxis = list(title = "VO<sub>2,fast</sub> [l]"),
  hovermode = "closest",
  hoverlabel = list(namelength = -1)
)

### Plot für PCr_used ###
# Sortieren des Dataframes basierend auf PCr_used
EPOC_data_df <- EPOC_data_df %>%
  arrange(PCr_used)

# Vorbereiten der Daten für das Diagramm
plot_data_PCr <- EPOC_data_df %>%
  select(Proband, Nr, PCr_used, PCr_min, PCr_norm, PCr_max) %>%
  mutate(Index = row_number()) %>%
  pivot_longer(cols = c(PCr_used, PCr_min, PCr_norm, PCr_max),
               names_to = "Variable", values_to = "PCr")

# Definieren der Farbpalette für PCr
color_palette_PCr <- c("PCr_used" = "#9C85C0", 
                       "PCr_norm" = "#1CADE4",
                       "PCr_min" = "#42BA97", 
                       "PCr_max" = "#EF5350")

# Erstellen des Plotly-Diagramms für PCr
p_PCr <- plot_ly()

# Hinzufügen der PCr_used Datenpunkte
p_PCr <- p_PCr %>% add_trace(
  data = subset(plot_data_PCr, Variable == "PCr_used"),
  x = ~Index,
  y = ~PCr,
  type = 'scatter',
  mode = 'markers',
  marker = list(color = color_palette_PCr["PCr_used"], symbol = 'circle', size = 8),
  name = "PCr_used",
  hoverinfo = 'text',
  text = ~paste(round(PCr, 4), "mmol/kg<br>Proband: ", Proband, "_", Nr)
)

# Hinzufügen der Linien für PCr_norm, PCr_min und PCr_max
for (var in c("PCr_norm", "PCr_min", "PCr_max")) {
  p_PCr <- p_PCr %>% add_trace(
    data = subset(plot_data_PCr, Variable == var),
    x = ~Index,
    y = ~PCr,
    type = 'scatter',
    mode = 'lines',
    line = list(color = color_palette_PCr[var], width = 1),
    name = var,
    hoverinfo = 'text',
    text = ~paste(round(PCr, 4), "mmol/kg<br>Proband: ", Proband, "_", Nr)
  )
}

# Hinzufügen der grauen Fläche zwischen PCr_max und PCr_min
p_PCr <- p_PCr %>%
  add_trace(data = EPOC_data_df, x = ~seq_along(PCr_used), y = ~PCr_max, 
            type = 'scatter', mode = 'lines', line = list(color = 'rgba(0,0,0,0)'),
            showlegend = FALSE, name = 'Max Area', hoverinfo = 'none') %>%
  add_trace(data = EPOC_data_df, x = ~seq_along(PCr_used), y = ~PCr_min, 
            type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = 'rgba(128,128,128,0.1)',
            line = list(color = 'rgba(0,0,0,0)'), showlegend = FALSE, name = 'Min Area', hoverinfo = 'none')

p_PCr <- p_PCr %>% layout(
  title = "Ermittelte PCr-Mengen in Relation zu literaturbasierten PCr-Referenzwerten (Min., Max., MW)",
  margin = list(l = 60, r = 40, t = 40, b = 60),
  xaxis = list(title = "Durchgang", range = c(0, max(plot_data_PCr$Index) + 1)),
  yaxis = list(title = "PCr [mmol·kg⁻¹]"),
  hovermode = "closest",
  hoverlabel = list(namelength = -1)
)


# Anzeigen der Diagramme
#p_WPCr
p_VO2
p_PCr


plot_ergebnis_folder <- "C:\\Users\\johan\\OneDrive\\Desktop\\SpoWi\\WS 22,23\\Masterarbeit - Wirkungsgrad\\Wirkungsgrad-Rad.github.io\\Ergebnisse\\images\\"

#htmlwidgets::saveWidget(p_WPCr, file = paste0(plot_ergebnis_folder, "p_WPCr.html"))
#htmlwidgets::saveWidget(p_VO2, file = paste0(plot_ergebnis_folder, "p_VO2.html"))
#htmlwidgets::saveWidget(p_PCr, file = paste0(plot_ergebnis_folder, "p_PCr.html"))


### EPOC_data_df ergänzen ###

# Definiere die hinzuzufügende Spalte
columns_to_add <- c("PCr_used")
# Füge neue Spalten hinzu oder aktualisiere bestehende
EPOC_data_df <- EPOC_data_df %>%
  select(-any_of(columns_to_add)) %>%  # Entferne existierende Spalten falls vorhanden
  left_join(select(EPOC_data_df, Proband, Nr, all_of(columns_to_add)), 
            by = c("Proband", "Nr"))

#######################################################
### Verhältnis PCR_used und P_Tot_kg_active ###
EPOC_data_df$P_Tot_kg_active <- EPOC_data_df$P_Tot / EPOC_data_df$aktive_Muskelmasse

# Lineare Regression durchführen
model <- lm(EPOC_data_df$PCr_used ~ P_Tot_kg_active, data = EPOC_data_df)

# Regressionswerte für die Trendlinie berechnen
P_Tot_kg_active_seq<- seq(0, max(EPOC_data_df$P_Tot_kg_active ), length.out = 100)
regression_values <- predict(model, newdata = data.frame(P_Tot_kg_active  = P_Tot_kg_active_seq))

# Schnittpunkt der Regressionslinie mit der x-Achse berechnen
x_intercept <- -coef(model)[1] / coef(model)[2]

# Regressionsergebnisse extrahieren
equation <- format(coef(model), digits = 3)
equation_text <- paste("y =", equation[1], "+", equation[2], "x")
r_squared <- summary(model)$r.squared
r_squared_text <- paste("R² =", round(r_squared, 3))
f_stat <- summary(model)$fstatistic
f_stat_text <- paste("F-Statistik =", round(f_stat[1], 3))

# Plot erstellen
plot_PCR_P_Tot <- plot_ly() %>%
  add_markers(data = EPOC_data_df, x = ~P_Tot_kg_active , y = ~EPOC_data_df$PCr_used, type = 'scatter', mode = 'markers',
              color = ~factor(Proband), colors = colorRampPalette(brewer.pal(10, "Spectral"))(length(unique(EPOC_data_df$Proband))),
              marker = list(size = 9, opacity = 0.8)) %>%
  layout(title = "PCR_used  vs. P_Tot_kg_active ",
         margin = list(t = 40),
         xaxis = list(title = "P_Tot_kg_active ", range = c(0, 500, x_intercept)),
         yaxis = list(title = "PCR_used", range = c(0, max(EPOC_data_df$PCr_used)*1.05))) %>%
  add_lines(x = ~P_Tot_kg_active_seq[P_Tot_kg_active_seq>= x_intercept], y = ~regression_values[P_Tot_kg_active_seq>= x_intercept], 
            name = "Regressionslinie", line = list(color = 'darkgrey', width = 2)) %>%
  add_annotations(text = equation_text, x = 1, y = max(EPOC_data_df$PCr_used) * 0.95,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'bottom') %>%
  add_annotations(text = r_squared_text, x = 1, y = max(EPOC_data_df$PCr_used) * 0.9,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'bottom') %>%
  add_annotations(text = f_stat_text, x = 1, y = max(EPOC_data_df$PCr_used) * 0.85,
                  showarrow = FALSE, xanchor = 'left', yanchor = 'bottom')

plot_PCR_P_Tot


############################################################
# Halbwetszeit t1/2 aus tau_off berechnen
EPOC_data_df$t_halb <- EPOC_data_df$tau_off * log(2)
EPOC_data_df$`VO2_fast [ml·kg⁻¹]` <-round(EPOC_data_df$`VO2_fast [l]`/ EPOC_data_df$Masse * 1000 , 3)

EPOC_data_df <- rename(EPOC_data_df, "VO2_PCr [l]" = VO2_fast_l_corrected)
EPOC_data_df$`VO2_PCr [ml·kg⁻¹]` <-round( EPOC_data_df$`VO2_PCr [l]` / EPOC_data_df$Masse * 1000 , 3)

# Liste der beizubehaltenden Spalten
EPOC_Spalten_behalten <- c("Proband", "Nr", "Bedingung", "Intensität", "P_Tot", "P_Tot_kg", "VO2_fast [l]", "VO2_fast [ml·kg⁻¹]","VO2_PCr [l]", "VO2_PCr [ml·kg⁻¹]",
                           "A", "TauA", "B", "TauB","R_squared_off", "Masse", "aktive_Muskelmasse", "VO2_Referenz", 
                           "VO2_Ruhe", "VO2_gross_SS", "VO2_net_SS", "O2_Speicher", "WPCR [kJ]", 
                           "WPCR_corrected_calc", "PCr_used")
# Neuen Dataframe erstellen mit nur den spezifizierten Spalten
EPOC_data_df_short <- EPOC_data_df[, EPOC_Spalten_behalten]



# Speichern des DataFrames als RDS-Datei
#saveRDS(EPOC_data_df, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/EPOC_data_df.rds")
#saveRDS(EPOC_data_df_short, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/EPOC_data_df_short.rds")

