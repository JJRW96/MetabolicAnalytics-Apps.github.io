library(dplyr)
library(tibble) 

# Definieren Sie die Dateipfade
pfad_female <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/df_anthropometrisch_female.rds"
pfad_male <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/df_anthropometrisch_male.rds"

df_female <- readRDS(pfad_female)
df_male <- readRDS(pfad_male)
df_female <- as_tibble(df_female)
df_male <- as_tibble(df_male)

# 1. Aufteilen der Data Frames

# Weibliche Daten aufteilen
df_female_laenge <- df_female %>%
  select(Koerperlaenge, lBein, lOS, lUS)

df_female_masse <- df_female %>%
  select(Masse, uOS, uUS)

# Männliche Daten aufteilen
df_male_laenge <- df_male %>%
  select(Koerperlaenge, lBein, lOS, lUS)

df_male_masse <- df_male %>%
  select(Masse, uOS, uUS)

# 2. Extrapolation der Daten
extrapolate_laenge <- function(df_laenge) {
  # Lineare Modelle für jede abhängige Variable erstellen
  # Wichtig: lm() benötigt mindestens 2 Datenpunkte. Stellen Sie sicher, dass Ihre Original-dfs genug Daten haben.
  if (nrow(df_laenge) < 2) {
    stop("Nicht genügend Datenpunkte (weniger als 2) für die lineare Regression im Längen-DataFrame.")
  }
  model_lBein <- lm(lBein ~ Koerperlaenge, data = df_laenge)
  model_lOS <- lm(lOS ~ Koerperlaenge, data = df_laenge)
  model_lUS <- lm(lUS ~ Koerperlaenge, data = df_laenge)
  
  # Neuen Bereich für Koerperlaenge definieren (1.400 bis 2.200 in 0.100er Schritten)
  # seq(1.4, 2.2, by = 0.1) erzeugt die Sequenz 1.4, 1.5, ..., 2.2
  new_koerperlaenge <- seq(1.40, 2.20, by = 0.01)
  
  # Erstellen eines neuen Data Frames für die Vorhersage
  new_data <- data.frame(Koerperlaenge = new_koerperlaenge)
  
  # Vorhersagen für die neuen Koerperlaengen treffen
  predicted_lBein <- predict(model_lBein, newdata = new_data)
  predicted_lOS <- predict(model_lOS, newdata = new_data)
  predicted_lUS <- predict(model_lUS, newdata = new_data)
  
  # Erstellen des extrapolierten Data Frames
  df_extrapoliert <- tibble(
    Koerperlaenge = new_koerperlaenge,
    lBein = predicted_lBein,
    lOS = predicted_lOS,
    lUS = predicted_lUS
  )
  
  return(df_extrapoliert)
}

extrapolate_masse <- function(df_masse) {
  # Lineare Modelle für jede abhängige Variable erstellen
  if (nrow(df_masse) < 2) {
    stop("Nicht genügend Datenpunkte (weniger als 2) für die lineare Regression im Massen-DataFrame.")
  }
  model_uOS <- lm(uOS ~ Masse, data = df_masse)
  model_uUS <- lm(uUS ~ Masse, data = df_masse)
  
  # Neuen Bereich für Masse definieren (40.0 bis 120.0 in 1er Schritten)
  new_masse <- seq(40, 120, by = 1)
  
  # Erstellen eines neuen Data Frames für die Vorhersage
  new_data <- data.frame(Masse = new_masse)
  
  # Vorhersagen für die neuen Massen treffen
  predicted_uOS <- predict(model_uOS, newdata = new_data)
  predicted_uUS <- predict(model_uUS, newdata = new_data)
  
  # Erstellen des extrapolierten Data Frames
  df_extrapoliert <- tibble(
    Masse = new_masse,
    uOS = predicted_uOS,
    uUS = predicted_uUS
  )
  
  return(df_extrapoliert)
}

# Extrapolation durchführen

# Weibliche Daten extrapolieren
df_female_laenge_extrapoliert <- extrapolate_laenge(df_female_laenge)
df_female_laenge_extrapoliert[, 1] <- round(df_female_laenge_extrapoliert[, 1], 2); df_female_laenge_extrapoliert[, 2:4] <- round(df_female_laenge_extrapoliert[, 2:4], 3)
print(df_female_laenge_extrapoliert, n = nrow(df_female_laenge_extrapoliert))

df_female_masse_extrapoliert <- extrapolate_masse(df_female_masse)
df_female_masse_extrapoliert[, 1] <- round(df_female_masse_extrapoliert[, 1], 0); df_female_masse_extrapoliert[, 2:3] <- round(df_female_masse_extrapoliert[, 2:3], 3)
print(df_female_masse_extrapoliert, n = nrow(df_female_masse_extrapoliert))

# Männliche Daten extrapolieren
df_male_laenge_extrapoliert <- extrapolate_laenge(df_male_laenge)
df_male_laenge_extrapoliert[, 1] <- round(df_male_laenge_extrapoliert[, 1], 2); df_male_laenge_extrapoliert[, 2:4] <- round(df_male_laenge_extrapoliert[, 2:4], 3)
print(df_male_laenge_extrapoliert, n = nrow(df_male_laenge_extrapoliert))

df_male_masse_extrapoliert <- extrapolate_masse(df_male_masse)
df_male_masse_extrapoliert[, 1] <- round(df_male_masse_extrapoliert[, 1], 0); df_male_masse_extrapoliert[, 2:3] <- round(df_male_masse_extrapoliert[, 2:3], 3)
print(df_male_masse_extrapoliert, n = nrow(df_male_masse_extrapoliert))



