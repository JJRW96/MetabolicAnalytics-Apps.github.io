# Required packages
library(readxl)
library(dplyr)
library(plotly)
library(zoo)  # For rolling averages

# Function to detect and clean outliers with adjustable parameters
clean_outliers <- function(data, column, window_size = 5, threshold = 2) {
  # Create a copy of the data
  cleaned_data <- data
  
  # Calculate rolling mean
  rolling_mean <- rollmean(data[[column]], window_size, fill = NA, align = "center")
  
  # Manually calculate rolling standard deviation
  n <- length(data[[column]])
  rolling_sd <- rep(NA, n)
  half_window <- floor(window_size/2)
  
  for (i in 1:n) {
    start <- max(1, i - half_window)
    end <- min(n, i + half_window)
    if (end - start + 1 >= 3) {  # At least 3 points for meaningful SD
      rolling_sd[i] <- sd(data[[column]][start:end], na.rm = TRUE)
    }
  }
  
  # Replace NA values with original values
  rolling_mean[is.na(rolling_mean)] <- data[[column]][is.na(rolling_mean)]
  
  # Replace missing SD values with overall SD
  rolling_sd[is.na(rolling_sd)] <- sd(data[[column]], na.rm = TRUE)
  
  # Calculate Z-scores
  z_scores <- abs((data[[column]] - rolling_mean) / (rolling_sd + 0.0001))  # Small constant to avoid division by 0
  
  # Identify outliers
  # Special treatment for RPM: don't consider values of 0 as outliers
  if (column == "RPM") {
    outliers <- which(z_scores > threshold & !is.na(z_scores) & data[[column]] != 0)
  } else {
    outliers <- which(z_scores > threshold & !is.na(z_scores))
  }
  
  # Replace outliers with rolling mean
  if (length(outliers) > 0) {
    cleaned_data[[column]][outliers] <- rolling_mean[outliers]
    cat(paste("Outliers found in", column, ":", length(outliers), "\n"))
  } else {
    cat(paste("No outliers found in", column, ".\n"))
  }
  
  return(cleaned_data)
}

# Function to smooth curves with adjustable window
smooth_curve <- function(data, columns, window_sizes) {
  # Create a copy of the data
  smoothed_data <- data
  
  # Smooth each specified column
  for (i in 1:length(columns)) {
    column <- columns[i]
    window_size <- window_sizes[i]
    
    # Special treatment for RPM column
    if (column == "RPM") {
      # Temporary copy for calculation
      temp_data <- data[[column]]
      zero_indices <- which(temp_data == 0)
      
      # Calculate rolling mean (without affecting 0 values)
      non_zero_indices <- which(temp_data != 0)
      if (length(non_zero_indices) > 0) {
        # Smooth only non-zero values
        smoothed_values <- rep(NA, length(temp_data))
        smoothed_values[non_zero_indices] <- rollmean(temp_data[non_zero_indices], 
                                                      window_size, fill = NA, align = "center")
        
        # Fill NA values at non-zero positions with original values
        na_non_zero <- non_zero_indices[is.na(smoothed_values[non_zero_indices])]
        if (length(na_non_zero) > 0) {
          smoothed_values[na_non_zero] <- temp_data[na_non_zero]
        }
        
        # Retain 0 values
        smoothed_values[zero_indices] <- 0
        
        # Insert smoothed values into dataset
        smoothed_data[[column]] <- smoothed_values
      }
    } else {
      # Standard smoothing for other columns
      smoothed_values <- rollmean(data[[column]], window_size, fill = NA, align = "center")
      
      # Fill NA values at the beginning and end with original values
      na_indices <- which(is.na(smoothed_values))
      if (length(na_indices) > 0) {
        smoothed_values[na_indices] <- data[[column]][na_indices]
      }
      
      # Insert smoothed values into dataset
      smoothed_data[[column]] <- smoothed_values
    }
    
    cat(paste("Curve", column, "smoothed with window size", window_size, "\n"))
  }
  
  return(smoothed_data)
}

# Alternative/additional method specifically for respiratory gas values
detect_gas_outliers <- function(data) {
  # Differentiate VO2 to detect rapid changes
  vo2_diff <- c(0, diff(data$VO2_t))
  vco2_diff <- c(0, diff(data$VCO2_t))
  
  # Standard deviation of changes
  vo2_diff_sd <- sd(vo2_diff, na.rm = TRUE)
  vco2_diff_sd <- sd(vco2_diff, na.rm = TRUE)
  
  # Very strict thresholds for outliers
  vo2_outliers <- which(abs(vo2_diff) > 1.2 * vo2_diff_sd)
  vco2_outliers <- which(abs(vco2_diff) > 1.2 * vco2_diff_sd)
  
  cat(paste("Additional outliers found in VO2:", length(vo2_outliers), "\n"))
  cat(paste("Additional outliers found in VCO2:", length(vco2_outliers), "\n"))
  
  # For VO2
  if (length(vo2_outliers) > 0) {
    # Replace outliers with smoothed values
    window_size <- 5
    for (i in vo2_outliers) {
      start_idx <- max(1, i - floor(window_size/2))
      end_idx <- min(nrow(data), i + floor(window_size/2))
      if (end_idx > start_idx) {
        data$VO2_t[i] <- mean(data$VO2_t[start_idx:end_idx], na.rm = TRUE)
      }
    }
  }
  
  # For VCO2
  if (length(vco2_outliers) > 0) {
    # Replace outliers with smoothed values
    window_size <- 5
    for (i in vco2_outliers) {
      start_idx <- max(1, i - floor(window_size/2))
      end_idx <- min(nrow(data), i + floor(window_size/2))
      if (end_idx > start_idx) {
        data$VCO2_t[i] <- mean(data$VCO2_t[start_idx:end_idx], na.rm = TRUE)
      }
    }
  }
  
  return(data)
}

# Completely revised function to calculate steady state (SS) VO2 values and RPM avg
calculate_ss_vo2 <- function(data, start_time, end_time, rpm_target) {
  # Filter data for the specific stage
  stage_data <- data %>% 
    filter(t_s >= start_time & t_s <= end_time)
  
  # Make sure we have data
  if(nrow(stage_data) == 0) {
    cat(sprintf("No data found for stage %d-%d (rpm target %d)\n", start_time, end_time, rpm_target))
    return(list(ss_vo2 = NA, avg_rpm = NA))
  }
  
  # Get the last 120 seconds of the stage
  end_time_actual <- max(stage_data$t_s)
  start_time_last120 <- max(start_time, end_time_actual - 120)
  
  last_120s <- stage_data %>%
    filter(t_s >= start_time_last120)
  
  # Check if we have enough data
  if(nrow(last_120s) < 10) {  # Need at least some data points
    cat(sprintf("Not enough data points in last 120s for stage %d-%d (rpm target %d)\n", 
                start_time, end_time, rpm_target))
    return(list(ss_vo2 = NA, avg_rpm = NA))
  }
  
  # Find the highest 90s average for VO2
  if(nrow(last_120s) >= 90) {
    # Calculate rolling 90s averages for VO2
    rolling_vo2 <- rollmean(last_120s$VO2_t, k = 90, align = "left", fill = NA)
    max_vo2 <- max(rolling_vo2, na.rm = TRUE)
    
    # Find the window with the highest average VO2
    best_window_start <- which.max(rolling_vo2)
    best_window_indices <- best_window_start:(best_window_start + 89)
    
    # For the exact same time period, calculate average RPM
    # First, get the time points of this best window
    best_window_times <- last_120s$t_s[best_window_indices]
    
    # Get RPM for these exact time points (may need to join data)
    # Create a temporary dataframe with just the time points we need
    best_times_df <- data.frame(t_s = best_window_times)
    
    # Now find the closest t_s_rpm value for each t_s in our best window
    best_rpm_data <- data.frame()
    for(t in best_window_times) {
      # Find closest t_s_rpm to this t_s
      closest_row <- df %>% 
        filter(!is.na(RPM)) %>%
        mutate(time_diff = abs(t_s_rpm - t)) %>%
        arrange(time_diff) %>%
        slice(1)
      
      if(nrow(closest_row) > 0) {
        best_rpm_data <- rbind(best_rpm_data, closest_row)
      }
    }
    
    # Calculate average RPM from this matched data
    if(nrow(best_rpm_data) > 0) {
      avg_rpm <- mean(best_rpm_data$RPM, na.rm = TRUE)
    } else {
      # Use target RPM as fallback
      avg_rpm <- rpm_target
      cat(sprintf("Warning: No RPM data found for best VO2 window in stage with target %d rpm. Using target value.\n", 
                  rpm_target))
    }
  } else {
    # If we don't have 90 data points, use all available data
    max_vo2 <- mean(last_120s$VO2_t, na.rm = TRUE)
    
    # For RPM, we need to do the same time-matching as above but for all points
    all_times <- last_120s$t_s
    
    rpm_data <- data.frame()
    for(t in all_times) {
      closest_row <- df %>% 
        filter(!is.na(RPM)) %>%
        mutate(time_diff = abs(t_s_rpm - t)) %>%
        arrange(time_diff) %>%
        slice(1)
      
      if(nrow(closest_row) > 0) {
        rpm_data <- rbind(rpm_data, closest_row)
      }
    }
    
    if(nrow(rpm_data) > 0) {
      avg_rpm <- mean(rpm_data$RPM, na.rm = TRUE)
    } else {
      avg_rpm <- rpm_target
    }
  }
  
  return(list(ss_vo2 = max_vo2, avg_rpm = avg_rpm))
}

# New function to calculate lowest VO2 for stage 0 (resting phase)
calculate_min_ss_vo2 <- function(data, start_time, end_time) {
  # Filter data for the specific stage
  stage_data <- data %>% 
    filter(t_s >= start_time & t_s <= end_time)
  
  # Make sure we have data
  if(nrow(stage_data) == 0) {
    cat(sprintf("No data found for stage 0 resting phase %d-%d\n", start_time, end_time))
    return(list(ss_vo2 = NA, avg_rpm = 0))
  }
  
  # Debug: Print how many data points we have
  cat(sprintf("Stage 0 data points: %d (from %d to %d)\n", nrow(stage_data), start_time, end_time))
  
  # Check if we have enough data for 90-second windows
  if(nrow(stage_data) < 90) {
    cat(sprintf("WARNING: Not enough data points for 90s window in resting phase %d-%d, using average of all points\n", start_time, end_time))
    # If not enough points, just use the mean of all available data
    mean_vo2 <- mean(stage_data$VO2_t, na.rm = TRUE)
    cat(sprintf("Stage 0 rpm: Mean VO2 = %.1f (insufficient data for window calculation)\n", mean_vo2))
    return(list(ss_vo2 = mean_vo2, avg_rpm = 0))
  }
  
  # Calculate rolling 90s averages for VO2 for the entire stage
  rolling_vo2 <- rollmean(stage_data$VO2_t, k = 90, align = "left", fill = NA)
  
  # Debug: Check if we have valid averages
  cat(sprintf("Valid rolling averages: %d, NA values: %d\n", 
              sum(!is.na(rolling_vo2)), sum(is.na(rolling_vo2))))
  
  # Make sure we have at least one valid average
  if(all(is.na(rolling_vo2))) {
    cat("WARNING: No valid 90s windows found, using average of all data points\n")
    mean_vo2 <- mean(stage_data$VO2_t, na.rm = TRUE)
    return(list(ss_vo2 = mean_vo2, avg_rpm = 0))
  }
  
  # Find the lowest average VO2 (this is different from the standard function)
  min_vo2 <- min(rolling_vo2, na.rm = TRUE)
  
  cat(sprintf("Stage 0 rpm: Lowest SS VO2 = %.1f\n", min_vo2))
  
  return(list(ss_vo2 = min_vo2, avg_rpm = 0))
}

# Read Excel file (sheet "Spiro")
file_path <- "Spiro_Will.xlsx"
df <- read_excel(file_path, sheet = "Spiro")

# Keep only the required columns
df <- df %>% 
  select(t_s, Marker, HF, VT, AF, VE, VO2_t, VCO2_t, t_s_rpm, power, RPM) %>%
  mutate(RPM = na_if(RPM, 0)) 

# Outlier cleaning for cadence - very low sensitivity
df <- clean_outliers(df, "RPM", window_size = 15, threshold = 1.5)

# Outlier cleaning for HR - medium sensitivity
df <- clean_outliers(df, "HF", window_size = 15, threshold = 2.0)

# Outlier cleaning for VO2_t and VCO2_t - very sensitive
df <- clean_outliers(df, "VO2_t", window_size = 15, threshold = 1.5)
df <- clean_outliers(df, "VCO2_t", window_size = 15, threshold = 1.5)

# Apply additional outlier detection for respiratory gases
df <- detect_gas_outliers(df)

# Calculate the lowest average VO2 over 120s
df_for_rest_vo2 <- df %>% 
  select(t_s, VO2_t) %>% 
  filter(!is.na(VO2_t) & !is.na(t_s))

# Define time limits for the search
search_start_time <- 0
search_end_time <- 2000
window_k <- 120 

# Calculate rolling average for VO2 over the *entire* dataset
rolling_vo2_avg_full <- tryCatch({
  rollmean(df_for_rest_vo2$VO2_t, k = window_k, fill = NA, align = "right", na.rm = TRUE)
}, warning = function(w) {
  cat("Warning occurred in rollmean (possibly fewer than", window_k, "data points):", conditionMessage(w), "\n")
  rep(NA_real_, nrow(df_for_rest_vo2))
}, error = function(e) {
  cat("Error occurred in rollmean:", conditionMessage(e), "\n")
  rep(NA_real_, nrow(df_for_rest_vo2))
})

# Find the indices in the original DataFrame that correspond to the time window
time_indices_in_range <- which(df_for_rest_vo2$t_s >= search_start_time & df_for_rest_vo2$t_s <= search_end_time)

# Ensure that rolling_vo2_avg_full has the same length as df_for_rest_vo2
if(length(rolling_vo2_avg_full) == nrow(df_for_rest_vo2)) {
  
  # Select the rolling averages that are within the time window
  rolling_vo2_avg_in_range <- rolling_vo2_avg_full[time_indices_in_range]
  
  # Check if valid averages were found in the time range
  if (length(rolling_vo2_avg_in_range) > 0 && any(!is.na(rolling_vo2_avg_in_range))) {
    
    # Find the lowest average value only in the specified time range
    min_rolling_vo2_l_min <- min(rolling_vo2_avg_in_range, na.rm = TRUE)
    
    # Create and display text message
    message_vo2_rest <- sprintf("VO2_rest (lowest 120s average between %.0fs and %.0fs) is %.3f l/min", 
                                search_start_time, search_end_time, min_rolling_vo2_l_min)
    cat(message_vo2_rest, "\n")
    
  } else {
    cat(sprintf("No valid 120s average values found in the time range %.0fs to %.0fs.\n",
                search_start_time, search_end_time))
  }
} else {
  cat("Error: Length of rolling average does not match data length.\n")
}

# Smooth curves with individual window sizes for each curve
columns_to_smooth <- c("VO2_t", "VCO2_t", "HF", "RPM")
window_sizes <- c(12, 12, 12, 9)  # Individual window sizes for each curve

# Apply smoothing
df <- smooth_curve(df, columns_to_smooth, window_sizes)

# For displaying RPM values: exclude 0 values (will be filtered later)
df_rpm_base <- df %>%
  filter(RPM > 0) %>%
  select(t_s_rpm, RPM)

# ---- ANGEPASST: Define stage times for test 1 (0 Watt) with stages shifted right ----
start_time1 <- 1487-360
end_time1 <- 3300
rpm_values1 <- c(0, 20, 40, 60, 80, 100, 120, 140, 160)

# Shift time_points1 to align with stages correctly (180s offset from original)
time_points1 <- seq(start_time1 + 180, 2940 + 180, 180)  # Starting points shifted right
first_vertical_line1 <- time_points1[2]  # Save the position of the first vertical line (now skipping first one)

# Remove first AND last vertical line for Plot 1
vertical_lines1 <- time_points1[2:(length(time_points1)-1)]  # Skip both first and last vertical lines

# Calculate SS values for each stage of test 1
df_DT1_SS <- data.frame(
  rpm_target = rpm_values1,
  ss_vo2 = numeric(length(rpm_values1)),
  avg_rpm = numeric(length(rpm_values1))
)

# Calculate SS for Stage 0 rpm in Plot 1 (special case - use lowest VO2 in the range)
# Debug: Check the time range for plot 1 stage 0
cat(sprintf("\nPlot 1 - Stage 0 time range: %.1f to %.1f\n", start_time1, first_vertical_line1))

# Fallback value for Plot 1 Stage 0 rpm in case calculation fails
fallback_stage0_vo2_plot1 <- mean(df$VO2_t[df$t_s >= start_time1 & df$t_s <= first_vertical_line1], na.rm = TRUE)
cat(sprintf("Fallback VO2 value for Plot 1 Stage 0: %.1f\n", fallback_stage0_vo2_plot1))

# Try to calculate min SS VO2 for Plot 1 Stage 0
ss_result0_plot1 <- calculate_min_ss_vo2(df, start_time1, first_vertical_line1)

# If calculation failed, use fallback value
if(is.na(ss_result0_plot1$ss_vo2)) {
  cat("Using fallback value for Plot 1 Stage 0\n")
  df_DT1_SS$ss_vo2[1] <- fallback_stage0_vo2_plot1
} else {
  df_DT1_SS$ss_vo2[1] <- ss_result0_plot1$ss_vo2
}
df_DT1_SS$avg_rpm[1] <- 0

# Calculate SS for other stages in Plot 1
for(i in 2:length(rpm_values1)) {
  if(i < length(time_points1)) {
    # Define start and end time for this stage
    stage_start <- time_points1[i]
    stage_end <- time_points1[i+1]
    
    # Calculate SS value for this stage, passing the target RPM
    ss_result <- calculate_ss_vo2(df, stage_start, stage_end, rpm_values1[i])
    df_DT1_SS$ss_vo2[i] <- ss_result$ss_vo2
    df_DT1_SS$avg_rpm[i] <- ss_result$avg_rpm
    
    cat(sprintf("Test 1 - Stage %d rpm: SS VO2 = %.1f, Avg RPM = %.1f\n", 
                rpm_values1[i], ss_result$ss_vo2, ss_result$avg_rpm))
  }
}

# ---- ANGEPASST: Define stage times for test 2 (200 Watt) with stages shifted right ----
start_time2 <- 4207-360
end_time2 <- 6000
rpm_values2 <- c(0, 20, 40, 60, 80, 100, 120, 140, 160)

# Shift time_points2 to align with stages correctly (180s offset from original)
time_points2 <- seq(start_time2 + 180, 5700 + 180, 180)  # Starting points shifted right
first_vertical_line2 <- time_points2[2]  # Save the position of the first vertical line

# Remove the first and last vertical line for Plot 2
vertical_lines2 <- time_points2[2:(length(time_points2)-1)]  # Skip first and last vertical lines

# Calculate SS values for each stage of test 2
df_DT2_SS <- data.frame(
  rpm_target = rpm_values2,
  ss_vo2 = numeric(length(rpm_values2)),
  avg_rpm = numeric(length(rpm_values2))
)

# Calculate SS for Stage 0 rpm in Plot 2 (special case - use lowest VO2 in the range)
# Debug: Check the time range for plot 2 stage 0
cat(sprintf("\nPlot 2 - Stage 0 time range: %.1f to %.1f\n", start_time2, first_vertical_line2))

# Fallback value for Plot 2 Stage 0 rpm in case calculation fails
fallback_stage0_vo2_plot2 <- mean(df$VO2_t[df$t_s >= start_time2 & df$t_s <= first_vertical_line2], na.rm = TRUE)
cat(sprintf("Fallback VO2 value for Plot 2 Stage 0: %.1f\n", fallback_stage0_vo2_plot2))

# Try to calculate min SS VO2 for Plot 2 Stage 0
ss_result0_plot2 <- calculate_min_ss_vo2(df, start_time2, first_vertical_line2)

# If calculation failed, use fallback value
if(is.na(ss_result0_plot2$ss_vo2)) {
  cat("Using fallback value for Plot 2 Stage 0\n")
  df_DT2_SS$ss_vo2[1] <- fallback_stage0_vo2_plot2
} else {
  df_DT2_SS$ss_vo2[1] <- ss_result0_plot2$ss_vo2
}
df_DT2_SS$avg_rpm[1] <- 0

# Calculate SS for other stages in Plot 2, including stages 20-140 rpm
for(i in 2:(length(rpm_values2)-1)) {
  if(i < length(time_points2)) {
    # Define start and end time for this stage
    stage_start <- time_points2[i]
    stage_end <- time_points2[i+1]
    
    # Calculate SS value for this stage, passing the target RPM
    ss_result <- calculate_ss_vo2(df, stage_start, stage_end, rpm_values2[i])
    df_DT2_SS$ss_vo2[i] <- ss_result$ss_vo2
    df_DT2_SS$avg_rpm[i] <- ss_result$avg_rpm
    
    cat(sprintf("Test 2 - Stage %d rpm: SS VO2 = %.1f, Avg RPM = %.1f\n", 
                rpm_values2[i], ss_result$ss_vo2, ss_result$avg_rpm))
  }
}

# Spezielle Berechnung für Stage 160 rpm in Plot 2
# Definiere den Zeitbereich für Stage 160 rpm in Plot 2
last_stage_index2 <- length(rpm_values2)
last_stage_start <- time_points2[last_stage_index2-1]  # Startzeit der letzten Stage
last_stage_end <- end_time2                            # Ende der Daten verwenden
last_stage_rpm <- rpm_values2[last_stage_index2]

# Debug Ausgabe
cat(sprintf("\nPlot 2 - Stage 160 rpm time range: %.1f to %.1f\n", 
            last_stage_start, last_stage_end))

# Filter für den spezifischen Zeitraum der Stage 160 rpm
stage_160_data <- df %>% 
  filter(t_s >= last_stage_start & t_s <= last_stage_end)

# Überprüfen, ob wir ausreichend Daten haben
if(nrow(stage_160_data) < 90) {
  cat("WARNING: Not enough data points for Stage 160 rpm in Plot 2\n")
  # Fallback: Nutze den Durchschnitt, wenn nicht genug Daten vorhanden sind
  ss_vo2_value <- mean(stage_160_data$VO2_t, na.rm = TRUE)
  avg_rpm_value <- mean(df_rpm_base$RPM[df_rpm_base$t_s_rpm >= last_stage_start & 
                                          df_rpm_base$t_s_rpm <= last_stage_end], na.rm = TRUE)
} else {
  # Berechne rollenden 90s-Durchschnitt für VO2
  rolling_vo2 <- rollmean(stage_160_data$VO2_t, k = 90, align = "left", fill = NA)
  
  # Finde den höchsten Durchschnittswert
  max_vo2 <- max(rolling_vo2, na.rm = TRUE)
  
  # Finde den Index des Fensters mit dem höchsten VO2-Durchschnitt
  best_window_start <- which.max(rolling_vo2)
  best_window_indices <- best_window_start:(best_window_start + 89)
  
  # Zeitpunkte dieses besten Fensters ermitteln
  best_window_times <- stage_160_data$t_s[best_window_indices]
  
  # Debug Ausgabe
  cat(sprintf("Best 90s window for Stage 160 rpm in Plot 2: From t=%.1f to t=%.1f\n", 
              min(best_window_times), max(best_window_times)))
  
  # VO2 SS-Wert ist der höchste 90s-Durchschnitt
  ss_vo2_value <- max_vo2
  
  # Für RPM: Finde die zugehörigen RPM-Werte für denselben Zeitraum
  rpm_data_for_best_window <- data.frame()
  for(t in best_window_times) {
    # Finde den nächstgelegenen Zeitpunkt in den RPM-Daten
    closest_row <- df_rpm_base %>% 
      mutate(time_diff = abs(t_s_rpm - t)) %>%
      arrange(time_diff) %>%
      slice(1)
    
    if(nrow(closest_row) > 0) {
      rpm_data_for_best_window <- rbind(rpm_data_for_best_window, closest_row)
    }
  }
  
  # Berechne den RPM-Durchschnitt für diesen Zeitraum
  if(nrow(rpm_data_for_best_window) > 0) {
    avg_rpm_value <- mean(rpm_data_for_best_window$RPM, na.rm = TRUE)
    cat(sprintf("Avg RPM for best VO2 window in Stage 160 rpm: %.1f (from %d data points)\n", 
                avg_rpm_value, nrow(rpm_data_for_best_window)))
  } else {
    # Fallback: Nutze den Target RPM, wenn keine Daten gefunden wurden
    avg_rpm_value <- last_stage_rpm
    cat("WARNING: No RPM data found for best VO2 window. Using target RPM.\n")
  }
}

# Speichere die berechneten Werte
df_DT2_SS$ss_vo2[last_stage_index2] <- ss_vo2_value
df_DT2_SS$avg_rpm[last_stage_index2] <- avg_rpm_value

cat(sprintf("Test 2 - Stage %d rpm: SS VO2 = %.1f, Avg RPM = %.1f\n", 
            last_stage_rpm, ss_vo2_value, avg_rpm_value))

# Ensure RPM values are correct for stage 0
df_DT1_SS$avg_rpm[df_DT1_SS$rpm_target == 0] <- 0
df_DT2_SS$avg_rpm[df_DT2_SS$rpm_target == 0] <- 0

# Print the complete results
cat("\nSteady State values for Test 1 (0 Watt):\n")
print(df_DT1_SS)
cat("\nSteady State values for Test 2 (200 Watt):\n")
print(df_DT2_SS)

# Filter dataset for the first diagram 
df_plot1 <- df %>%
  filter(t_s >= start_time1 & t_s <= end_time1)

# RPM for Plot 1 only show up to t_s = 2923
df_rpm_plot1 <- df_rpm_base %>%
  filter(t_s_rpm >= start_time1 & t_s_rpm <= 2923)

# Create Plot 1 
plot1 <- plot_ly() %>%
  # Data with t_s as x-axis
  add_trace(data = df_plot1, x = ~t_s, y = ~VO2_t, type = 'scatter', mode = 'lines', 
            name = "V̇O<sub>2</sub>", 
            line = list(color = '#1CADE4')) %>%
  add_trace(data = df_plot1, x = ~t_s, y = ~VCO2_t, type = 'scatter', mode = 'lines', 
            name = "V̇CO<sub>2</sub>", 
            line = list(color = '#EF5350')) %>%
  # Cadence with t_s_rpm as x-axis, only for values > 0
  add_trace(data = df_rpm_plot1, x = ~t_s_rpm, y = ~RPM, type = 'scatter', mode = 'lines', 
            name = "Cadence", yaxis = "y2", 
            line = list(color = 'darkgrey'),
            connectgaps = FALSE) %>%  # No connection across gaps
  layout(
    margin = list(t = 40, b = 50, l = 60, r = 60), 
    title = paste("Cadence Test - 0 Watt"),
    xaxis = list(title = "Time [s]", tickvals = seq(0, end_time1, 500)),
    yaxis = list(title = "V̇O<sub>2</sub> & V̇CO<sub>2</sub> [ml · min<sup>-1</sup>]", 
                 range = c(0, 7000), tickvals = seq(0, 7000, 500),
                 tickformat = '.0f'),
    yaxis2 = list(title = "Cadence [rpm]", 
                  overlaying = "y", side = "right", 
                  range = c(0, 280), tickvals = seq(0, 280, 20)),
    showlegend = TRUE,
    legend = list(
      x = 0.90,
      y = 1.00,
      xanchor = "left",
      yancho = "top",
      bgcolor = "rgba(255, 255, 255, 0.3)"
    ))

# Add vertical lines for Plot 1 - now WITHOUT the first AND last one
for(t in vertical_lines1) {
  plot1 <- plot1 %>% add_segments(x = t, xend = t, y = 0, yend = 7000, 
                                  line = list(color = 'darkgrey', dash = 'dash', width = 1),
                                  showlegend = FALSE)
}

# Add cadence stage annotations and SS values for Plot 1
# Special handling for "Stage: 0 rpm" - place in middle between start and first vertical line
mid_point0_plot1 <- (start_time1 + first_vertical_line1) / 2

# Add Stage 0 rpm annotation in the middle between start and first vertical line
plot1 <- plot1 %>% add_annotations(
  x = mid_point0_plot1,
  y = 6750,
  text = "Stage: 0 rpm",
  showarrow = FALSE,
  font = list(size = 10)
)

# Add SS VO2 value for Stage 0 in Plot 1
vo2_text_plot1_stage0 <- sprintf("V̇O<sub>2,SS</sub>: %.0f ml · min<sup>-1</sup>", df_DT1_SS$ss_vo2[1])

plot1 <- plot1 %>% add_annotations(
  x = mid_point0_plot1,
  y = 6500,
  text = vo2_text_plot1_stage0,
  showarrow = FALSE,
  font = list(size = 9)
)

# Add Cadence avg value
rpm_text_plot1_stage0 <- sprintf("Cadence<sub>avg</sub>: %.0f rpm", df_DT1_SS$avg_rpm[1])

plot1 <- plot1 %>% add_annotations(
  x = mid_point0_plot1,
  y = 6350,
  text = rpm_text_plot1_stage0,
  showarrow = FALSE,
  font = list(size = 9)
)

# Add annotations for other stages as before
for(i in 2:(length(rpm_values1)-1)) {  # Exclude the last stage as it's incomplete
  if(i < length(time_points1) - 1) {  
    mid_point1 <- (time_points1[i] + time_points1[i+1]) / 2
    
    # Add stage annotation
    plot1 <- plot1 %>% add_annotations(
      x = mid_point1,
      y = 6750,
      text = paste("Stage:", rpm_values1[i], "rpm"),
      showarrow = FALSE,
      font = list(size = 10)
    )
    
    # Add SS VO2 value as first line
    if(!is.na(df_DT1_SS$ss_vo2[i])) {
      vo2_text <- sprintf("V̇O<sub>2,SS</sub>: %.0f ml · min<sup>-1</sup>", df_DT1_SS$ss_vo2[i])
      
      plot1 <- plot1 %>% add_annotations(
        x = mid_point1,
        y = 6500,
        text = vo2_text,
        showarrow = FALSE,
        font = list(size = 9)
      )
      
      # Add Cadence avg value as second line
      rpm_text <- sprintf("Cadence<sub>avg</sub>: %.0f rpm", df_DT1_SS$avg_rpm[i])
      
      plot1 <- plot1 %>% add_annotations(
        x = mid_point1,
        y = 6350,
        text = rpm_text,
        showarrow = FALSE,
        font = list(size = 9)
      )
    }
  }
}

# Add annotation for the last stage in Plot 1 - CORRECTED POSITION
last_stage_index1 <- length(rpm_values1)
# Calculate mid-point with adjustment to move 180s left from previous position
last_mid_point1 <- time_points1[last_stage_index1] + 90  # Position at middle of stage

plot1 <- plot1 %>% add_annotations(
  x = last_mid_point1,
  y = 6750,
  text = paste("Stage:", rpm_values1[last_stage_index1], "rpm"),
  showarrow = FALSE,
  font = list(size = 10)
)

if(!is.na(df_DT1_SS$ss_vo2[last_stage_index1])) {
  vo2_text <- sprintf("V̇O<sub>2,SS</sub>: %.0f ml · min<sup>-1</sup>", df_DT1_SS$ss_vo2[last_stage_index1])
  
  plot1 <- plot1 %>% add_annotations(
    x = last_mid_point1,
    y = 6500,
    text = vo2_text,
    showarrow = FALSE,
    font = list(size = 9)
  )
  
  rpm_text <- sprintf("Cadence<sub>avg</sub>: %.0f rpm", df_DT1_SS$avg_rpm[last_stage_index1])
  
  plot1 <- plot1 %>% add_annotations(
    x = last_mid_point1,
    y = 6350,
    text = rpm_text,
    showarrow = FALSE,
    font = list(size = 9)
  )
}

# Filter dataset for the second diagram 
df_plot2 <- df %>%
  filter(t_s >= start_time2 & t_s <= end_time2)

# RPM for Plot 2 only show from t = 4207 onward
df_rpm_plot2 <- df_rpm_base %>%
  filter(t_s_rpm >= 4207 & t_s_rpm <= end_time2)

# Create Plot 2
plot2 <- plot_ly() %>%
  # Data with t_s as x-axis
  add_trace(data = df_plot2, x = ~t_s, y = ~VO2_t, type = 'scatter', mode = 'lines', 
            name = "V̇O<sub>2</sub>", 
            line = list(color = '#1CADE4')) %>%
  add_trace(data = df_plot2, x = ~t_s, y = ~VCO2_t, type = 'scatter', mode = 'lines', 
            name = "V̇CO<sub>2</sub>", 
            line = list(color = '#EF5350')) %>%
  # Cadence with t_s_rpm as x-axis, only for values > 0
  add_trace(data = df_rpm_plot2, x = ~t_s_rpm, y = ~RPM, type = 'scatter', mode = 'lines', 
            name = "Cadence", yaxis = "y2", 
            line = list(color = 'darkgrey'),
            connectgaps = FALSE) %>%  # No connection across gaps
  layout(
    margin = list(t = 40, b = 50, l = 60, r = 60), 
    title = paste("Cadence Test - 200 Watt"),
    xaxis = list(title = "Time [s]", tickvals = seq(0, end_time2, 500)),
    yaxis = list(title = "V̇O<sub>2</sub> & V̇CO<sub>2</sub> [ml · min<sup>-1</sup>]", 
                 range = c(0, 7000), tickvals = seq(0, 7000, 500),
                 tickformat = '.0f'),
    yaxis2 = list(title = "Cadence [rpm]", 
                  overlaying = "y", side = "right", 
                  range = c(0, 280), tickvals = seq(0, 280, 20)),
    showlegend = TRUE,
    legend = list(
      x = 0.90,
      y = 1.00,
      xanchor = "left",
      yanchor = "top",
      bgcolor = "rgba(255, 255, 255, 0.3)"
    ))

# Add vertical lines for Plot 2 - WITHOUT first and last vertical lines
for(t in vertical_lines2) {
  plot2 <- plot2 %>% add_segments(x = t, xend = t, y = 0, yend = 7000, 
                                  line = list(color = 'darkgrey', dash = 'dash', width = 1),
                                  showlegend = FALSE)
}

# Add cadence stage annotations and SS values for Plot 2
# Special handling for "Stage: 0 rpm" - place in middle between start and first vertical line
mid_point0_plot2 <- (start_time2 + first_vertical_line2) / 2

# Add Stage 0 rpm annotation in the middle between start and first vertical line
plot2 <- plot2 %>% add_annotations(
  x = mid_point0_plot2,
  y = 6750,
  text = "Stage: 0 rpm",
  showarrow = FALSE,
  font = list(size = 10)
)

# Add SS VO2 value for Stage 0 in Plot 2
vo2_text_plot2_stage0 <- sprintf("V̇O<sub>2,SS</sub>: %.0f ml · min<sup>-1</sup>", df_DT2_SS$ss_vo2[1])

plot2 <- plot2 %>% add_annotations(
  x = mid_point0_plot2,
  y = 6500,
  text = vo2_text_plot2_stage0,
  showarrow = FALSE,
  font = list(size = 9)
)

# Add Cadence avg value
rpm_text_plot2_stage0 <- sprintf("Cadence<sub>avg</sub>: %.0f rpm", df_DT2_SS$avg_rpm[1])

plot2 <- plot2 %>% add_annotations(
  x = mid_point0_plot2,
  y = 6350,
  text = rpm_text_plot2_stage0,
  showarrow = FALSE,
  font = list(size = 9)
)

# Add annotations for other stages (excluding the last one in loop, handling separately)
for(i in 2:(length(rpm_values2)-1)) {
  if(i < length(time_points2) - 1) {
    mid_point2 <- (time_points2[i] + time_points2[i+1]) / 2
    
    # Add stage annotation
    plot2 <- plot2 %>% add_annotations(
      x = mid_point2,
      y = 6750,
      text = paste("Stage:", rpm_values2[i], "rpm"),
      showarrow = FALSE,
      font = list(size = 10)
    )
    
    # Add SS VO2 value as first line
    if(!is.na(df_DT2_SS$ss_vo2[i])) {
      vo2_text <- sprintf("V̇O<sub>2,SS</sub>: %.0f ml · min<sup>-1</sup>", df_DT2_SS$ss_vo2[i])
      
      plot2 <- plot2 %>% add_annotations(
        x = mid_point2,
        y = 6500,
        text = vo2_text,
        showarrow = FALSE,
        font = list(size = 9)
      )
      
      # Add Cadence avg value as second line
      rpm_text <- sprintf("Cadence<sub>avg</sub>: %.0f rpm", df_DT2_SS$avg_rpm[i])
      
      plot2 <- plot2 %>% add_annotations(
        x = mid_point2,
        y = 6350,
        text = rpm_text,
        showarrow = FALSE,
        font = list(size = 9)
      )
    }
  }
}

# Add annotation for the last stage in Plot 2 (Stage 160 rpm) - CORRECTED POSITION
last_stage_index2 <- length(rpm_values2)
# Calculate mid-point with adjustment to move 180s left from previous position
last_mid_point2 <- time_points2[last_stage_index2] + 90  # Position at middle of stage

plot2 <- plot2 %>% add_annotations(
  x = last_mid_point2,
  y = 6750,
  text = paste("Stage:", rpm_values2[last_stage_index2], "rpm"),
  showarrow = FALSE,
  font = list(size = 10)
)

if(!is.na(df_DT2_SS$ss_vo2[last_stage_index2])) {
  vo2_text <- sprintf("V̇O<sub>2,SS</sub>: %.0f ml · min<sup>-1</sup>", df_DT2_SS$ss_vo2[last_stage_index2])
  
  plot2 <- plot2 %>% add_annotations(
    x = last_mid_point2,
    y = 6500,
    text = vo2_text,
    showarrow = FALSE,
    font = list(size = 9)
  )
  
  rpm_text <- sprintf("Cadence<sub>avg</sub>: %.0f rpm", df_DT2_SS$avg_rpm[last_stage_index2])
  
  plot2 <- plot2 %>% add_annotations(
    x = last_mid_point2,
    y = 6350,
    text = rpm_text,
    showarrow = FALSE,
    font = list(size = 9)
  )
}

# Display both plots
print(plot1)
print(plot2)
##############################

# Entfernen der 2. Zeile aus df_DT2_SS
df_DT2_SS_filtered <- df_DT2_SS

# Erstellen der Plotly-Abbildung mit nur Datenpunkten
plot <- plot_ly() %>%
  # Datenpunkte für df_DT1_SS
  add_trace(data = df_DT1_SS, x = ~avg_rpm, y = ~ss_vo2, type = 'scatter', mode = 'markers',
            name = "0 Watt", 
            marker = list(color = '#1CADE4', size = 10)) %>%
  
  # Datenpunkte für df_DT2_SS (ohne die 2. Zeile)
  add_trace(data = df_DT2_SS_filtered, x = ~avg_rpm, y = ~ss_vo2, type = 'scatter', mode = 'markers',
            name = "200 Watt", 
            marker = list(color = '#EF5350', size = 10)) %>%
  
  layout(
    title = "VO₂ Steady State vs. Cadence",
    xaxis = list(title = "Cadence [rpm]", 
                 range = c(0, 200), 
                 tickvals = seq(0, 200, 20)),
    yaxis = list(title = "V̇O<sub>2</sub> [ml · min<sup>-1</sup>]", 
                 range = c(0, 7000), 
                 tickvals = seq(0, 7000, 1000),
                 tickformat = '.0f'),
    showlegend = TRUE,
    legend = list(
      x = 0.90,
      y = 1.00,
      xanchor = "left",
      yanchor = "top",
      bgcolor = "rgba(255, 255, 255, 0.3)"
    )
  )

# Plot anzeigen
plot



#####################################

# Erstellen eines neuen Dataframes für die Differenz
df_diff <- data.frame(
  rpm_target = df_DT2_SS$rpm_target,
  avg_rpm = df_DT2_SS$avg_rpm,
  diff_vo2 = df_DT2_SS$ss_vo2 - df_DT1_SS$ss_vo2
)

# Zeilenindex für spätere Verwendung
df_diff$row_index <- 1:nrow(df_diff)

# Entfernen der ersten Zeile
df_diff_filtered <- df_diff[-1, ]

# Aufteilen in Haupt- und spezielle Punkte
df_diff_main <- df_diff_filtered[3:8, ]  # Zeilen 3-8 (entspricht ursprünglichen Zeilen 4-9 wegen Entfernung von Zeile 1)
df_diff_special <- df_diff_filtered[c(1, nrow(df_diff_filtered)), ]  # Zeile 2 und letzte Zeile

# Lineares Modell für Zeilen 3-8
model <- lm(diff_vo2 ~ avg_rpm, data = df_diff_main)

# Vorhersagepunkte für die Linie
new_x <- data.frame(avg_rpm = seq(0, 200, length.out = 100))
pred <- predict(model, newdata = new_x)

# Erstellen der Plotly-Abbildung
plot_diff <- plot_ly() %>%
  # Hauptdatenpunkte (Zeilen 3-8)
  add_trace(data = df_diff_main, x = ~avg_rpm, y = ~diff_vo2, type = 'scatter', mode = 'markers',
            name = "Hauptdatenpunkte", 
            marker = list(color = '#8BC34A', size = 10)) %>%
  
  # Spezielle Datenpunkte (Zeile 2 und letzte Zeile) mit spezieller Darstellung
  add_trace(data = df_diff_special, x = ~avg_rpm, y = ~diff_vo2, type = 'scatter', mode = 'markers',
            name = "Spezielle Datenpunkte",
            marker = list(
              color = '#FFA500', 
              size = 10,
              symbol = 'circle-open',  # offenes Symbol
              line = list(
                color = '#FFA500',
                width = 2,
                dash = 'dash'  # gestrichelte Linie für den Umriss
              )
            )) %>%
  
  # Regressionslinie für Zeilen 3-8
  add_trace(x = new_x$avg_rpm, y = pred, type = 'scatter', mode = 'lines',
            line = list(color = '#8BC34A', width = 2),
            name = sprintf("Regression: y = %.2f + %.2fx", coef(model)[1], coef(model)[2])) %>%
  
  layout(
    title = "Differenz der VO₂ Steady State Werte mit linearer Regression",
    xaxis = list(title = "Cadence [rpm]", 
                 range = c(0, 200), 
                 tickvals = seq(0, 200, 20)),
    yaxis = list(title = "Differenz V̇O<sub>2</sub> [ml · min<sup>-1</sup>]", 
                 range = c(0, 4000), 
                 tickvals = seq(0, 4000, 500),
                 tickformat = '.0f'),
    showlegend = TRUE,
    legend = list(
      x = 0.90,
      y = 1.00,
      xanchor = "left",
      yanchor = "top",
      bgcolor = "rgba(255, 255, 255, 0.3)"
    )
  )

# Plot anzeigen
plot_diff
