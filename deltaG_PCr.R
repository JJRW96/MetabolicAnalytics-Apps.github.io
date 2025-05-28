# Install and load plotly if you haven't already
# install.packages("plotly")
library(plotly)
library(dplyr)

# --- Define constants based on your JavaScript ---
ATP_YIELD_MAX_pH70 <- 65.0
ATP_YIELD_MIN_pH70 <- 50.0
ATP_YIELD_MAX_pH62 <- 57.0
ATP_YIELD_MIN_pH62 <- 48.0
PH_REF_HIGH <- 7.0
PH_REF_LOW <- 6.2
PCR_INIT_APPSTATE <- 24.0 # Assumed appState.pcr_init for this example

# Function to calculate ATP yield based on pH and PCr
calculate_atp_yield <- function(current_pHm, current_pcr_mmol_kg) {
  
  current_ATP_YIELD_MAX_effective <- 0
  current_ATP_YIELD_MIN_effective <- 0
  
  if (current_pHm >= PH_REF_HIGH) {
    current_ATP_YIELD_MAX_effective <- ATP_YIELD_MAX_pH70
    current_ATP_YIELD_MIN_effective <- ATP_YIELD_MIN_pH70
  } else if (current_pHm <= PH_REF_LOW) {
    current_ATP_YIELD_MAX_effective <- ATP_YIELD_MAX_pH62
    current_ATP_YIELD_MIN_effective <- ATP_YIELD_MIN_pH62
  } else {
    pH_fraction <- (current_pHm - PH_REF_LOW) / (PH_REF_HIGH - PH_REF_LOW)
    current_ATP_YIELD_MAX_effective <- ATP_YIELD_MAX_pH62 + (ATP_YIELD_MAX_pH70 - ATP_YIELD_MAX_pH62) * pH_fraction
    current_ATP_YIELD_MIN_effective <- ATP_YIELD_MIN_pH62 + (ATP_YIELD_MIN_pH70 - ATP_YIELD_MIN_pH62) * pH_fraction
  }
  
  pcr_fraction <- max(0, min(1, current_pcr_mmol_kg / PCR_INIT_APPSTATE))
  calculated_yield <- current_ATP_YIELD_MIN_effective + (current_ATP_YIELD_MAX_effective - current_ATP_YIELD_MIN_effective) * pcr_fraction
  
  return(calculated_yield)
}

# --- Generate data for plotting ---
pcr_values_sim <- seq(PCR_INIT_APPSTATE, 1, by = -1) 
ph_values_sim <- c(7.0, 6.6, 6.2)
plot_data <- data.frame()

for (ph_val in ph_values_sim) {
  yields_for_ph <- sapply(pcr_values_sim, function(pcr_val) {
    calculate_atp_yield(ph_val, pcr_val)
  })
  
  temp_data <- data.frame(
    pH = factor(ph_val), # Factor for discrete colors
    PCr_mmol_kg = pcr_values_sim,
    ATP_Yield_kJ_mol = yields_for_ph
  )
  plot_data <- rbind(plot_data, temp_data)
}

# Approximate "Sum_ADP_AMP_Pi" for X-axis similar to Mader/Heck
# S[C] = [PCr] + [Pi] = 28, S[A] = [ATP] + [ADP] + [AMP] = 7
# ATP_approx ~ 5.5 => ADP + AMP ~ 1.5
# Sum_ADP_AMP_Pi_approx = (S[A] - ATP_approx) + (S[C] - PCr)
plot_data <- plot_data %>%
  mutate(Sum_ADP_AMP_Pi_approx = (7 - 5.5) + (28 - PCr_mmol_kg))

# --- Create Plotly plot ---
fig <- plot_ly()

# Add traces for ATP Yield for each pH
for (ph_level in ph_values_sim) {
  data_subset <- subset(plot_data, pH == ph_level)
  fig <- fig %>% add_trace(data = data_subset, 
                           x = ~Sum_ADP_AMP_Pi_approx, 
                           y = ~ATP_Yield_kJ_mol,
                           name = paste("ATP Yield (pH", ph_level, ")"), 
                           type = 'scatter', 
                           mode = 'lines+markers',
                           yaxis = "y1") # Link to left y-axis
}

# Add PCr trace (only once, as it's independent of pH for this plot's X-axis)
# We need a data frame that has Sum_ADP_AMP_Pi_approx and corresponding PCr_mmol_kg
# We can use the data for pH 7.0 for this, as PCr is just on the x-axis relation.
pcr_plot_data <- plot_data %>% 
  filter(pH == 7.0) %>% # Select data for one pH to get the PCr vs Sum mapping
  select(Sum_ADP_AMP_Pi_approx, PCr_mmol_kg) %>%
  distinct() # Ensure unique x-values if any due to rounding

fig <- fig %>% add_trace(data = pcr_plot_data,
                         x = ~Sum_ADP_AMP_Pi_approx,
                         y = ~PCr_mmol_kg,
                         name = "PCr (mmol/kg)",
                         type = 'scatter',
                         mode = 'lines',
                         line = list(dash = 'dash', color = 'grey'), # Grey dashed line for PCr
                         yaxis = "y2") # Link to right y-axis

fig <- fig %>% layout(
  title = list(text = "Calculated ATP Yield (-ΔG<sub>ATP,cyt</sub>) and PCr vs. Metabolic State", y = 0.95),
  xaxis = list(title = "Sum ADP + AMP + Pi (mmol/kg) (approx.)<br><i>Lower values = Rest, Higher values = Fatigue</i>"),
  yaxis = list(title = "-ΔG<sub>ATP,cyt</sub> (kJ/mol)", range = c(45, 70), side="left", showgrid=TRUE), # ATP Yield on left
  yaxis2 = list(title = "PCr (mmol/kg)", range = c(0, 30), overlaying = "y", side = "right", showgrid = FALSE), # PCr on right
  legend = list(title = list(text = "<b>Legend</b>")),
  hovermode = "closest"
)

# Add annotations for context
fig <- fig %>% add_annotations(
  x = 5, y = 68, text = "Rest", showarrow = FALSE, font=list(size=12), yref="y1"
) %>% add_annotations(
  x = 28, y = 47, text = "Fatigue", showarrow = FALSE, font=list(size=12), yref="y1"
)

# Print the figure
fig



### Mit Peak - Korrektur ###

# Install and load plotly if you haven't already
# install.packages("plotly")
library(plotly)
library(dplyr)

# --- Define constants ---
# Optimal pH (e.g., 7.0)
ATP_YIELD_PEAK_pH70 <- 68.0 # Higher peak value at very high PCr
ATP_YIELD_MAX_pH70  <- 65.0 # Value where more linear decline starts
ATP_YIELD_MIN_pH70  <- 50.0 # Value at PCr depletion

# Acidic pH (e.g., 6.2)
ATP_YIELD_PEAK_pH62 <- 60.0 # Higher peak value at very high PCr
ATP_YIELD_MAX_pH62  <- 57.0 # Value where more linear decline starts
ATP_YIELD_MIN_pH62  <- 48.0 # Value at PCr depletion

PH_REF_HIGH <- 7.0
PH_REF_LOW  <- 6.2
PCR_INIT_APPSTATE <- 24.0 # Assumed appState.pcr_init
PCR_THRESHOLD_FOR_PEAK <- 0.90 # PCr fraction above which peak logic applies (e.g., PCr > 21.6 if init is 24)

# Function to calculate ATP yield with peak correction
calculate_atp_yield_with_peak <- function(current_pHm, current_pcr_mmol_kg) {
  
  # 1. Interpolate pH-dependent PEAK, MAX, and MIN yields
  peak_yield_eff <- 0
  max_yield_eff  <- 0 # This is the "shoulder" where the linear part starts
  min_yield_eff  <- 0
  
  if (current_pHm >= PH_REF_HIGH) {
    peak_yield_eff <- ATP_YIELD_PEAK_pH70
    max_yield_eff  <- ATP_YIELD_MAX_pH70
    min_yield_eff  <- ATP_YIELD_MIN_pH70
  } else if (current_pHm <= PH_REF_LOW) {
    peak_yield_eff <- ATP_YIELD_PEAK_pH62
    max_yield_eff  <- ATP_YIELD_MAX_pH62
    min_yield_eff  <- ATP_YIELD_MIN_pH62
  } else {
    pH_fraction    <- (current_pHm - PH_REF_LOW) / (PH_REF_HIGH - PH_REF_LOW)
    peak_yield_eff <- ATP_YIELD_PEAK_pH62 + (ATP_YIELD_PEAK_pH70 - ATP_YIELD_PEAK_pH62) * pH_fraction
    max_yield_eff  <- ATP_YIELD_MAX_pH62  + (ATP_YIELD_MAX_pH70  - ATP_YIELD_MAX_pH62)  * pH_fraction
    min_yield_eff  <- ATP_YIELD_MIN_pH62  + (ATP_YIELD_MIN_pH70  - ATP_YIELD_MIN_pH62)  * pH_fraction
  }
  
  # 2. Calculate current pcr_fraction
  pcr_fraction <- max(0, min(1, current_pcr_mmol_kg / PCR_INIT_APPSTATE))
  
  # 3. Apply logic based on pcr_fraction
  calculated_yield <- 0
  
  if (pcr_fraction >= PCR_THRESHOLD_FOR_PEAK) {
    # In the "peak" region (from threshold to 1.0)
    # Interpolate from peak_yield_eff (at pcr_fraction = 1.0)
    # down to max_yield_eff (at pcr_fraction = PCR_THRESHOLD_FOR_PEAK)
    # Normalize fraction within this peak region:
    fraction_in_peak_region <- (pcr_fraction - PCR_THRESHOLD_FOR_PEAK) / (1.0 - PCR_THRESHOLD_FOR_PEAK)
    # Using a power (e.g., 0.5 for sqrt, or 2 for quadratic) can shape the steepness
    # Let's try a simple linear interpolation within this small peak region for now,
    # or a slightly non-linear one. A sqrt would make it drop fast from the peak.
    # calculated_yield <- max_yield_eff + (peak_yield_eff - max_yield_eff) * sqrt(fraction_in_peak_region)
    # Linear interpolation for simplicity in this segment:
    calculated_yield <- max_yield_eff + (peak_yield_eff - max_yield_eff) * fraction_in_peak_region
    
  } else {
    # Below the threshold: linear interpolation from max_yield_eff down to min_yield_eff
    # Normalize fraction within this main linear region (from 0 to PCR_THRESHOLD_FOR_PEAK)
    fraction_in_linear_region <- pcr_fraction / PCR_THRESHOLD_FOR_PEAK
    calculated_yield <- min_yield_eff + (max_yield_eff - min_yield_eff) * fraction_in_linear_region
  }
  
  return(calculated_yield)
}

# --- Generate data for plotting ---
pcr_values_sim <- seq(PCR_INIT_APPSTATE, 1, by = -0.1) # More data points for smoother curve
ph_values_sim <- c(7.0, 6.6, 6.2)
plot_data_peak <- data.frame()

for (ph_val in ph_values_sim) {
  yields_for_ph_peak <- sapply(pcr_values_sim, function(pcr_val) {
    calculate_atp_yield_with_peak(ph_val, pcr_val)
  })
  
  temp_data_peak <- data.frame(
    pH = factor(ph_val),
    PCr_mmol_kg = pcr_values_sim,
    ATP_Yield_kJ_mol = yields_for_ph_peak
  )
  plot_data_peak <- rbind(plot_data_peak, temp_data_peak)
}

# Approximate "Sum_ADP_AMP_Pi" for X-axis
plot_data_peak <- plot_data_peak %>%
  mutate(Sum_ADP_AMP_Pi_approx = (7 - 5.5) + (28 - PCr_mmol_kg))

# --- Create Plotly plot ---
fig_peak <- plot_ly()

# Add traces for ATP Yield for each pH
for (ph_level in ph_values_sim) {
  data_subset_peak <- subset(plot_data_peak, pH == ph_level)
  fig_peak <- fig_peak %>% add_trace(data = data_subset_peak, 
                                     x = ~Sum_ADP_AMP_Pi_approx, 
                                     y = ~ATP_Yield_kJ_mol,
                                     name = paste("ATP Yield (pH", ph_level, ")"), 
                                     type = 'scatter', 
                                     mode = 'lines+markers',
                                     yaxis = "y1")
}

# Add PCr trace
pcr_plot_data_peak <- plot_data_peak %>% 
  filter(pH == 7.0) %>%
  select(Sum_ADP_AMP_Pi_approx, PCr_mmol_kg) %>%
  distinct()

fig_peak <- fig_peak %>% add_trace(data = pcr_plot_data_peak,
                                   x = ~Sum_ADP_AMP_Pi_approx,
                                   y = ~PCr_mmol_kg,
                                   name = "PCr (mmol/kg)",
                                   type = 'scatter',
                                   mode = 'lines',
                                   line = list(dash = 'dash', color = 'grey'),
                                   yaxis = "y2")

fig_peak <- fig_peak %>% layout(
  title = list(text = "ATP Yield (-ΔG<sub>ATP,cyt</sub>) with Peak Correction & PCr vs. Metabolic State", y=0.95),
  xaxis = list(title = "Sum ADP + AMP + Pi (mmol/kg) (approx.)<br><i>Lower values = Rest, Higher values = Fatigue</i>", range = c(0, 35)),
  yaxis = list(title = "-ΔG<sub>ATP,cyt</sub> (kJ/mol)", range = c(45, 75), side="left", showgrid=TRUE), # Adjusted range for peak
  yaxis2 = list(title = "PCr (mmol/kg)", range = c(0, 30), overlaying = "y", side = "right", showgrid = FALSE),
  legend = list(title = list(text = "<b>Legend</b>")),
  hovermode = "closest"
)

# Add annotations
fig_peak <- fig_peak %>% add_annotations(
  x = 5, y = 72, text = "Rest", showarrow = FALSE, font=list(size=12), yref="y1"
) %>% add_annotations(
  x = 28, y = 47, text = "Fatigue", showarrow = FALSE, font=list(size=12), yref="y1"
)

# Print the figure
fig_peak


#####################
# Install and load plotly if you haven't already
# install.packages("plotly")
library(plotly)
library(dplyr)

# --- Define constants ---
ATP_YIELD_PEAK_pH70 <- 68.0 
ATP_YIELD_MAX_pH70  <- 65.0 
ATP_YIELD_MIN_pH70  <- 50.0 

ATP_YIELD_PEAK_pH62 <- 60.0 
ATP_YIELD_MAX_pH62  <- 57.0 
ATP_YIELD_MIN_pH62  <- 48.0 

PH_REF_HIGH <- 7.0
PH_REF_LOW  <- 6.2
PCR_INIT_APPSTATE <- 24.0 
PCR_THRESHOLD_FOR_PEAK <- 0.90 
PEAK_SMOOTHING_EXPONENT <- 0.5 # Try 0.4 for steeper, 0.6 for less steep initial drop

# Function to calculate ATP yield with peak correction
calculate_atp_yield_with_peak_smoother <- function(current_pHm, current_pcr_mmol_kg) {
  
  peak_yield_eff <- 0
  max_yield_eff  <- 0 
  min_yield_eff  <- 0
  
  if (current_pHm >= PH_REF_HIGH) {
    peak_yield_eff <- ATP_YIELD_PEAK_pH70
    max_yield_eff  <- ATP_YIELD_MAX_pH70
    min_yield_eff  <- ATP_YIELD_MIN_pH70
  } else if (current_pHm <= PH_REF_LOW) {
    peak_yield_eff <- ATP_YIELD_PEAK_pH62
    max_yield_eff  <- ATP_YIELD_MAX_pH62
    min_yield_eff  <- ATP_YIELD_MIN_pH62
  } else {
    pH_fraction    <- (current_pHm - PH_REF_LOW) / (PH_REF_HIGH - PH_REF_LOW)
    peak_yield_eff <- ATP_YIELD_PEAK_pH62 + (ATP_YIELD_PEAK_pH70 - ATP_YIELD_PEAK_pH62) * pH_fraction
    max_yield_eff  <- ATP_YIELD_MAX_pH62  + (ATP_YIELD_MAX_pH70  - ATP_YIELD_MAX_pH62)  * pH_fraction
    min_yield_eff  <- ATP_YIELD_MIN_pH62  + (ATP_YIELD_MIN_pH70  - ATP_YIELD_MIN_pH62)  * pH_fraction
  }
  
  pcr_fraction <- max(0, min(1, current_pcr_mmol_kg / PCR_INIT_APPSTATE))
  calculated_yield <- 0
  
  if (pcr_fraction >= PCR_THRESHOLD_FOR_PEAK) {
    if ( (1.0 - PCR_THRESHOLD_FOR_PEAK) < 1e-6 ) { # Avoid division by zero if threshold is 1.0
      fraction_in_peak_region <- 1.0 # effectively at the peak
    } else {
      fraction_in_peak_region <- (pcr_fraction - PCR_THRESHOLD_FOR_PEAK) / (1.0 - PCR_THRESHOLD_FOR_PEAK)
    }
    # Apply power function for smoothing
    calculated_yield <- max_yield_eff + (peak_yield_eff - max_yield_eff) * (fraction_in_peak_region ^ PEAK_SMOOTHING_EXPONENT)
  } else {
    if (PCR_THRESHOLD_FOR_PEAK < 1e-6) { # Avoid division by zero if threshold is 0
      fraction_in_linear_region <- 1.0 # effectively at max_yield_eff
    } else {
      fraction_in_linear_region <- pcr_fraction / PCR_THRESHOLD_FOR_PEAK
    }
    calculated_yield <- min_yield_eff + (max_yield_eff - min_yield_eff) * fraction_in_linear_region
  }
  
  return(calculated_yield)
}

# --- Generate data for plotting ---
pcr_values_sim <- seq(PCR_INIT_APPSTATE, 1, by = -0.25) # Even more data points
ph_values_sim <- c(7.0, 6.6, 6.2)
plot_data_peak_smooth <- data.frame()

for (ph_val in ph_values_sim) {
  yields_for_ph_peak_smooth <- sapply(pcr_values_sim, function(pcr_val) {
    calculate_atp_yield_with_peak_smoother(ph_val, pcr_val)
  })
  
  temp_data_peak_smooth <- data.frame(
    pH = factor(ph_val),
    PCr_mmol_kg = pcr_values_sim,
    ATP_Yield_kJ_mol = yields_for_ph_peak_smooth
  )
  plot_data_peak_smooth <- rbind(plot_data_peak_smooth, temp_data_peak_smooth)
}

plot_data_peak_smooth <- plot_data_peak_smooth %>%
  mutate(Sum_ADP_AMP_Pi_approx = (7 - 5.5) + (28 - PCr_mmol_kg))

# --- Create Plotly plot ---
fig_peak_smooth <- plot_ly()

for (ph_level in ph_values_sim) {
  data_subset_peak_smooth <- subset(plot_data_peak_smooth, pH == ph_level)
  fig_peak_smooth <- fig_peak_smooth %>% add_trace(data = data_subset_peak_smooth, 
                                                   x = ~Sum_ADP_AMP_Pi_approx, 
                                                   y = ~ATP_Yield_kJ_mol,
                                                   name = paste("ATP Yield (pH", ph_level, ")"), 
                                                   type = 'scatter', 
                                                   mode = 'lines', # Using lines only for smoother appearance with more points
                                                   yaxis = "y1")
}

pcr_plot_data_peak_smooth <- plot_data_peak_smooth %>% 
  filter(pH == 7.0) %>%
  select(Sum_ADP_AMP_Pi_approx, PCr_mmol_kg) %>%
  distinct()

fig_peak_smooth <- fig_peak_smooth %>% add_trace(data = pcr_plot_data_peak_smooth,
                                                 x = ~Sum_ADP_AMP_Pi_approx,
                                                 y = ~PCr_mmol_kg,
                                                 name = "PCr (mmol/kg)",
                                                 type = 'scatter',
                                                 mode = 'lines',
                                                 line = list(dash = 'dash', color = 'grey'),
                                                 yaxis = "y2")

fig_peak_smooth <- fig_peak_smooth %>% layout(
  title = list(text = "ATP Yield (-ΔG<sub>ATP,cyt</sub>) with Smoother Peak Correction & PCr", y=0.95),
  xaxis = list(title = "Sum ADP + AMP + Pi (mmol/kg) (approx.)<br><i>Lower values = Rest, Higher values = Fatigue</i>"),
  yaxis = list(title = "-ΔG<sub>ATP,cyt</sub> (kJ/mol)", range = c(45, 75), side="left", showgrid=TRUE),
  yaxis2 = list(title = "PCr (mmol/kg)", range = c(0, 30), overlaying = "y", side = "right", showgrid = FALSE),
  legend = list(title = list(text = "<b>Legend</b>")),
  hovermode = "closest"
)

fig_peak_smooth <- fig_peak_smooth %>% add_annotations(
  x = 5, y = 72, text = "Rest", showarrow = FALSE, font=list(size=12), yref="y1"
) %>% add_annotations(
  x = 28, y = 47, text = "Fatigue", showarrow = FALSE, font=list(size=12), yref="y1"
)

fig_peak_smooth