################################################################################
# R SCRIPT: SPECTRAL DATA PROCESSING AND ANALYSIS WORKFLOW
################################################################################
#
# DATE: November 9, 2025
# AUTHOR: Paul Arellano, PhD
# PROJECT: NAU/TREE_STRESS/FIELDWORK_2025/SPECTRA
#
# PURPOSE:
# This script implements a robust two-stage workflow for processing raw spectral 
# reflectance data, focusing on cleaning, filtering, and quality control, while 
# providing visual validation plots at each critical step.
#
# --- PROCESSING STAGES ---
#
# STAGE 1: CLEANING AND VISUALIZATION
# 1. Data Loading: Reads the consolidated CSV file containing spectral reflectance 
#    data (Wavelength column + multiple Spectrum ID columns).
# 2. X-Axis Standardization: Sets the plotting range for all graphs to 400 nm to 2500 nm.
# 3. Water Absorption Band Removal: Identifies and physically removes data points 
#    within the known water absorption regions (1350-1460 nm and 1790-1960 nm) from 
#    the dataset passed to Stage 2.
# 4. Plotting (Plots 1 & 2): Generates two side-by-side plots: 
#    - Plot 1 (Original Raw Data): Shows the full dataset with visual breaks (NAs) 
#      and highlighted absorption bands.
#    - Plot 2 (Cleaned Raw Data): Shows the data after row removal, also enforcing 
#      visual breaks (NAs).
#
# STAGE 2: FILTERING, QUALITY CONTROL, AND EXPORT
# 1. Pre-Filter Clipping: **Clips all reflectance values** in the cleaned dataset to 
#    the physically valid range of **[0, 1]** to prevent filtering artifacts from 
#    out-of-range inputs.
# 2. Savitzky-Golay Filtering: Applies the Savitzky-Golay smoothing filter (using 
#    user-defined frame length 'm' and polynomial order 'p') to **all** spectra 
#    in the cleaned, clipped dataset.
# 3. Plotting (Plot 3): Generates the final plot showing the smoothed spectra, 
#    with visual breaks (NAs) enforced in the absorption regions.
# 4. Data Export: Saves the **complete, final filtered dataset** (with water bands 
#    excluded and smoothing applied) to a new CSV file for further analysis.
#
# --- DEPENDENCIES ---
# Libraries required: ggplot2, signal, dplyr, tidyr, patchwork
#
################################################################################


# 1. Load Required Libraries
library(ggplot2)
library(signal)
library(dplyr)
library(tidyr)
library(patchwork) 

# --- Configuration ---
# 🛑 IMPORTANT: Set the path to your original consolidated CSV file 🛑
input_file_path <- "C:/Users/pa589/NAU/TREE_STRESS/FIELDWORK_2025/SPECTRA/Mormon_Lake/OUTPUT/spectra_consolidated_output_FINAL_WORKING.csv"

# Output file path for the complete filtered data
output_file_path <- "C:/Users/pa589/NAU/TREE_STRESS/FIELDWORK_2025/SPECTRA/Mormon_Lake/OUTPUT/Filtered_Spectra_SG_Two_Stage.csv"

# --- Plotting/Filtering Parameters ---
subset_size <- 10 
filter_m <- 11 # Frame Length
filter_p <- 3  # Polynomial Order

# --- Water Absorption Bands to Remove (Used for Cleaning and Highlighting) ---
water_bands_remove <- list(
  c(1350, 1460), # 1400 nm region (SWIR-1)
  c(1790, 1960)  # 1900 nm region (SWIR-2)
)

# Create a data frame for ggplot geom_rect highlighting
highlight_df <- data.frame(
  xmin = c(1350, 1790),
  xmax = c(1460, 1960),
  ymin = 0,
  ymax = 1
)

# Function to check if a wavelength falls within any absorption band (used for NA insertion in all plots)
is_in_absorption_band <- function(wave) {
  sapply(wave, function(w) {
    any(sapply(water_bands_remove, function(band) w >= band[1] & w <= band[2]))
  })
}

# -------------------------------------------------------------
## 📝 STAGE 1: CLEANING AND INITIAL PLOTTING
# -------------------------------------------------------------

clean_and_plot_raw <- function(input_path, subset_n, bands_to_remove) {
  
  cat("--- STAGE 1: Data Loading, Cleaning, and Initial Plotting ---\n")
  
  # Load Data and Prepare
  data_wide <- read.csv(input_path, check.names = FALSE)
  wavelengths <- data_wide$Wavelength
  data_reflectance <- data_wide %>% select(-Wavelength)
  all_spectrum_ids <- names(data_reflectance)
  
  # Set seed and select random subset IDs
  set.seed(42) 
  if (length(all_spectrum_ids) < subset_n) {
    subset_n <- length(all_spectrum_ids)
  }
  subset_ids <- sample(all_spectrum_ids, subset_n)
  
  cat(paste("Total spectra loaded:", length(all_spectrum_ids), "— Plotting subset of", subset_n, ".\n"))
  
  
  # 1. Prepare Raw Subset (Original) Data and Plot Object
  raw_data_long <- data_wide %>%
    select(Wavelength, all_of(subset_ids)) %>%
    pivot_longer(
      cols = -Wavelength, names_to = "Spectrum_ID", values_to = "Reflectance"
    ) %>%
    filter(Reflectance >= 0) 
  
  # NA FIX: Insert NA values into the original data for plotting breaks (Plot 1)
  raw_data_long_break <- raw_data_long %>%
    mutate(
      Reflectance = case_when(
        is_in_absorption_band(Wavelength) ~ NA_real_,
        TRUE ~ Reflectance
      )
    )
  
  raw_plot <- ggplot(raw_data_long_break, aes(x = Wavelength, y = Reflectance, color = Spectrum_ID)) +
    # Add highlighting layer for original data
    geom_rect(data = highlight_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.1, inherit.aes = FALSE) +
    geom_line(alpha = 0.8) + 
    ylim(0, 1) + 
    xlim(400, 2500) + 
    labs(
      title = paste("1. Original Raw Data (N=", subset_n, ") [W.B. Break]"),
      x = "Wavelength (nm)", y = "Reflectance", color = "Spectrum ID"
    ) + theme_minimal() + theme(legend.position = "none")
  
  
  # 2. Remove Water Absorption Bands & Create New Dataset (for filtering/processing)
  cat("\n2. Removing water absorption bands:\n")
  bands_to_keep_logic <- rep(TRUE, length(wavelengths))
  
  for (band_range in bands_to_remove) {
    min_wave <- band_range[1]
    max_wave <- band_range[2]
    bands_to_keep_logic <- bands_to_keep_logic & !(wavelengths >= min_wave & wavelengths <= max_wave)
    cat(paste("   - Removing band range:", min_wave, "nm to", max_wave, "nm\n"))
  }
  
  wavelengths_cleaned <- wavelengths[bands_to_keep_logic]
  
  # CRITICAL FIX: Robust column naming via tibble conversion (This is the data used for filtering)
  data_reflectance_cleaned <- as.data.frame(as.matrix(data_reflectance)[bands_to_keep_logic, ])
  colnames(data_reflectance_cleaned) <- all_spectrum_ids 
  data_reflectance_cleaned <- as_tibble(data_reflectance_cleaned)
  
  cat(paste("✅ Wavelengths reduced from", length(wavelengths), "to", length(wavelengths_cleaned), ".\n"))
  
  
  # 3. Prepare Raw Subset (Water Bands Removed) Data and Plot Object
  cleaned_raw_data_df <- data_reflectance_cleaned 
  cleaned_raw_data_df$Wavelength <- wavelengths_cleaned
  
  # Get long format subset
  cleaned_raw_long <- cleaned_raw_data_df %>%
    select(Wavelength, all_of(subset_ids)) %>%
    pivot_longer(
      cols = -Wavelength, names_to = "Spectrum_ID", values_to = "Reflectance"
    ) %>%
    filter(Reflectance >= 0)
  
  # 🛑 NA FIX: Insert NA values into the Cleaned Raw Data for plotting breaks (Plot 2)
  # This uses the cleaned data (with fewer rows) and inserts NAs where the removed data used to be.
  cleaned_raw_long_break <- cleaned_raw_long %>%
    mutate(
      Reflectance = case_when(
        is_in_absorption_band(Wavelength) ~ NA_real_,
        TRUE ~ Reflectance
      )
    )
  
  cleaned_raw_plot <- ggplot(cleaned_raw_long_break, aes(x = Wavelength, y = Reflectance, color = Spectrum_ID)) +
    geom_line(alpha = 0.8) + 
    ylim(0, 1) + 
    xlim(400, 2500) + 
    labs(
      title = paste("2. Cleaned Raw Data (W.B. Removed) - N=", subset_n, ") [W.B. Break]"),
      x = "Wavelength (nm)", y = "Reflectance", color = "Spectrum ID"
    ) + theme_minimal() + theme(legend.position = "none")
  
  
  # Combine and EXPLICITLY PRINT Plot 1 and Plot 2
  combined_plot <- (raw_plot + cleaned_raw_plot) + 
    plot_layout(guides = 'collect') + 
    plot_annotation(
      title = 'Comparison: Effect of Water Band Removal on Raw Spectral Profiles',
      theme = theme_minimal()
    )
  
  print(combined_plot) 
  cat("✅ Plotted raw and cleaned profiles side-by-side with line breaks.\n")
  
  
  # Return the cleaned data structure for Stage 2
  final_cleaned_data <- data_reflectance_cleaned
  final_cleaned_data$Wavelength <- wavelengths_cleaned
  
  return(list(
    cleaned_data = final_cleaned_data,
    subset_ids = subset_ids,
    all_ids = all_spectrum_ids 
  ))
}

# -------------------------------------------------------------
## 📝 STAGE 2: FILTERING AND FINAL PLOTTING
# -------------------------------------------------------------

process_and_plot_spectra <- function(cleaned_data_list, output_path, m, p) {
  
  cat("\n--- STAGE 2: Filtering and Final Plotting ---\n")
  
  # Unpack the list received from STAGE 1
  data_cleaned_wide <- cleaned_data_list$cleaned_data
  subset_ids <- cleaned_data_list$subset_ids
  all_spectrum_ids <- cleaned_data_list$all_ids 
  
  wavelengths_cleaned <- data_cleaned_wide$Wavelength
  
  # 🛑 STEP 1: CLIPPING REFLECTANCE DATA TO [0, 1] BEFORE FILTERING 🛑
  cat("1. Clipping reflectance values to the valid range [0, 1] before filtering...\n")
  data_cleaned_wide <- data_cleaned_wide %>%
    mutate(across(!Wavelength, ~ pmax(0, pmin(1, .))))
  cat("✅ Reflectance clipped successfully.\n")
  
  data_to_filter <- data_cleaned_wide %>% select(-Wavelength)
  colnames(data_to_filter) <- all_spectrum_ids 
  subset_n <- length(subset_ids) 
  
  
  # Step 2: Apply Savitzky-Golay Filter to the WHOLE Cleaned Dataset
  cat("2. Applying Savitzky-Golay filter to ALL cleaned spectra...\n") 
  
  filtered_list <- list()
  
  for (col_name in all_spectrum_ids) {
    filtered_list[[col_name]] <- sgolayfilt(
      x = data_to_filter[[col_name]], 
      m = 0, p = p, n = m
    )
  }
  
  # Reassemble the final filtered data frame
  filtered_data_df <- as_tibble(filtered_list) 
  
  filtered_data_df$Wavelength <- wavelengths_cleaned 
  
  filtered_data_df <- filtered_data_df %>%
    dplyr::select(Wavelength, everything())
  
  
  # Step 3: Plot Filtered Subset 
  cat("\n3. Plotting final filtered profiles...\n")
  
  filtered_long <- filtered_data_df %>%
    select(Wavelength, all_of(subset_ids)) %>%
    pivot_longer(
      cols = -Wavelength, names_to = "Spectrum_ID", values_to = "Reflectance"
    ) %>%
    filter(Reflectance >= 0)
  
  # NA FIX: Insert NA values into the final filtered data for plotting breaks (Plot 3)
  filtered_long_break <- filtered_long %>%
    mutate(
      Reflectance = case_when(
        is_in_absorption_band(Wavelength) ~ NA_real_,
        TRUE ~ Reflectance
      )
    )
  
  filtered_plot <- ggplot(filtered_long_break, aes(x = Wavelength, y = Reflectance, color = Spectrum_ID)) +
    geom_line(alpha = 0.8) +
    ylim(0, 1) + 
    xlim(400, 2500) + 
    labs(
      title = paste("3. Filtered Spectral Profiles (Final) - Subset N=", subset_n, ") [W.B. Break]"),
      x = "Wavelength (nm)", y = "Reflectance", color = "Spectrum ID"
    ) + theme_minimal() + theme(legend.position = "bottom")
  
  print(filtered_plot) 
  cat("✅ Plotted final filtered subset profiles with line breaks.\n")
  
  
  # Step 4: Save the COMPLETE Filtered Results
  write.csv(filtered_data_df, file = output_path, row.names = FALSE)
  cat(paste("\n✅ Complete filtered dataset saved to:", output_file_path, "\n"))
  
  cat("\nProcessing complete!\n")

  # Step 4: Save the COMPLETE Filtered Results
  # THIS LINE IS EXECUTED INSIDE THE FUNCTION.
  write.csv(filtered_data_df, file = output_path, row.names = FALSE)
  cat(paste("\n✅ Complete filtered dataset saved to:", output_file_path, "\n"))
  
  cat("\nProcessing complete!\n")
}
# -------------------------------------------------------------
## 🎯 EXECUTION FLOW
# -------------------------------------------------------------

# 1. Run STAGE 1: Load, Clean, and Plot Initial Results
cleaned_data_structure <- clean_and_plot_raw(
  input_file_path, 
  subset_size,
  water_bands_remove
)

# 2. Run STAGE 2: Filter and Plot Final Results
process_and_plot_spectra(
  cleaned_data_structure,
  output_file_path, 
  filter_m, 
  filter_p
)
