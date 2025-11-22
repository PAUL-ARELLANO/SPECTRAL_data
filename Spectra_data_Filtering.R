################################################################################
# R SCRIPT: SPECTRAL DATA PROCESSING AND ANALYSIS WORKFLOW
################################################################################
#
# DATE: November 21, 2025
# AUTHOR: Paul Arellano, PhD
# PROJECT: NAU/TREE_STRESS/FIELDWORK_2025/SPECTRA
#
# PURPOSE:
# This script implements a robust two-stage workflow for processing raw spectral 
# reflectance data, focusing on cleaning, filtering, and quality control, while 
# providing visual validation plots at each critical step.
#
# **MODIFICATION:** This version **RETAINS** the data in the water absorption 
# regions (1350-1460 nm and 1790-1960 nm) for subsequent filtering.
#
# --- PROCESSING STAGES ---
#
# STAGE 1: CLEANING AND VISUALIZATION
# 1. Data Loading: Reads the consolidated CSV file.
# 2. **Water Absorption Band Retention:** **NO** data is removed from the dataset.
# 3. Plotting (Plots 1 & 2): Generates two side-by-side plots (which will now 
#    be identical), showing the raw data profile with all bands included.
#
# STAGE 2: FILTERING, QUALITY CONTROL, AND EXPORT
# 1. Pre-Filter Clipping: Clips all reflectance values to the physically valid 
#    range of **[0, 1]**.
# 2. Savitzky-Golay Filtering: Applies the Savitzky-Golay smoothing filter to 
#    the **ENTIRE** dataset (including water bands).
# 3. Plotting (Plot 3): Generates the final plot showing the smoothed spectra, 
#    with water absorption regions included and processed.
# 4. Data Export: Saves the **complete, final filtered dataset** (with all bands)
#    to a new CSV file.
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
output_file_path <- "C:/Users/pa589/NAU/TREE_STRESS/FIELDWORK_2025/SPECTRA/Mormon_Lake/OUTPUT/Filtered_Spectra_SG_All_Bands_Retained.csv"

# --- Plotting/Filtering Parameters ---
subset_size <- 10 
filter_m <- 11 # Frame Length
filter_p <- 3  # Polynomial Order

# --- Water Absorption Band Definitions (REMOVED) ---
# The logic for removal and plotting breaks is removed.

# -------------------------------------------------------------
## 📝 STAGE 1: CLEANING AND INITIAL PLOTTING (NO BAND REMOVAL)
# -------------------------------------------------------------

# Note: The bands_to_remove argument is now ignored.
clean_and_plot_raw <- function(input_path, subset_n, bands_to_remove) {
    
    cat("--- STAGE 1: Data Loading, Preparation, and Initial Plotting ---\n")
    
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
    # **NOTE: Water Absorption Regions are RETAINED in this modified script.**
    raw_data_long <- data_wide %>%
        select(Wavelength, all_of(subset_ids)) %>%
        pivot_longer(
            cols = -Wavelength, names_to = "Spectrum_ID", values_to = "Reflectance"
        ) %>%
        filter(Reflectance >= 0) 
    
    # --- PLOT 1: Original Data (Continuous) ---
    raw_plot <- ggplot(raw_data_long, aes(x = Wavelength, y = Reflectance, color = Spectrum_ID)) +
        geom_line(alpha = 0.8) + 
        ylim(0, 1) + 
        xlim(400, 2500) + 
        labs(
            title = paste("1. Original Raw Data (N=", subset_n, ") [Water Bands Retained]"),
            x = "Wavelength (nm)", y = "Reflectance", color = "Spectrum ID"
        ) + theme_minimal() + theme(legend.position = "none")
    
    
    # 2. **MODIFICATION: RETAIN ALL BANDS**
    cat("\n2. **Keeping all spectral bands, including water absorption regions.**\n")
    
    # The 'cleaned' data for processing is simply the original data
    wavelengths_cleaned <- wavelengths
    
    # CRITICAL: Prepare data structure without removing any rows
    data_reflectance_cleaned <- as.data.frame(data_reflectance)
    colnames(data_reflectance_cleaned) <- all_spectrum_ids 
    data_reflectance_cleaned <- as_tibble(data_reflectance_cleaned)
    
    cat(paste("✅ All", length(wavelengths_cleaned), "wavelengths retained.\n"))
    
    
    # 3. Prepare Data Ready for Filtering (Identical to Plot 1)
    cleaned_raw_data_df <- data_reflectance_cleaned 
    cleaned_raw_data_df$Wavelength <- wavelengths_cleaned
    
    # Get long format subset
    cleaned_raw_long <- cleaned_raw_data_df %>%
        select(Wavelength, all_of(subset_ids)) %>%
        pivot_longer(
            cols = -Wavelength, names_to = "Spectrum_ID", values_to = "Reflectance"
        ) %>%
        filter(Reflectance >= 0)
    
    cleaned_raw_plot <- ggplot(cleaned_raw_long, aes(x = Wavelength, y = Reflectance, color = Spectrum_ID)) +
        geom_line(alpha = 0.8) + 
        ylim(0, 1) + 
        xlim(400, 2500) + 
        labs(
            title = paste("2. Data Ready for Filtering (N=", subset_n, ") [Water Bands Retained]"),
            x = "Wavelength (nm)", y = "Reflectance", color = "Spectrum ID"
        ) + theme_minimal() + theme(legend.position = "none")
    
    
    # Combine and EXPLICITLY PRINT Plot 1 and Plot 2
    combined_plot <- (raw_plot + cleaned_raw_plot) + 
        plot_layout(guides = 'collect') + 
        plot_annotation(
            title = 'Comparison: Raw Spectral Profiles Before and After Data Preparation (Water Bands Retained)',
            theme = theme_minimal()
        )
    
    print(combined_plot) 
    cat("✅ Plotted original and prepared profiles side-by-side (identical in this version).\n")
    
    
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
## 📝 STAGE 2: FILTERING AND FINAL PLOTTING (ALL BANDS)
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
    cat("2. Applying Savitzky-Golay filter to ALL retained spectra (including water bands)...\n") 
    
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
    
    # Note: NA insertion for plotting breaks has been removed.
    
    filtered_plot <- ggplot(filtered_long, aes(x = Wavelength, y = Reflectance, color = Spectrum_ID)) +
        geom_line(alpha = 0.8) +
        ylim(0, 1) + 
        xlim(400, 2500) + 
        labs(
            title = paste("3. Filtered Spectral Profiles (Final) - Subset N=", subset_n, ") [Water Bands Retained]"),
            x = "Wavelength (nm)", y = "Reflectance", color = "Spectrum ID"
        ) + theme_minimal() + theme(legend.position = "bottom")
    
    print(filtered_plot) 
    cat("✅ Plotted final filtered subset profiles (including water bands).\n") 
    
    
    # Step 4: Save the COMPLETE Filtered Results
    write.csv(filtered_data_df, file = output_path, row.names = FALSE)
    cat(paste("\n✅ Complete filtered dataset saved to:", output_file_path, "\n"))
    
    cat("\nProcessing complete!\n")
}
# -------------------------------------------------------------
## 🎯 EXECUTION FLOW
# -------------------------------------------------------------

# 1. Run STAGE 1: Load, Clean, and Plot Initial Results
# Note: The third argument (bands_to_remove) is passed but ignored by the function's logic.
cleaned_data_structure <- clean_and_plot_raw(
    input_file_path, 
    subset_size,
    list() # Passing an empty list, although the function ignores the removal logic now.
)

# 2. Run STAGE 2: Filter and Plot Final Results
process_and_plot_spectra(
    cleaned_data_structure,
    output_file_path, 
    filter_m, 
    filter_p
)
