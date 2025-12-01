################################################################################
# R SCRIPT: RECURSIVE SPECTRAL CSV PROCESSOR (NO PLOTS)
#
# DATE: December 2025
# AUTHOR: Paul Arellano, PhD
#
# PURPOSE:
# - Recursively process all spectral CSV files under the "Filtered" directory.
# - Apply clipping and Savitzky-Golay filtering.
# - Save processed CSVs into a mirrored folder structure under "Preprossed".
# - Appends "_filtered" to the output filenames.
#
# INPUT ROOT:  C:/Users/pa589/.../SPECTRAL/2024/Filtered
# OUTPUT ROOT: C:/Users/pa589/.../SPECTRAL/2024/Preprossed
#
################################################################################
# Author: Paul Arellano, PhD
# Date: Dec 01, 2025


library(dplyr)
library(tidyr)
library(signal)
library(fs)

# --------------------------------------------------------------------
# CONFIGURATION
# --------------------------------------------------------------------

input_root  <- "C:/Users/pa589/NAU/TREE_STRESS/FIELDWORK_overall/SPECTRAL/2025/Preprocess"
output_root <- "C:/Users/pa589/NAU/TREE_STRESS/FIELDWORK_overall/SPECTRAL/2025/Filtered"

# Filtering parameters
filter_m <- 11  # frame length
filter_p <- 3   # polynomial order

# --------------------------------------------------------------------
# PROCESSING FUNCTION (NO PLOTS)
# --------------------------------------------------------------------

process_spectral_csv <- function(input_file, output_file) {
  
  cat("\nProcessing:", input_file, "\n")
  
  # Load Data
  df <- read.csv(input_file, check.names = FALSE)
  
  # Extract wavelength and spectra
  wavelengths <- df$Wavelength
  spectra_df <- df %>% select(-Wavelength)
  spectrum_ids <- names(spectra_df)
  
  # STEP 1 — Clip to valid range [0,1]
  spectra_df <- spectra_df %>%
    mutate(across(everything(), ~ pmax(0, pmin(1, .))))
  
  # STEP 2 — Apply Savitzky-Golay filter
  filtered_list <- list()
  for (id in spectrum_ids) {
    filtered_list[[id]] <- sgolayfilt(
      spectra_df[[id]],
      m = 0,
      p = filter_p,
      n = filter_m
    )
  }
  
  # Reassemble the final dataframe
  filtered_df <- as.data.frame(filtered_list)
  filtered_df$Wavelength <- wavelengths
  filtered_df <- filtered_df %>% select(Wavelength, everything())
  
  # Create output directory if needed
  dir_create(path_dir(output_file))
  
  # Save output
  write.csv(filtered_df, output_file, row.names = FALSE)
  
  cat("Saved to:", output_file, "\n")
}

# --------------------------------------------------------------------
# MAIN RECURSIVE PROCESS
# --------------------------------------------------------------------

# Get all CSV files recursively
all_csv_files <- dir_ls(input_root, recurse = TRUE, type = "file", glob = "*.csv")

cat("Found", length(all_csv_files), "CSV files to process.\n")

for (csv in all_csv_files) {
  
  # Determine relative path from the input root
  rel_path <- path_rel(csv, start = input_root)
  
  # Build base output path with same folder structure
  base_output_path <- path(output_root, rel_path)
  
  # Modify filename: Remove .csv extension, add _filtered, add .csv back
  output_file <- paste0(path_ext_remove(base_output_path), "_filtered.csv")
  
  # Process file
  process_spectral_csv(csv, output_file)
}

cat("\n\n🎉 ALL FILES PROCESSED SUCCESSFULLY!\n")
