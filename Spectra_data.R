################################################################################
# R SCRIPT: ASD SPECTRAL DATA CONSOLIDATOR (spectrolab FIX)
#
# PURPOSE:
# This script reads multiple binary ASD spectral files (from a directory) 
# using the 'spectrolab' package and exports the consolidated spectral data
# into a single, clean CSV file in wide format (Wavelengths in rows, 
# Samples in columns).
#
# COMPATIBILITY NOTES (Crucial Fixes for specific spectrolab versions):
# 1. READ METHOD: Uses 'path =' instead of 'files =' in read_spectra().
# 2. DATA ACCESS: Bypasses non-exported functions (like metadata(), names(), 
#    sample_names()) by directly accessing list elements for names and bands.
# 3. EXTRACTION: Uses t(spectrolab::value(x)) to robustly extract and transpose
#    the reflectance matrix, as recommended by the package author.
#
# INPUT:
# - A single directory containing multiple .asd files.
#
# OUTPUT:
# - One CSV file in wide format (Wavelength | Sample_1 | Sample_2 | ...).
#
# REQUIRED LIBRARIES:
# - spectrolab
# - dplyr
# - tidyr
################################################################################
# Autor: Paul Arellano, PhD
# Date: November 09, 2025
# -------------------------------------------------------------


# 1. Load Required Libraries
library(spectrolab)
library(stringr)
library(dplyr)
library(tidyr)

# --- Configuration ---
input_path <- "C:/Users/pa589/NAU/TREE_STRESS/FIELDWORK_2025/SPECTRA/Mormon_Lake"
output_dir <- "C:/Users/pa589/NAU/TREE_STRESS/FIELDWORK_2025/SPECTRA/Mormon_Lake/OUTPUT"
output_filename <- "spectra_consolidated_output_FINAL_WORKING.csv"
full_output_path <- file.path(output_dir, output_filename)

# -------------------------------------------------------------

# 2. Define the Conversion Function
asd_to_csv_R <- function(input_path, full_output_path) {
  
  cat("Loading spectra from directory:", input_path, "\n")
  
  # Check and Create Output Directory
  if (!dir.exists(dirname(full_output_path))) {
    dir.create(dirname(full_output_path), recursive = TRUE)
  }
  
  # Load Data using spectrolab::read_spectra()
  tryCatch({
    
    spectra_collection <- spectrolab::read_spectra(
      path = input_path, 
      format = "asd"
    )
    
    if (length(spectra_collection) == 0) {
      stop("0 ASD files were successfully read by spectrolab. Check directory contents.")
    }
    
    cat("Successfully loaded", length(spectra_collection), "spectra.\n")
    
    # --------------------------------------------------------
    # 3. Final Corrected Data Extraction and Tidy
    # --------------------------------------------------------
    
    # FIX 1: Extract the reflectance matrix using the 'value()' function, and transpose it.
    reflectance_matrix <- t(spectrolab::value(spectra_collection)) 
    
    # FINAL FIX: Retrieve the sample names directly from the list element '$names'
    sample_names_vector <- spectra_collection$names 
    
    # Manually set the column names of the matrix (78 rows)
    colnames(reflectance_matrix) <- sample_names_vector
    
    # Convert matrix to a base R data frame
    spectra_df_tidy <- as.data.frame(reflectance_matrix)
    
    # Retrieve the Wavelengths directly from the list element '$bands'
    spectra_df_tidy$Wavelength <- spectra_collection$bands 
    
    # Reorder columns: Wavelength first, then the spectra
    spectra_df_tidy <- spectra_df_tidy %>%
      dplyr::select(Wavelength, everything())
    
  }, error = function(e) {
    message("\n🚨 Process failed during data extraction or loading.")
    stop(paste("Process aborted. Reason:", e$message))
  })
  
  # 4. Export to CSV
  write.csv(spectra_df_tidy, file = full_output_path, row.names = FALSE)
  
  cat("\n✅ Data successfully consolidated and exported to:\n")
  cat("  ", full_output_path, "\n")
  cat("   Exported data shape (rows x columns):", paste(dim(spectra_df_tidy), collapse = " x "), "\n")
}

# --- Execution ---
# Run the function
asd_to_csv_R(input_path, full_output_path)
