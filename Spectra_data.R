################################################################################
# R SCRIPT: ASD SPECTRAL DATA CONSOLIDATOR – MULTI-FOLDER VERSION
#
# This version:
#   1) Iterates over all subfolders in the "Original" directory
#   2) Creates matching subfolders in the "Preprossed" directory
#   3) Runs your ASD → CSV converter for each folder
################################################################################

library(spectrolab)
library(stringr)
library(dplyr)
library(tidyr)

# --------------------------------------------------------------------
# 1. Base directories
# --------------------------------------------------------------------

input_root  <- "C:/Users/pa589/NAU/TREE_STRESS/FIELDWORK_overall/SPECTRAL/2025/Original"
output_root <- "C:/Users/pa589/NAU/TREE_STRESS/FIELDWORK_overall/SPECTRAL/2025/Preprocess"

# --------------------------------------------------------------------
# 2. Your ASD → CSV conversion function (unchanged except for messages)
# --------------------------------------------------------------------

asd_to_csv_R <- function(input_path, output_folder, output_filename) {
  
  full_output_path <- file.path(output_folder, output_filename)
  
  cat("-------------------------------------------------------------\n")
  cat("Processing folder:", input_path, "\n")
  
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  tryCatch({
    spectra_collection <- spectrolab::read_spectra(
      path = input_path,
      format = "asd"
    )
    
    if (length(spectra_collection) == 0) {
      stop("0 ASD files read. Check directory.")
    }
    
    cat("Loaded", length(spectra_collection), "spectra\n")
    
    # Extract reflectance + metadata
    reflectance_matrix <- t(spectrolab::value(spectra_collection))
    sample_names_vector <- spectra_collection$names
    colnames(reflectance_matrix) <- sample_names_vector
    
    spectra_df <- as.data.frame(reflectance_matrix)
    spectra_df$Wavelength <- spectra_collection$bands
    
    spectra_df <- spectra_df %>% select(Wavelength, everything())
    
  }, error = function(e) {
    message("\n🚨 ERROR while processing folder: ", input_path)
    stop(e$message)
  })
  
  write.csv(spectra_df, full_output_path, row.names = FALSE)
  
  cat("Saved CSV →", full_output_path, "\n")
  cat("Rows x columns:", paste(dim(spectra_df), collapse = " x "), "\n")
}

# --------------------------------------------------------------------
# 3. Iterate over ALL subfolders under input_root
# --------------------------------------------------------------------

subfolders <- list.dirs(input_root, full.names = TRUE, recursive = FALSE)

cat("📂 Found", length(subfolders), "subfolders to process.\n")

for (folder in subfolders) {
  
  folder_name <- basename(folder)  # e.g., "Plot_01"
  
  output_folder <- file.path(output_root, folder_name)
  
  output_filename <- paste0(folder_name, "_spectra_consolidated.csv")
  
  asd_to_csv_R(
    input_path = folder,
    output_folder = output_folder,
    output_filename = output_filename
  )
}

cat("\n🎉 ALL FOLDERS PROCESSED SUCCESSFULLY!\n")
