################################################################################
# R SCRIPT: ASD SPECTRAL DATA CONSOLIDATOR (Recursive Version)
#
# PURPOSE:
# Recursively scan a root directory for *all* subfolders that contain .asd files,
# process each folder using spectrolab, and save the processed CSV files into
# a mirrored directory structure under the OUTPUT root directory.
################################################################################

library(spectrolab)
library(stringr)
library(dplyr)
library(tidyr)
library(fs)   # for recursive directory operations

# -------------------------------------------------------------------------
# USER CONFIGURATION
# -------------------------------------------------------------------------

input_root  <- "C:/Users/pa589/NAU/TREE_STRESS/FIELDWORK_overall/SPECTRAL/2024/Original"
output_root <- "C:/Users/pa589/NAU/TREE_STRESS/FIELDWORK_overall/SPECTRAL/2024/Preprocessed"

# -------------------------------------------------------------------------
# FUNCTION: Convert ASD to CSV for one folder
# -------------------------------------------------------------------------

asd_to_csv_R <- function(input_path, output_folder) {
  
  cat("\n----------------------------------------\n")
  cat("Processing folder:", input_path, "\n")
  
  # Create the output folder if needed
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Build output file name
  output_filename <- paste0(basename(input_path), "_spectra_consolidated.csv")
  full_output_path <- file.path(output_folder, output_filename)
  
  # Try reading spectra:
  tryCatch({
    
    spectra_collection <- spectrolab::read_spectra(
      path = input_path,
      format = "asd"
    )
    
    if (length(spectra_collection) == 0) {
      cat("⚠️ No ASD files found here. Skipping.\n")
      return(NULL)
    }
    
    cat("   Loaded:", length(spectra_collection), "ASD spectra\n")
    
    # Extract reflectance matrix
    reflectance_matrix <- t(spectrolab::value(spectra_collection))
    
    sample_names_vector <- spectra_collection$names
    colnames(reflectance_matrix) <- sample_names_vector
    
    spectra_df <- as.data.frame(reflectance_matrix)
    spectra_df$Wavelength <- spectra_collection$bands
    spectra_df <- spectra_df %>% dplyr::select(Wavelength, everything())
    
    # Write CSV
    write.csv(spectra_df, file = full_output_path, row.names = FALSE)
    
    cat("   ✔ Exported:", full_output_path, "\n")
    cat("   ✔ Dimensions:", paste(dim(spectra_df), collapse = " x "), "\n")
    
  }, error = function(e) {
    cat("❌ ERROR processing folder:", input_path, "\n")
    cat("   Reason:", e$message, "\n")
  })
}

# -------------------------------------------------------------------------
# MAIN PROCESS: Walk all subfolders recursively
# -------------------------------------------------------------------------

cat("\nScanning directories recursively under:\n", input_root, "\n")

# Get all subfolders (recursive = TRUE)
all_dirs <- dir_ls(input_root, type = "directory", recurse = TRUE)

# Include root folder itself
all_dirs <- c(input_root, all_dirs)

cat("Found", length(all_dirs), "folders.\n")

for (folder in all_dirs) {
  
  # Check if folder contains ASD files
  asd_files <- dir(pattern = "\\.asd$", path = folder, full.names = TRUE)
  
  if (length(asd_files) > 0) {
    cat("\n📂 ASD files found in:", folder, "\n")
    
    # Build the relative path
    relative_path <- path_rel(folder, start = input_root)
    
    # Build mirrored output folder
    output_folder <- file.path(output_root, relative_path)
    
    # Process folder
    asd_to_csv_R(folder, output_folder)
  }
}

cat("\n\n🎉 ALL PROCESSING COMPLETE!\n")
