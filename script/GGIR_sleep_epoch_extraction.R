# ------------------------------------------------------------------------------
#   DESCRIPTION:
#     This 
#   AUTHOR: Pin-Wei Chen 
#     Children's Hospital of Philadelphia
#     chenp7@chop.edu
#     Github: https://github.com/pinweichen
#
#   DATE CREATED: 2025-01-30
#   LAST MODIFIED:2025-07-30
# ------------------------------------------------------------------------------

# Ensure you have these installed: install.packages(c("data.table", "tidyverse"))
library(data.table)
library(tidyverse)
rm(list=ls())


TIMEZONE <- "America/New_York"
# The path should lead to "output_XXX" folder of GGIR
GGIR_output_path <- "/path/to/your/ggir_folder/"
Extracted_output_path <- "/path/to/your/extracted_epochs"

#' Generate a sequence of timestamps for a given period.
#'
#' @param start_datetime The starting timestamp (POSIXct).
#' @param end_datetime The ending timestamp (POSIXct).
#' @param epoch_sec The epoch length in seconds (default is 30).
#' @return A data.table with a single column 'epoch_time'.
generate_epoch_table <- function(start_datetime, end_datetime, epoch_sec = 30) {
  # Input validation
  if (!is.POSIXct(start_datetime) || !is.POSIXct(end_datetime)) {
    stop("Start and end times must be POSIXct objects.")
  }
  
  # Generate a sequence of timestamps from start to end by the epoch length
  epoch_sequence <- seq(from = start_datetime, to = end_datetime, by = paste(epoch_sec, "sec"))
  
  # Create a data.table with the sequence
  epoch_table <- data.table(epoch_time = epoch_sequence)
  
  return(epoch_table)
}


#' Extract Sleep/Wake Epochs from GGIR Results
#'
#' This function processes the output of a GGIR analysis to generate a time-series
#' CSV file for each participant, labeling each 30-second epoch as "Sleep" or "Wake"
#' based on the SIB (sustained inactivity bout) classification.
#'
#' @param ggir_output_path A string path to the main GGIR output directory. This
#'   directory should contain the 'meta' folder.
#' @param output_path A string path to the directory where the output CSV files
#'   will be saved. The directory will be created if it doesn't exist.
#' @param timezone A valid timezone string (e.g., "America/New_York") to correctly
#'   interpret timestamps. See `OlsonNames()` for a list of valid timezones.
#' @return This function does not return a value. It writes one CSV file per
#'   participant to the specified `output_path`.
#'
extract_ggir_sleep_epochs <- function(ggir_output_path, output_path, timezone) {
  
  # --- 1. Input Validation and Setup ---
  
  # Check if the provided timezone is valid
  if (!timezone %in% OlsonNames()) {
    stop("Invalid timezone provided. See `OlsonNames()` for a list of valid timezones.")
  }
  
  # Define paths to required GGIR subdirectories
  ms3_path <- file.path(ggir_output_path, "meta", "ms3.out")
  basic_path <- file.path(ggir_output_path, "meta", "basic")
  
  # Check if required directories exist
  if (!dir.exists(ggir_output_path)) stop("GGIR output path not found: ", ggir_output_path)
  if (!dir.exists(ms3_path)) stop("Required subdirectory not found: ", ms3_path)
  if (!dir.exists(basic_path)) stop("Required subdirectory not found: ", basic_path)
  
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_path)) {
    message("Output directory not found. Creating it now: ", output_path)
    dir.create(output_path, recursive = TRUE)
  }
  
  # Get the list of GGIR part 3 output files
  file_list <- list.files(ms3_path)
  
  if (length(file_list) == 0) {
    warning("No files found in ", ms3_path, ". Nothing to process.")
    return(invisible(NULL))
  }
  
  # --- 2. Main Processing Loop ---
  
  for (current_file in file_list) {
    
    # Extract participant ID from the filename
    id <- word(current_file, 1, sep = ".bin")
    cat("\nProcessing participant:", id, "...\n")
    
    # Define full paths to the required RData files
    ms3_file_path <- file.path(ms3_path, current_file)
    basic_file_path <- file.path(basic_path, paste0("meta_", current_file))
    
    # Error handling for each file
    tryCatch({
      
      # --- 2a. Load Data ---
      
      # Check for file existence before loading
      if (!file.exists(basic_file_path)) stop("Basic meta file not found.")
      if (!file.exists(ms3_file_path)) stop("MS3 file not found.")
      
      load(basic_file_path)   # Should load 'M'
      load(ms3_file_path)     # Should load 'sib.cla.sum'
      
      # Verify that the expected objects were loaded
      if (!exists("M")) stop("'M' object not found in the basic meta file.")
      if (!exists("sib.cla.sum")) stop("'sib.cla.sum' object not found in the MS3 file.")
      
      # --- 2b. Prepare Data ---
      # Convert sleep data to a data.table and parse timestamps
      setDT(sib.cla.sum)
      sib.cla.sum[, `:=`(
        ts_start = as_datetime(sib.onset.time, tz = timezone),
        ts_end = as_datetime(sib.end.time, tz = timezone)
      )]
      
      # Extract recording start and end times
      long_epoch <- M$metalong
      setDT(long_epoch)
      
      # Generate a complete timeline of 30-second epochs for the recording period
      epoch_rows <- generate_epoch_table(
        start_datetime = min(as_datetime(long_epoch$timestamp, tz = timezone)),
        end_datetime = max(as_datetime(long_epoch$timestamp, tz = timezone))
      )
      epoch_rows[, id := id]
      
      # --- 2c. Label Epochs (Efficiently) ---
      
      # Initialize all epochs to "Wake"
      epoch_rows[, GGIR_label := "Wake"]
      # Write Sleep periods
      epoch_rows[sib.cla.sum, on = .(epoch_time >= ts_start, epoch_time < ts_end), GGIR_label := "Sleep"]
      
      # --- 2d. Save Output ---
      output_filename <- file.path(output_path, paste0(id, "_GGIR_epochs.csv"))
      fwrite(epoch_rows, output_filename)
      cat("  -> Successfully saved GGIR epochs to:", output_filename, "\n")
      
      # Clean up variables from the loaded files to prevent conflicts in the next loop
      rm(sib.cla.sum, M, long_epoch)
      
    }, error = function(e) {
      # If an error occurs for one file, report it and continue to the next
      cat("  -> ERROR processing", id, ":", e$message, "\n")
    })
  }
  cat("\nProcessing complete.\n")
}


# Use the function to extract data. This will output epochs into a csv file named "ID_GGIR_epochs.csv"
extract_ggir_sleep_epochs(GGIR_output_path,Extracted_output_path, timezone = TIMEZONE)

# This data will contain all your subjects sleep-wake epochs. 
# The sib.cla.sum will load sleep periods determined by the sleep algorithm
# You can combine this data with the PSG labels and run epoch by epoch analysis after.
