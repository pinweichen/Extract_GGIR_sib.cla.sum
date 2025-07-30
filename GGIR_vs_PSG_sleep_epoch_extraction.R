library(data.table)
library(tidyverse)
rm(list=ls())


OS_p <- "/Volumes/"
#OS_p <- "/mnt/isilon/"
general <- file.path(OS_p, "chps_digital_health_core_general/Benny_Actigraphy/AGV_GGIR_paper/Data/results/") 
general_sgrow2 <- file.path(OS_p, "SCRM_Study/AGV_Arcus_Format/")

test_name_ls <- c("GGIR_300_AGV_vanHees_Final","GGIR_300_AGV_CK_Final","GGIR_300_AGV_Sadeh_Final")
# general = "/Volumes/SCRM_Study/AGV_Arcus_Format/"
# sandbox_p = paste0(general, "Research\ Administration/Sandbox_Benny/")
test_name <- test_name_ls[1]
  output_sub1 = paste0(general, "GGIR/",test_name,"/")
  output_label = paste0(general, "GGIR/",test_name,"/Epoch_b_Epoch/")
  metadatadir <- file.path(output_sub1,list.files(output_sub1, pattern = "output_")[1])
  
  GGIR_output_path <- metadatadir

  
# The path should lead to "output_XXX" folder of GGIR
GGIR_output_path <- ""
Extracted_output_path <- ""

timezone <- "America/New_York"
# The function will extract the sleep wake epochs extracted from GGIR processing part 3.
# The 


# Helper function ==================
#' Generate a data.table of 30-second epochs
#'
#' @param start_datetime A POSIXct object or character string for the start time.
#' @param end_datetime A POSIXct object or character string for the end time.
#' @return A data.table with a single column 'epoch_time' containing the 30-second intervals.

generate_epoch_table <- function(start_datetime, end_datetime) {
  # Convert inputs to POSIXct objects to ensure correct handling
  start_dt <- as.POSIXct(start_datetime)
  end_dt <- as.POSIXct(end_datetime)
  
  # Generate the sequence of timestamps from start to end by 30 seconds
  timestamps <- seq(from = start_dt, to = end_dt, by = "30 sec")
  
  # Create a data.table with the generated timestamps
  epoch_dt <- data.table(epoch_time = timestamps)
  
  return(epoch_dt)
}

extract_sib.cla.sum <- function(GGIR_output_path, timezone) {
  #Create output label
  ls_id <- list.files(file.path(GGIR_output_path, "meta/ms3.out/"))

  for ( n in 1:length(ls_id)) {
    id = word(ls_id[n],1,sep = ".bin")
    fname = ls_id[n]
    # Load meta folder data
    load(file = file.path(metadatadir, "/meta/basic",paste0("meta_",fname)))
    load(file = file.path(metadatadir, "/meta/ms3.out", fname))
    # load the sleep algorithm results
    setDT(sib.cla.sum)
    sib.cla.sum$ts_start <- as_datetime(sib.cla.sum$sib.onset.time, tz = timezone)
    sib.cla.sum$ts_end <- as_datetime(sib.cla.sum$sib.end.time, tz = timezone)
    # save the end of the recording time
    # Extract the timestamp of end of recording
    long_epoch <- M$metalong
    long_epoch$timestamp <- as_datetime(long_epoch$timestamp, tz = timezone)
    epoch_rows <- generate_epoch_table(start_datetime = min(long_epoch$timestamp),end_datetime = max(long_epoch$timestamp))
    epoch_rows[,id := id]
    # Find ts_start (i.e., sleep time) then label colnames of ActiSleep in psg_dt accordingly
    for (shi in 1:nrow(sib.cla.sum)) {
      epoch_rows[epoch_time >= sib.cla.sum[shi,ts_start] & epoch_time <sib.cla.sum[shi,ts_end], GGIR_label := "Sleep"]
    }
    epoch_rows[GGIR_label %in% NA, GGIR_label := "Wake"]
    fwrite(epoch_rows,file.path(Extracted_output_path, paste0(id,"_GGIR_epochs.csv")))
    cat("Successfully saved GGIR epochs for ", id)
  }
}

# Use the function to extract data. This will output epochs into a csv file named "ID_GGIR_epochs.csv"
dt_all <- extract_sib.cla.sum(GGIR_output_path, timezone = timezone)


# This data will contain all your subjects sleep-wake epochs in 15 seconds. 
# The data extract will contain date and times ever recorded for that subject.
# You can cut this data by the time start and end of your PSG data for each subject.
# Then use the de Zombodie algorithm to analyze epoch by epoch results.