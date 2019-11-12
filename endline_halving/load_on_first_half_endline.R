library(tidyverse)
# Factoring off
options(stringsAsFactors = F)
# Set working dir to where current script is saved (or set it to where the data is)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Inputs
# CSV file of all hauls with endline index/load - needs cols "uid",
# "max_endline_load" and "endline_index"
haul_csv <- "haul_data.csv"
# CSV file of load cell data produced by loadcell package
data_csv <- "lc_data.csv"
# Name for modified output CSV of hauls
output_csv <- "hauls_half_el.csv"

### Main script
# Load data, clean column names to lowercase _ delimited
hauls <- readr::read_csv(haul_csv) %>%
  janitor::clean_names()
data <- readr::read_csv(data_csv) %>%
  janitor::clean_names()
# Check that provided column names are in haul data
for (col in c("uid", "max_endline_load", "endline_index")) {
  stopifnot(col %in% colnames(hauls))
}
# Check that data has right cols
for (col in c("uid", "load", "index")) {
  stopifnot(col %in% colnames(data))
}

# Blank dataframe for max load on first half of endline
el_halfs <- data.frame(uid = character(),
                       first_half_el_load = numeric(),
                       first_half_el_index = numeric(),
                       comment = character())
# For each haul
for (row in 1:nrow(hauls)) {
  # Values for haul
  uid <- hauls$uid[row]
  load <- hauls$max_endline_load[row]
  index <- hauls$endline_index[row]
  # Subset data for haul
  haul_data <- data[data$uid == uid,]
  # Check that data exists for haul
  if (nrow(haul_data) == 0) {
    comment <- paste0("Haul ", uid, " has no matching data...")
    half_load <- NA
    half_index <- NA
    message(comment)
  } else { # Haul has data
    # Subset haul data to less than endline index
    el_data <- haul_data[haul_data$index <= index,]
    # Check that provided endline load from haul file is reasonably close to
    # load at same index position in data
    load2 <- el_data[el_data$index <= index,]$load[1]
    if (abs(load - load2) > 3) {
      comment <- paste0("Haul ", uid, " has potential load/index mismatch...")
      message(comment)
    } else (comment <- "")
    # Now calculate max load for the first half of the endline
    half_data <- el_data[el_data$endline_index <= index/2,]
    half_load <- max(half_data$load)
    half_index <- half_data[half_data$load == half_load,]$index[1]
  }
  # Save results in el_halfs dataframe
  df <- data.frame(uid = uid,
                   first_half_el_load = half_load,
                   first_half_el_index = half_index,
                   comment = comment)
  el_halfs <- rbind(el_halfs, df)
}
# Join el_halfs to original haul data by uid
hauls_final <- dplyr::left_join(hauls, el_halfs, by = "uid")
# Write to file
readr::write_csv(hauls_final, output_csv)
