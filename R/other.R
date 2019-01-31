#' Load to tension
#'
#' Calculates the tension on a line travelling through a block based on the load
#' on the block and included angle of the line over the block.
#'
#' @rdname adjust_load
#' @author Bill DeVoe, \email{William.DeVoe@@maine.gov}
#' @param load Numeric vector of load value or values to adjust.
#' @param angle The included angle across the block in degrees.
#' @return A numeric vector representing the adjusted load value.
adjust_load <- function(load, angle) {
  # TODO: Figure this out and then export function
  message("This function is yet to be implemented.")
  return(load)
}


#' Export load cell data
#'
#' For an input object of class `LoadCellHauls` or `LoadCellPeaks`, flattens the
#'  object data into CSVs of data. Optionally, a prefix can be given to be added
#'  to the filename. The following CSVs are created:
#' \describe{
#' \item{prefix-lc_data.csv}{The load cell data, with UID and haul numbers
#' added.}
#' \item{prefix-lc_haul.csv}{The haul attributes, including the settings used
#' for peak analysis. UID is included to relate hauls to load cell data.}
#' \item{prefix-lc_peaks.csv}{The actual peak values for each haul, only if peak
#'  analysis was successful for the haul. Includes UID and haul number.}
#' }
#'
#' @rdname export_loadcell
#' @author Bill DeVoe, \email{William.DeVoe@@maine.gov}
#' @param data An object of class `LoadCellHauls` or `LoadCellPeaks`
#' @param prefix *Optional* A prefix to append to the filenames of the
#' output CSVs.
#' @return Boolean True if successful.
#' @export
#' @importFrom methods hasArg
export_loadcell <- function(data, prefix) {
  # Check that data is of correct class
  classes <- c("LoadCellHauls", "LoadCellPeaks")
  if (!(class(data) %in% classes)) {
    stop("Input data to function 'export_loadcell' must be of class
    LoadCellHauls or LoadCellPeaks")
  }
  ### Both classes have these attributes for export
  # Filename and blank df for load cell data
  datafn <- ifelse(hasArg(prefix),
                   paste0(prefix,"-lc_data.csv"),
                   "lc_data.csv")
  lc_data <- data.frame()
  # Filename and blank df for haul data
  haulfn <- ifelse(hasArg(prefix),
                   paste0(prefix,"-lc_hauls.csv"),
                   "lc_hauls.csv")
  lc_hauls <- data.frame()
  # Filename and blank df for actual peak data
  peakfn <- ifelse(hasArg(prefix),
                   paste0(prefix,"-lc_peaks.csv"),
                   "lc_peaks.csv")
  lc_peaks <- data.frame()
  # For each haul in data object
  for (i in 1:length(data)) {
    # UID of haul
    uid <- names(data)[i]
    ### First extract load cell data
    haul_cell_data <- data[[i]]$data
    # Add a column for UID and haul
    haul_cell_data$UID <- uid
    haul_cell_data$haul <- data[[i]]$haul
    # Bind cell data for haul to df of all cell data
    lc_data <- rbind(lc_data, haul_cell_data)
    ### Make DF of haul data
    haul_data <- data.frame(uid = uid,
                            haul = data[[i]]$haul,
                            sn = data[[i]]$sn,
                            traps = data[[i]]$traps,
                            start_dt = data[[i]]$start_dt,
                            end_dt = data[[i]]$end_dt,
                            seconds = data[[i]]$seconds,
                            max_load = data[[i]]$max_load,
                            min_load = data[[i]]$min_load)
    # If data is of class LoadCellPeaks, also add the peak analysis attributes
    if (class(data) == "LoadCellPeaks") {
      haul_data$span <- data[[i]]$span
      haul_data$peakdist <- data[[i]]$peakdist
      haul_data$peakheight <- data[[i]]$peakheight
      haul_data$peak_analysis <- data[[i]]$peak_analysis
    }
    # If the peak analysis was successful, parse the actual peak df; add UID
    # to relate it to the haul data
    if (class(data) == "LoadCellPeaks" && data[[i]]$peak_analysis) {
      haul_peaks <- data[[i]]$actual_peaks
      haul_peaks$UID <- uid
      haul_peaks$Haul <- data[[i]]$haul
      # Bind peak data to master peak df
      lc_peaks <- rbind(lc_peaks, haul_peaks)
    }
    # Bind haul df to master haul df
    lc_hauls <- rbind(lc_hauls, haul_data)
  }
  ## Export dfs to CSVs
  write.csv(lc_data, datafn, row.names = F)
  write.csv(lc_hauls, haulfn, row.names = F)
  if (nrow(lc_peaks) > 0) {
    write.csv(lc_peaks, peakfn, row.names = F)
  }
  return(T)
}



