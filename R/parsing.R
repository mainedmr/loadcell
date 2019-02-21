#' Load CSVs
#'
#' For an input directory, loads CSVs created by the DMR load cell software into
#' an output class `LoadCellData`.
#'
#' @rdname load_csvs
#' @author Bill DeVoe, \email{William.DeVoe@@maine.gov}
#' @param dir Directory to scan for load cell CSVs. If no directory is provided,
#' the current working directory will be used.
#' @return A class of type `LoadCellData`; class items are named with the CSV
#' filename. Each class item represents a nested list containing the following
#' attributes:
#' \describe{
#' \item{data}{A dataframe of the load cell data contained in the CSV.}
#' \item{sn}{Serial number of the load cell USB adapter.}
#' \item{traps}{The number of traps per trawl, if entered in the interface.}
#' \item{start_dt}{A POSIXlt object representing the start datetime of the CSV.}
#' \item{end_dt}{A POSIXlt object representing the end datetime of the CSV.}
#' \item{seconds}{The number of elapsed seconds in the CSV.}
#' \item{max_load}{An integer representing the maximum load within the CSV.}
#' \item{min_load}{An integer representing the minimum load within the CSV;
#' useful for determining a "zero" value for the weight of the block with no
#' line.}
#' \item{kfactor}{The Keliher conversion factor applied to the data.}
#' }
#' @export
load_csvs <- function(dir = getwd()) {
  # Check that directory exists
  if (!file.exists(dir)) {stop("ERROR: Input directory does not exist.")}
  # List CSVs in directory recursively
  csv_list <- sort(list.files(dir, pattern = ".csv", recursive = T))
  # Blank list to add data for each file into
  data_list <- list()
  # For each CSV
  for (csv in csv_list) {
    # Read CSV, no header; first column is time stamp, second is elapsed
    # seconds, third is load, fourth is raw output
    data <- read.csv(file.path(dir, csv), header = F)
    # Remove factoring
    if (class(data) == "data.frame") {
      f <- sapply(data, is.factor)
      data[f] <- lapply(data[f], as.character)
    }
    # If the CSV has more than four columns, it was not created by the load cell
    # software; skip it
    if (length(colnames(data)) != 4) {
      warning(paste0(csv, " does not contain four columns, skipping."))
      next
    }
    # Assign better column names to dataframe
    names(data) <- c("TimeStamp", "Seconds", "Load", "Raw")
    # Convert load from integer to numeric
    data$Load <- as.numeric(data$Load)
    # Get serial number from filename if available
    sn <- gsub(pattern = ".csv", replacement = "",
                  x = unlist(strsplit(csv, "-", fixed = T))[1],
                  ignore.case = T)
    # Placeholder for load adjustment factor
    kfactor <- 0
    # Get number of traps from filename if available
    traps <- 0
    traps <- try(as.numeric(gsub(pattern = ".csv", replacement = "",
                  x = unlist(strsplit(csv, "-", fixed = T))[4],
                  ignore.case = T)))
    # Start/end date-time as POSIX
    start.dt <- as.POSIXlt(data$TimeStamp[1],
                           format = "%Y-%m-%d-%H:%M:%S",
                           tz = Sys.timezone())
    end.dt <- as.POSIXlt(tail(data$TimeStamp, n=1),
                         format = "%Y-%m-%d-%H:%M:%S",
                         tz = Sys.timezone())
    # File length in minutes
    seconds <- as.numeric(difftime(end.dt, start.dt, units = "secs"))
    # Max/min load for file
    csv.max <- max(data$Load)
    csv.min <- min(data$Load)
    # Add CSV data to list of data
    data_list[[csv]] <- list(data = data, sn = sn, traps = traps,
                             start_dt = start.dt, end_dt = end.dt,
                             seconds = seconds, max_load = csv.max,
                             min_load = csv.min, kfactor = kfactor)
  }
  class(data_list) <- "LoadCellData"
  return(data_list)
}


#' Parse Hauls
#'
#' For an input object of class `LoadCellData`, parses load cell file data into
#' hauls by splitting apart load cell data based on minimum load, time
#' between hauls, and minimum haul length. Return an object of class
#' `LoadCellHauls`.
#'
#' @rdname parse_hauls
#' @author Bill DeVoe, \email{William.DeVoe@@maine.gov}
#' @param data An object of class `LoadCellData` representing data loaded from
#' load cell CSVs.
#' @param split Boolean indicating if CSV files should be split into separate
#' hauls; if a fisherman recorded separate CSVs for each haul, this option would
#' be False and the hauls will be numbered sequentially per day. If True, hauls
#' will be parsed from the CSV data according to options set in the remaining
#' function arguments.
#' @param min_load Numeric - If split is True, load cell data below this
#' threshold will be excluded to create gaps between hauls. If this argument is
#' omitted, the minimum value for each CSV will be used.
#' @param min_time *Optional* Numeric - If split is True, hauls resulting from
#' excluding load values below `min_load` must be this many seconds in duration
#' to be considered a haul. This is useful for excluding junk data produced by a
#'  load cell being jostled in rough seas in between actual hauls.
#' @param min_gap *Optional* Numeric - If split is True, a haul must begin this
#' many seconds after the previous haul to be considered a separate haul.
#' @param pass *Optional* Numeric; if the CSV is less than this many seconds in
#' length, it will not be split and will be assumed to represent a single haul.
#' @return An object of class `LoadCellHauls`. Each item in the class is
#' represented by a unique name constructed by concaternation of the load cell
#' serial number, date, and haul number, ie "SN-2018-03-24-1". Each item has
#' attributes as follows:
#' \describe{
#' \item{haul}{The haul number of the data.}
#' \item{data}{A dataframe containing the load cell data for the haul.}
#' \item{sn}{Serial number of the load cell adapter.}
#' \item{traps}{The number of traps per trawl, if entered in the interface.}
#' \item{start_dt}{A POSIXlt object representing the start datetime of the haul.}
#' \item{end_dt}{A POSIXlt object representing the end datetime of the haul.}
#' \item{seconds}{The number of elapsed seconds in the haul.}
#' \item{max_load}{An integer representing the maximum load within the haul.}
#' \item{kfactor}{The Keliher conversion factor applied to the data.}
#' }
#' @importFrom methods hasArg
#' @export
parse_hauls <- function(data, split, min_load, min_time, min_gap, pass) {
  # Check that input data is of correct class
  if (class(data) != "LoadCellData") {
    stop("Data passed to function parse_hauls must be of class LoadCellData.")
  }
  # Haul incrementer
  haul = 1
  # Date string incrementer
  date_before = ""
  # Empty container for haul data
  haul_list = list()
  # For each CSV item in class
  for (csv in data) {
    # If min_load arg was omitted, default to min_load from CSV
    if (missingArg(min_load)) {
      min_load <- csv$min_load
    }
    # String of the CSV date
    date_str <- strftime(csv$start_dt, format = '%Y-%m-%d')
    # Reset date_before and haul incrementer if date changes
    if (date_str != date_before) {
      date_before <- date_str
      haul = 1
    }
    ### If split == False, simply number CSVs per day
    if (split == F) {
      # Add index column
      csv$data$Index <- 1:nrow(csv$data)
      # Unique identifer for current haul: SN-Date-Haul
      uid <- paste0(csv$sn, "-", date_str, "-", haul)
      # Append data to haul list
      haul_list[[uid]] <- list(haul = haul, data = csv$data, sn = csv$sn,
                               traps = csv$traps, start_dt = csv$start_dt,
                               end_dt = csv$end_dt, seconds = csv$seconds,
                               max_load = max(csv$data$Load),
                               min_load = min(csv$data$Load),
                               kfactor = csv$kfactor)
      # Increment haul number
      haul = haul + 1
      # Next CSV
      next
    }
    ### Now if split == True
    # If pass argument provided, check if CSV is below pass threshold
    if (hasArg(pass)) {
      if (csv$seconds < pass) {
        # Now check if it is below min_time and skip if it is
        if (hasArg(min_time) && csv$seconds < min_time) {next}
        # CSV data will be a single haul
        # Add index column
        csv$data$Index <- 1:nrow(csv$data)
        # Unique identifer for current haul: SN-Date-Haul
        uid <- paste0(csv$sn, "-", date_str, "-", haul)
        # Append data to haul list
        haul_list[[uid]] <- list(haul = haul, data = csv$data, sn = csv$sn,
                                 traps = csv$traps, start_dt = csv$start_dt,
                                 end_dt = csv$end_dt, seconds = csv$seconds,
                                 max_load = max(csv$data$Load),
                                 min_load = min(csv$data$Load),
                                 kfactor = csv$kfactor)
        # Increment haul number
        haul = haul + 1
        # Next CSV
        next
      }
    }
    ### Split is True and CSV was not passed; time to actually split data
    # First remove load data less than min_load
    load_data <- subset(csv$data, Load >= min_load)
    # Add a column for the number of seconds between load values; compare it
    # to min_time and min_gap to increment haul number.
    load_data$TimeDiff <- 0
    load_data$Haul <- 0
    for (i in 1:nrow(load_data)) {
      # If first row, TimeDiff is 0
      if (i == 1) {
        load_data$TimeDiff[i] = 0
        load_data$Haul <- haul
        next
      }
      # Else TimeDiff is difference in seconds to previous row
      last_time <- as.POSIXlt(load_data$TimeStamp[i-1],
                              format = "%Y-%m-%d-%H:%M:%S",
                              tz = Sys.timezone())
      cur_time <- as.POSIXlt(load_data$TimeStamp[i],
                             format = "%Y-%m-%d-%H:%M:%S",
                             tz = Sys.timezone())
      diff <- as.numeric(difftime(cur_time, last_time,
                                  units = "secs"))
      load_data$TimeDiff[i] <- diff
      # If time in sec between rows is greater than min_gap, increment haul
      if (diff >= min_gap) {
        haul = haul + 1
      }
      # Assign haul to row
      load_data$Haul[i] = haul
    }
    ### load_data now has TimeDiff and haul identifiers
    # Split load_data on haul
    hauls <- split(load_data, f = load_data$Haul)
    # Add a column for haul length in seconds
    # Number of hauls skipped due to being less than min_time
    dropped = 0
    for (df in hauls) {
      # Total length of haul in sec
      first_time <- as.POSIXlt(df$TimeStamp[1],
                              format = "%Y-%m-%d-%H:%M:%S",
                              tz = Sys.timezone())
      last_time <- as.POSIXlt(df$TimeStamp[nrow(df)],
                              format = "%Y-%m-%d-%H:%M:%S",
                              tz = Sys.timezone())
      haul_len <- as.numeric(difftime(last_time, first_time,
                                      units = "secs"))
      # If haul length is less than min_time, skip to next and increment dropped
      if (hasArg(min_time) && haul_len < min_time) {
        dropped = dropped + 1
        next
      }
      # Haul number
      cur_haul <- df$Haul[1]
      # Subset just columns that matter
      d <- df[c("TimeStamp", "Seconds", "Load", "Raw")]
      # Adjust haul number based on dropped hauls
      adj_haul <- cur_haul - dropped
      # Add index column
      d$Index <- 1:nrow(d)
      # Unique identifer for current haul: SN-Date-Haul
      uid <- paste0(csv$sn, "-", date_str, "-", adj_haul)
      # Append data to haul list
      haul_list[[uid]] <- list(haul = adj_haul, data = d, sn = csv$sn,
                               traps = csv$traps, start_dt = first_time,
                               end_dt = last_time, seconds = haul_len,
                               max_load = max(df$Load), min_load = min(df$Load),
                               kfactor = csv$kfactor)
    }
  }
  # Assign haul list class of LoadCellHauls
  class(haul_list) <- "LoadCellHauls"
  return(haul_list)
}

#' Parse peaks
#'
#' Searches for peaks and valleys in load data from an object of class
#' `LoadCellHauls`.
#'
#' @rdname parse_peaks
#' @author Bill DeVoe, \email{William.DeVoe@@maine.gov}
#' @param data Object of class `LoadCellHauls`
#' @param span Loess smoothing factor; defaults to .05
#' @param peakdist The minimum distance between peaks in index values
#' @param peakheight The minimum height of peaks in LBF
#' @return An object of class `LoadCellPeaks`, containing all the attributes of
#' object class `LoadCellHauls`, plus the following attributes:
#' \describe{
#' \item{span}{The span used to apply Loess smoothing.}
#' \item{peakdist}{The peak distance parameter used to apply Loess smoothing.}
#' \item{peakheight}{The peak height parameter used to apply Loess smoothing.}
#' \item{peak_analysis}{Boolean indicating if peak analysis was successful; if
#' False, the remaining attributes will not be present.}
#' \item{smoothed_peaks}{A dataframe containing the Index and Load for peaks
#' located in the smoothed data for the top trapcount peaks.}
#' \item{smoothed_valleys}{A dataframe containing the Index and Load for valleys
#' located in the smoothed data corresponding to peaks located.}
#' \item{smoothed}{A dataframe containing the Index and Load for each load
#' after Loess smoothing was applied.}
#' \item{actual_peaks}{A dataframe containing the Index and Load values for
#' actual peaks corresponding to the smoothed peaks located within the load cell
#'  data.}
#' }
#' @import pracma
#' @export
parse_peaks <- function(data, span=.05, peakdist=10, peakheight=200) {
  # Check that input data is of correct class
  if (class(data) != "LoadCellHauls") {
    stop("Data passed to function parse_peaks must be of class LoadCellHauls.")
  }
  # Iterate over hauls
  for (i in 1:length(data)) {
    # Add attributes of attempted smoothing
    data[[i]]$span <- span
    data[[i]]$peakdist <- peakdist
    data[[i]]$peakheight <- peakheight
    # Load cell data for haul
    haul <- data[[i]]$data
    # Number of traps per trawl for haul
    trapcount <- data[[i]]$traps
    # Apply Loess smoothing; this can generate errors if the data is too tight
    # for the given span (ie, if the "haul" is only a few seconds)
    result <- tryCatch({
      smooth <- loess(Load ~ Index, data=haul, span=0.05)
      smooth <- predict(smooth)
    }, error = function(e) e
    )
    if(inherits(result, "error")) {
      message(paste0("Loess smoothing was unsuccessful for haul ",
                     names(data)[i], ". Skipping to next haul..."))
      data[[i]]$peak_analysis <- F
      next
    }
    ## Peak analysis
    # Find peaks and valleys in smoothed data; pracma package needed
    # First find peaks
    peaks_all <- findpeaks(x=smooth,
                           minpeakheight = peakheight,
                           minpeakdist = peakdist,
                           sortstr = FALSE)
    # If no peaks located for given parameters, next haul
    if (is.null(peaks_all)) {
      message(paste0("No peaks found for haul ID ", names(data)[i]))
      data[[i]]$peak_analysis <- F
      next
    }
    # Then find valleys by inverting the smoothed line
    valleys <- findpeaks(x=smooth*-1, minpeakdist = peakdist, sortstr = F)
    # Turn valleys into dataframe and multiply load by -1 to convert peaks back
    # to valley values
    valleys <- data.frame(valleys)
    valleys$Load <- valleys$X1 *-1
    valleys$Index <- valleys$X2
    valleys <- valleys[c("Index", "Load")]
    # Take the trapcount highest peaks/valleys
    # First sort peaks/valleys by load descending
    peaks <- peaks_all[order(-peaks_all[,1]),]
    # NEW WAY: take the last trapcount peaks! gets rid of blips in endline
    # Order by indice descending
    ##peaks <- peaks_all[order(-peaks_all[,2]),]
    # If no peaks present skip
    if (nrow(peaks) == 0) {
      message("No peaks identified, skipping...")
      next
    }
    # Subset top peaks to trapcount number
    if (nrow(peaks) > 0 & nrow(peaks) >= trapcount) {peaks <- peaks[1:trapcount,]}
    if (nrow(peaks) > 0 & nrow(peaks) < trapcount) {peaks <- peaks[1:nrow(peaks),]}
    ## Find nearest peak in actual data from smoothed data peaks
    # Blank dataframe for peak points
    actualpeaks <- data.frame(Index=integer(), Load=double())
    # Iterate over peaks
    for (p in 1:nrow(peaks)) {
      height <- peaks[p,1]
      index <- peaks[p,2]
      # Subset haul data to either side of smoothed peak based
      # on distance to adjacent valleys
      valleybefore <- valleys$Index[which(valleys$Index < index)]
      if (length(valleybefore) > 0){valleybefore <- max(valleybefore)}
      else {valleybefore <- 0}
      valleyafter <- valleys$Index[which(valleys$Index > index)]
      if (length(valleyafter) > 0){valleyafter <- min(valleyafter)}
      else {valleyafter <- length(haul)}
      peakdata <- haul[valleybefore:valleyafter,]
      peak <- peakdata[which.max(peakdata$Load),]
      actualpeaks <- rbind(actualpeaks, peak)
    }
    ## Now add peak analysis results to master data list for haul
    data[[i]]$peak_analysis <- T
    # Convert smoothed peaks to df, cleanup fields
    smoothed_peaks <- data.frame(peaks)
    names(smoothed_peaks) <- c("Load", "Index")
    smoothed_peaks <- smoothed_peaks[c("Index", "Load")]
    data[[i]]$smoothed_peaks <- smoothed_peaks
    # Smoothed valleys
    data[[i]]$smoothed_valleys <- valleys
    # Smoothed line
    data[[i]]$smoothed <- data.frame(Index = seq(1:length(smooth)),
                                     Load = smooth)
    # Actual peaks
    actualpeaks <- actualpeaks[c("Load", "Index")]
    data[[i]]$actual_peaks <- actualpeaks
  }
  class(data) <- "LoadCellPeaks"
  return(data)
}


