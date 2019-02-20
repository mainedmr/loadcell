#' Plot Hauls
#'
#' For an input object of class `LoadCellPeaks`, creates a plot for each haul
#' containing the load cell data, smoothed data, and peak analysis.
#'
#' @rdname plot_hauls
#' @author Bill DeVoe, \email{William.DeVoe@@maine.gov}
#' @param data An object of class `LoadCellPeaks`.`
#' @param fileout *Optional* - Filename to save plots into. A PDF with this name
#' will be created in the current working directory. If the file exists, it will
#' be overwritten.
#' @return Boolean True if successful. If a fileout is provided, a PDF will be
#' created containing the plots. If the fileout argument is omitted, plots will
#' be output to the console/plot viewer.
#' @export
#' @import ggplot2
#' @import grDevices
#' @importFrom methods hasArg
plot_hauls <- function(data, fileout) {
  # Check that input data is of correct class
  if (class(data) != "LoadCellPeaks") {
    stop("Data passed to function plot_hauls must be of class LoadCellPeaks.")
  }
  # Open PDF file to write
  if (hasArg(fileout)) {
    pdf(file=paste0(fileout,".pdf"), width = 10, height = 7, onefile=T)
  }
  # Iterate over each haul
  for (i in 1:length(data)){
    # Need to use local scope for creating plots
    local({
      # Reassign i to local variable
      i <- i
      # Build a title for the plot
      title <- paste0("UID: ", names(data)[[i]], " Haul: ", data[[i]]$haul, "\n",
                      "Haul Start: ", strftime(data[[i]]$start_dt),
                      " Haul End: ", strftime(data[[i]]$end_dt), "\n",
                      "Length (min): ", round(data[[i]]$seconds/60, 1),
                      " Max Load: ", data[[i]]$max_load,
                      " Traps/Trawl: ", data[[i]]$traps,
                      " K-Factor: " , data[[i]]$kfactor, "\n"
                      )
      if (data[[i]]$peak_analysis) {
        title <- paste0(title, "Span: ", data[[i]]$span,
                        " Peak Distance: ", data[[i]]$peakdist,
                        " Peak Height: ", data[[i]]$peakheight)
      }
      # Build the plot
      p <- ggplot(data = data[[i]]$data) +
        ggtitle(title) +
        aes(x = Index, y = Load) +
        geom_line(color = "#0c4c8a", show.legend = T) +
        labs(x = "Time (Sample Index)",
             y = "Load (LBF)") +
        theme_dark()
      # If the peak analysis was successful, plot peak data
      if (data[[i]]$peak_analysis) {
        p <- p +
        # Smooth line
        geom_line(mapping = aes(x = Index, y = Load),
                    data = data[[i]]$smoothed,
                    col="blue",
                    show.legend = T) +
        # Smoothed peaks
        geom_point(mapping = aes(x = Index, y = Load),
                    data = data[[i]]$smoothed_peaks,
                    col="black",
                    show.legend = T) +
        # Actual peaks
        geom_point(mapping = aes(x = Index, y = Load),
                    data = data[[i]]$actual_peaks,
                    col="red",
                    show.legend = T) +
        geom_text(mapping = aes(x = Index, y = Load, label = Load),
                    vjust = -1,
                    data = data[[i]]$actual_peaks,
                    col="black",
                    show.legend = T)
      }
      print(p)
    })
  }
  # Close PDF
  if (hasArg(fileout)) {
    dev.off()
  }
  return(T)
}
