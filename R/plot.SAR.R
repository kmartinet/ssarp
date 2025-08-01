#' Plot a species-area relationship
#'
#' Function for plotting species-area relationship objects from the
#' `ssarp::create_SAR()` function
#' @param x The SAR object that will be plotted
#' @param ... Parameters to pass to plot()
#' @return A plot of your species-area relationship
#' @examples
#' # The GBIF key for the Anolis genus is 8782549
#' # Read in example dataset filtered from:
#' #  dat <- rgbif::occ_search(taxonKey = 8782549,
#' #                           hasCoordinate = TRUE,
#' #                           limit = 10000)
#' dat <- read.csv(system.file("extdata",
#'                             "ssarp_Example_Dat.csv",
#'                             package = "ssarp"))
#' occs <- find_land(occurrences = dat)
#' areas <- find_areas(occs = occs)
#' seg <- create_SAR(areas, npsi = 0)
#' plot(seg)
#'
#' @export

plot.SAR <- function(x, ...) {
  # Record x and y min/max for plot limits
  dat <- x[["aggDF"]]

  x_max <- max(dat$x)
  x_min <- min(dat$x)
  y_max <- max(dat$y)
  y_min <- min(dat$y)

  # Segmented plotting
  if (!is.null(x[["segObj"]])) {
    segmented::plot.segmented(
      x[["segObj"]],
      rug = FALSE,
      xlim = c(x_min, (x_max + 0.5)),
      ylim = c(y_min, (y_max + 0.5)),
      ylab = "Log Number of Species",
      xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
      main = "Species-Area Relationship"
    )
    # Add the points
    graphics::points(x[["aggDF"]]$x, x[["aggDF"]]$y, pch = 19)
  } else if (!is.null(x[["linObj"]])) {
    # Line plotting
    plot(
      x[["aggDF"]],
      xlim = c(x_min, (x_max + 0.5)),
      ylim = c(y_min, (y_max + 0.5)),
      ylab = "Log Number of Species",
      xlab = expression(paste("Log Island Area (", "m"^"2", ")")),
      main = "Species-Area Relationship",
      pch = 16
    )
    graphics::abline(x[["linObj"]])
  }
}
