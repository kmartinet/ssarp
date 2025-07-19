#' Remove continents from area dataframe.
#'
#' Reference a list of continental areas to remove them from the dataframe
#' output by `ssarp::find_areas()`.
#' @param occs The dataframe that is returned by `ssarp::find_areas()`. I do not
#' recommend using a custom dataframe for this function because it references
#' areas given by the area database used in `ssarp::find_areas()`. If you must
#' use a custom dataframe, please ensure  that the landmass areas are in a
#' column called "areas"
#' @return A dataframe of the species name, island name, and island area
#' (without continents)
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
#' new_areas <- remove_continents(areas)
#'
#' @export

remove_continents <- function(occs) {
  # Checkmate input validation
  checkmate::assertDataFrame(occs)
  checkmate::testSubset("areas", names(occs))
  # Ensure columns are correct type
  checkmate::assertNumeric(occs$areas)

  # List of continental areas that could be added through the use of
  #  ssarp::find_areas()
  continents <- c(5.50e13, 3.04e13, 1.78e13, 2.47e13)
  for (i in seq(continents)) {
    exclude <- which(occs[, "areas"] == continents[i])
    if (length(exclude) != 0) {
      occs <- occs[-exclude, ]
    }
  }

  return(occs)
}
