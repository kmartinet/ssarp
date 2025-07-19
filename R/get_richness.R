#' Create a species richness dataframe for a given occurrence record dataframe
#'
#' Use a dataframe output by `ssarp::find_areas()` to determine how many
#' species occur on each island by creating a species richness dataframe.
#'
#' The output of this function can be used directly with
#' [the sars R package](https://txm676.github.io/sars/articles/sars-r-package.html)
#' to fit additional SAR models that `ssarp` does not create itself.
#'
#' @param occs The dataframe output by `ssarp::find_areas()`, or if using a
#' custom dataframe, ensure that it has the following named columns:
#' - "areas" containing the areas associated with the land masses of interest
#' - "specificEpithet" containing the names of the species living on those
#' islands
#' @return A dataframe with two columns: the first containing island areas and
#' the second containing the associated species richness (number of unique
#' species)
#' @examples
#' # The GBIF key for the Anolis genus is 8782549
#' # Read in example dataset filtered from:
#' #  dat <- rgbif::occ_search(taxonKey = 8782549,
#' #                           hasCoordinate = TRUE,
#' #                           limit = 10000)
#' dat <- read.csv(system.file("extdata",
#'                             "ssarp_Example_Dat.csv",
#'                             package = "ssarp"))
#' land <- find_land(occurrences = dat)
#' areas <- find_areas(occs = land)
#' richness <- get_richness(occs = areas)
#'
#' @export

get_richness <- function(occs) {
  # Checkmate input validation
  checkmate::assertDataFrame(occs)
  checkmate::testSubset(
    c("specificEpithet", "areas"),
    names(occs)
  )
  # Ensure columns are correct type
  checkmate::assertCharacter(occs$specificEpithet)
  checkmate::assertNumeric(occs$areas)

  #   formula Species ~ Area means to group scientific names by area
  #   function(x) length(unique(x)) tells it to give me the number of unique
  #     species for each unique island area
  agg <- stats::aggregate(
    data = occs,
    specificEpithet ~ areas,
    function(x) length(unique(x))
  )

  colnames(agg) <- c("Area", "Richness")

  return(agg)
}
