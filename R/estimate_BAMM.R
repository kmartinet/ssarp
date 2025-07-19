#' Get tip speciation rates from BAMM (Rabosky 2014) analysis
#'
#' Use the BAMMtools package (Rabosky et al. 2014) to extract tip speciation
#' rates from user-supplied BAMM analysis objects.
#'
#' @param label_type Either "epithet" or "binomial" (default): describes the
#' type of tip label in the tree used for the BAMM analysis. If "epithet," only
#' the species epithet will be used to match speciation rates to tips in the
#' returned occurrence dataframe. If "binomial," the full species name
#' (including genus) will be used to match speciation rates to tips in the
#' returned occurrence dataframe.
#' @param occurrences The occurrence record dataframe output from the ssarp
#' pipeline. If you would like to use a custom dataframe, please make sure that
#' there are columns titled "genericName" and "specificEpithet"
#' @param edata The eventdata object created by using the
#' `BAMMtools::getEventData()` function
#' @return A dataframe that includes speciation rates for each species in the
#' occurrence record dataframe
#' @references
#' - Rabosky, D.L. (2014). Automatic Detection of Key Innovations, Rate Shifts,
#' and Diversity-Dependence on Phylogenetic Trees. PLOS ONE, 9(2): e89543.
#' - Rabosky, D.L., Grundler, M., Anderson, C., Title, P., Shi, J.J.,
#' Brown, J.W., Huang, H., & Larson, J.G. (2014), BAMMtools: an R package for
#' the analysis of evolutionary dynamics on phylogenetic trees. Methods in
#' Ecology and Evolution, 5: 701-707.
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
#'
#' # Read tree from Patton et al. (2021), trimmed to Caribbean species
#' tree <- ape::read.tree(system.file("extdata",
#'                              "Patton_Anolis_trimmed.tree",
#'                              package = "ssarp"))
#'
#' # Event data file from an external BAMM run
#' event_data <- system.file("extdata",
#'                           "event_data_Patton_Anolis.txt",
#'                            package = "ssarp")
#'
#' edata <- BAMMtools::getEventData(phy = tree, eventdata = event_data)
#'
#' occ_speciation <- estimate_BAMM(label_type = "epithet",
#'                                  occurrences = areas ,
#'                                  edata = edata)
#' @export

estimate_BAMM <- function(label_type = "binomial", occurrences, edata) {
  # Checkmate input validation
  checkmate::assertString(label_type)
  checkmate::assertDataFrame(occurrences)
  checkmate::testSubset(c("specificEpithet", "genericName"), names(occurrences))
  # Ensure columns are correct type
  checkmate::assertCharacter(occurrences$specificEpithet)
  checkmate::assertCharacter(occurrences$genericName)

  # Create a named number vector of speciation rates
  speciation_rates <- edata$meanTipLambda
  names(speciation_rates) <- edata$tip.label

  # For each Species name in speciation_rates, look for that name in the
  #  Species column of the occurrence record dataframe and add the
  #  appropriate rate

  # Initialize a rate column with NAs
  occurrences <- occurrences |> dplyr::mutate(rate = NA)

  # If the user specified label_type = "epithet"
  if (label_type == "epithet") {
    # For each name in speciation_rates, look for that name in the Species
    #  column of the occurrence record dataframe and add the appropriate rate
    for (i in names(speciation_rates)) {
      occurrences <- occurrences |>
        dplyr::mutate(
          rate = dplyr::case_when(
            grepl(i, specificEpithet) ~ speciation_rates[[i]],
            TRUE ~ rate # Keeps values for non-matching rows
          )
        )
    }
  } else if (label_type == "binomial") {
    # The occurrence record dataframe has separate "Genus" and "Species"
    #  columns, so they should be combined for this label type
    occurrences$Binomial <- paste(
      occurrences$genericName,
      occurrences$specificEpithet,
      sep = " "
    )
    # Now, for each name in speciation_rates, look for that name in the Binomial
    #  column of the occurrence record dataframe and add the appropriate rate
    for (i in names(speciation_rates)) {
      occurrences <- occurrences |>
        dplyr::mutate(
          rate = dplyr::case_when(
            grepl(i, Binomial) ~ speciation_rates[[i]],
            TRUE ~ rate # Keeps values for non-matching rows
          )
        )
    }
  }

  # Remove rows with NA in rate column
  occurrences <- occurrences[!is.na(occurrences$rate), ]

  return(occurrences)
}
