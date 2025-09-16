#' Get speciation rates using the DR statistic (Jetz et al. 2012)
#'
#' DR stands for “diversification rate,” but it is ultimately a better
#' estimation of speciation rate than net diversification (Belmaker and Jetz
#' 2015; Quintero and Jetz 2018) and returns results similar to BAMM’s tip
#' speciation rate estimations (Title and Rabosky 2019).
#'
#' This function uses methodology from Sun and Folk (2020) to calculate the DR
#' statistic for each tip of a given tree and output a dataframe for use in
#' ssarp's speciation-area relationship pipeline. This method also removes any
#' species rows without rates (this is most likely to occur when the tree does
#' not have all of the species included in the occurrence record dataframe).
#' The tree read in the examples below is modified from Patton et al. (2021).
#' @param tree The dated phylogenetic tree that corresponds with the taxa to be
#' included in a speciation-area relationship
#' @param label_type Either "epithet" or "binomial" (default): describes the
#' type of tip label in the provided tree. If "epithet," only the species
#' epithet will be used to match speciation rates to tips in the returned
#' occurrence dataframe. If "binomial," the full species name (including genus)
#' will be used to match speciation rates to tips in the returned occurrence
#' dataframe.
#' @param occurrences The occurrence record dataframe output from the ssarp
#' pipeline. If you would like to use a custom dataframe, please make sure that
#' there are columns titled "genericName" and "specificEpithet"
#' @return A dataframe that includes speciation rates for each species in the
#' occurrence record dataframe
#' @references
#' - Belmaker, J., & Jetz, W. (2015). Relative roles of ecological and
#' energetic constraints, diversification rates and region history on global
#' species richness gradients. Ecology Letters, 18: 563–571.
#' - Jetz, W., Thomas, G.H, Joy, J.B., Harmann, K., & Mooers, A.O. (2012). The
#' global diversity of birds in space and time. Nature, 491: 444-448.
#' - Patton, A.H., Harmon, L.J., del Rosario Castañeda, M.,
#' Frank, H.K., Donihue, C.M., Herrel, A., & Losos, J.B. (2021). When adaptive
#' radiations collide: Different evolutionary trajectories between and within
#' island and mainland lizard clades. PNAS, 118(42): e2024451118.
#' - Quintero, I., & Jetz, W. (2018). Global elevational diversity and
#' diversification of birds. Nature, 555, 246–250.
#' - Sun, M. & Folk, R.A. (2020). Cactusolo/rosid_NCOMMS-19-37964-T: Code and
#' data for rosid_NCOMMS-19-37964 (Version V.1.0). Zenodo.
#'  http://doi.org/10.5281/zenodo.3843441
#' - Title P.O. & Rabosky D.L. (2019). Tip rates, phylogenies and
#' diversification: What are we estimating, and how good are the estimates?
#' Methods in Ecology and Evolution. 10: 821–834.
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
#'                                    "Patton_Anolis_trimmed.tree",
#'                                    package = "ssarp"))
#'
#' occ_speciation <- estimate_dr(tree = tree,
#'                               label_type = "epithet",
#'                               occurrences = areas)
#'
#' @export

estimate_dr <- function(tree, label_type = "binomial", occurrences) {
  # Checkmate input validation
  checkmate::assertString(label_type)
  checkmate::assertDataFrame(occurrences)
  checkmate::testSubset(c("specificEpithet", "genericName"), names(occurrences))
  # Ensure columns are correct type
  checkmate::assertCharacter(occurrences$specificEpithet)
  checkmate::assertCharacter(occurrences$genericName)

  # Use DR statistic methodology from Sun and Folk (2020) to obtain per-tip
  #  DR stats
  # Adapted from the "DR_statistic.R" file (see References section in the
  #  estimate_DR documentation)
  rootnode <- length(tree$tip.label) + 1
  speciation_rates <- numeric(length(tree$tip.label))
  for (i in seq_along(speciation_rates)) {
    node <- i
    index <- 1
    qx <- 0
    while (node != rootnode) {
      el <- tree$edge.length[tree$edge[, 2] == node]
      node <- tree$edge[, 1][tree$edge[, 2] == node]
      qx <- qx + el * (1 / 2^(index - 1))
      index <- index + 1
    }
    speciation_rates[i] <- 1 / qx
  }
  names(speciation_rates) <- tree$tip.label

  # Initialize a rate column with NAs
  sp_occ <- occurrences |> dplyr::mutate(rate = NA)

  # If the user specified label_type = "epithet"
  if (label_type == "epithet") {
    # For each name in speciation_rates, look for that name in the Species
    #  column of the occurrence record dataframe and add the appropriate rate
    for (i in names(speciation_rates)) {
      sp_occ <- sp_occ |>
        dplyr::mutate(
          rate = dplyr::case_when(
            grepl(i, specificEpithet) ~ speciation_rates[[i]],
            TRUE ~ rate # Keeps values for non-matching rows
          )
        )
    }
  } else if (label_type == "binomial") {
    # The occurrence record dataframe has separate "genus" and "species"
    #  columns, so they should be combined for this label type
    sp_occ$Binomial <- paste(
      sp_occ$genericName,
      sp_occ$specificEpithet,
      sep = " "
    )
    # Now, for each name in speciation_rates, look for that name in the Binomial
    #  column of the occurrence record dataframe and add the appropriate rate
    for (i in names(speciation_rates)) {
      sp_occ <- sp_occ |>
        dplyr::mutate(
          rate = dplyr::case_when(
            grepl(i, Binomial) ~ speciation_rates[[i]],
            TRUE ~ rate # Keeps values for non-matching rows
          )
        )
    }
  }

  # Remove rows with NA in rate column
  sp_occ <- sp_occ[!is.na(sp_occ$rate), ]

  return(sp_occ)
}

# Create alias for estimate_dr
#' @rdname estimate_dr
#' @export
estimate_DR <- estimate_dr