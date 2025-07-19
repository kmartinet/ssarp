#' Get speciation rates following equation 4 in Magallon and Sanderson (2001)
#'
#' Use methodology from Magallon and Sanderson (2001) to estimate speciation
#' rates using a user-provided phylogeny and output a dataframe for use in
#' ssarp's speciation-area relationship pipeline. This method also removes any
#' species rows without rates (this is most likely to occur when the tree does
#' not have all of the species included in the occurrence record dataframe)
#' @param tree The dated phylogenetic tree that corresponds with the taxa to be
#' included in a speciation-area relationship
#' @param label_type Either "epithet" or "binomial" (default): describes the
#' type of tip label in the provided tree. If "epithet," only the species
#' epithet will be used when interacting with the tree. If "binomial," the full
#' species name (including genus) will be used when interacting with the tree.
#' @param occurrences The occurrence record dataframe output from the ssarp
#' pipeline. If you would like to use a custom dataframe, please make sure that
#' there are columns titled "specificEpithet", "genericName", and "areas"
#' @return A dataframe that includes speciation rates for each island in the
#' user-provided occurrence record dataframe.
#' @references
#' - Magallόn, S. & Sanderson, M.J. (2001). Absolute Diversification Rates in
#' Angiosperm Clades. Evolution, 55(9): 1762-1780.
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
#' occ_speciation <- estimate_MS(tree = tree,
#'                               label_type = "epithet",
#'                               occurrences = areas)
#'
#' @export

estimate_MS <- function(tree, label_type = "binomial", occurrences) {
  # Checkmate input validation
  checkmate::assertString(label_type)
  checkmate::assertDataFrame(occurrences)
  checkmate::testSubset(
    c("specificEpithet", "genericName", "areas"),
    names(occurrences)
  )
  # Ensure columns are correct type
  checkmate::assertCharacter(occurrences$specificEpithet)
  checkmate::assertCharacter(occurrences$genericName)
  checkmate::assertNumeric(occurrences$areas)

  # Get all subtrees from given phylogenetic tree
  sub_trees <- ape::subtrees(tree)

  # If the user specified label_type = "epithet"
  if (label_type == "epithet") {
    # Organize species into island groups
    island_groups <- tapply(
      occurrences$specificEpithet,
      occurrences$areas,
      unique
    )
  } else if (label_type == "binomial") {
    # The occurrence record dataframe has separate "genus" and "species"
    #  columns, so they should be combined for this label type
    # First, double-check that the "Species" column doesn't have any NAs
    occurrences <- occurrences[!is.na(occurrences$specificEpithet), ]
    # Then create a new column with the full name
    occurrences$Binomial <- paste(
      occurrences$genericName,
      occurrences$specificEpithet,
      sep = " "
    )
    # Organize species into island groups
    island_groups <- tapply(occurrences$Binomial, occurrences$areas, unique)
  }

  # Create a df to store: each monophyletic group, the number of species in
  #  each, and the node age
  mono_df <- data.frame()
  for (g in seq(island_groups)) {
    comp_group <- island_groups[[g]]
    # See how many subtrees are in this list of species
    for (i in seq(sub_trees)) {
      # If all taxa in the current subtree is in the comparison group, add its
      #  info to the df
      if (all(sub_trees[[i]]$tip.label %in% comp_group)) {
        comp_group <- g
        log_area <- names(island_groups[g])
        ntips <- sub_trees[[i]]$Ntip
        # Get node age
        # There are usually multiple nodes in the subtree, so get the root age
        node_ages <- ape::node.depth.edgelength(sub_trees[[i]])
        # The root age is the max of the node ages
        root_age <- max(node_ages)
        # Gather info to add to df
        new_row <- c(comp_group, log_area, ntips, root_age)
        mono_df <- rbind(mono_df, new_row)
      }
    }
  }

  colnames(mono_df) <- c("comp_group", "area", "ntips", "root_age")

  # They've all turned into characters, so let's change that
  mono_df$area <- as.numeric(mono_df$area)
  mono_df$ntips <- as.numeric(mono_df$ntips)
  mono_df$root_age <- as.numeric(mono_df$root_age)

  # Following Magallon and Sanderson (2001) Equation 4
  # lambda = [log(n) - log2]/t
  sp_rates <- rep(0, length(mono_df$comp_group))
  for (i in seq(mono_df$comp_group)) {
    sp_rates[i] <- (log(mono_df[i, 3]) - log(2)) / mono_df[i, 4]
  }

  # Add speciation rates to mono_df
  mono_df$specation_rates <- sp_rates

  # Now create full dataframe
  # Since we're looking at speciation rates for each island, we don't need to
  #  retain species names
  uniq_islands <- unique(occurrences$areas)
  # If an island only had one species on it, the speciation rate will
  #  remain zero
  sp_rates <- rep(0, length(uniq_islands))
  final_df <- as.data.frame(cbind(uniq_islands, sp_rates))

  # Add speciation rates for specific islands from mono_df
  for (i in seq_len(length(mono_df$area))) {
    # Figure out which row has the current area
    ind <- which(final_df$uniq_islands == mono_df[i, 2])
    # Add corresponding speciation rate to final_df
    final_df[ind, 2] <- mono_df[i, 5]
  }

  # Remove rows with NA in area column
  final_df <- final_df[!is.na(final_df$uniq_islands), ]

  # Rename columns
  colnames(final_df) <- c("areas", "rate")

  # Ensure columns are numeric
  final_df$areas <- as.numeric(final_df$areas)
  final_df$rate <- as.numeric(final_df$rate)

  # The rates here are logged, which would make it incorrect to log them again
  #   when the speciation-area relationship is plotted
  #   (as happens in ssarp::create_SpAR()).
  # To this end, we will exponentiate the rate values here so when they are
  #   logged in ssarp::create_SpAR(), the rates will be displayed appropriately.
  final_df$rate <- exp(final_df$rate)

  return(final_df)
}

