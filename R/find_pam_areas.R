#' Find areas of islands using a presence-absence matrix (PAM)
#'
#' Use a presence-absense matrix (PAM) to create a dataframe that can be
#' used to generate a species-area relationship (SAR) or speciation-area
#' relationship (SpAR) in the `ssarp` pipeline.
#'
#' PAMs summarize the occurrence
#' of species across different geographic locations. The column names of a PAM
#' are species names, with the exception of the first column, which specifies
#' the name of locations. For each cell corresponding to a species/location
#' pair, either a 1 (presence) or a 0 (absence) is input depending on whether
#' the species can be found at that location or not.
#'
#' Using a PAM, this function will find the areas of the land masses relevant
#' to the taxon of interest with two options: a built-in database of island
#' names and areas, or a user-provided list of island names and areas.
#'
#' The default method is to reference a built-in dataset of island names
#' and areas to find the areas of the landmasses relevant to the taxon of
#' interest. The user may also decide to input their own custom dataframe
#' including names of relevant land masses and their associated areas to
#' bypass using `ssarp`'s built-in dataset.
#'
#' While the word "landmasses" was used heavily in this documentation, users
#' supplying their own custom area dataframe or shapefile are encouraged to
#' use this function in the `ssarp` workflow to create species- and speciation-
#' area relationships for island-like systems such as lakes, fragmented habitat,
#' and mountain peaks.
#'
#' @param pam A presence-absence matrix (PAM), saved as a dataframe.
#' Please ensure that the PAM has
#' species names (include both generic name and specific epithet,
#' with an underscore separating them) as the column
#' names, with the exception of the first column
#' that designates locations, which must be named "Island".
#' @param area_custom A dataframe including names of land masses and their
#' associated areas. This dataframe should be provided when the user would like
#' to bypass using the built-in database of island names and areas. Please
#' ensure that the custom dataframe includes the land mass's area in a column
#' called "AREA" and the name in a column called "Name". (Optional)
#' @return A dataframe of species names, island names, and island areas
#' @examples
#' pam <- read.csv(system.file("extdata",
#'                             "example_pam.csv",
#'                             package = "ssarp"))
#' areas <- find_pam_areas(pam = pam)
#' @export

find_pam_areas <- function(pam, area_custom = NULL) {
  # Checkmate input verification
  checkmate::assertDataFrame(pam)
  checkmate::testSubset(
    c("Island"),
    names(pam)
  )

  # Turn PAM into dataframe with three columns:
  #  Island, genericName, specificEpithet
  # First, create a list to hold data
  dat <- list()
  for (i in seq_len(nrow(pam))) {
    # Get current island
    island <- pam$Island[i]

    # Figure out which species are on the island (1s in row)
    # Species are column names, excluding the first (which is the island name)
    sp <- names(pam)[-1][pam[i, -1] == 1]

    # Just in case the user input a PAM where there are 0 species on an island,
    #  check that there is at least one species in sp before continuing
    if (length(sp) > 0) {
      # Split species names into genericName and specificEpithet for downstream
      split_sp <- strsplit(sp, "_")

      # Turn into a dataframe
      sp_df <- do.call(rbind.data.frame, split_sp)
      # Fix colnames
      colnames(sp_df) <- c("genericName", "specificEpithet")

      # Add island to dataframe (call it "third" for ssarp::find_areas())
      sp_df$third <- island

      # Add this small dataframe to the list
      dat[[length(dat) + 1]] <- sp_df
    }
  }

  # Turn dat into a dataframe
  occs <- do.call(rbind.data.frame, dat)

  # Add "first" and "second" columns so it can be used with ssarp::find_areas()
  occs$first <- NA
  occs$second <- NA
  # They also need to be characters
  occs$first <- as.character(occs$first)
  occs$second <- as.character(occs$second)

  return(find_areas(occs, area_custom))
}