#' Find areas of land masses.
#'
#' Find the areas of the land masses relevant to the taxon of interest with two
#' options: a database of island names and areas, or a user-provided shapefile.
#'
#' The first method is to reference a built-in dataset of island names
#' and areas to find the areas of the landmasses relevant to the taxon of
#' interest. The user may also decide to input their own custom dataframe
#' including names of relevant land masses and their associated areas to
#' bypass using *ssarp*'s built-in dataset.
#'
#' The second method is to reference a user-supplied shapefile containing
#' spatial information for the landmasses of interest in order to determine
#' their areas.
#'
#' While the word "landmasses" was used heavily in this documentation, users
#' supplying their own custom area dataframe or shapefile are encouraged to
#' use this function in the *ssarp* workflow to create species- and speciation-
#' area relationships for island-like systems such as lakes, fragmented habitat,
#' and mountain peaks.
#'
#' @param occs The dataframe that is returned by `ssarp::find_land()`. If using
#' a custom occurrence record dataframe, ensure that it has the following
#' columns: "acceptedScientificName", "genericName", "specificEpithet",
#' "decimalLongitude", "decimalLatitude", "First", "Second", "Third",
#' "datasetKey". The "datasetKey" column is important for GBIF records and
#' identifies the dataset to which the occurrence record belongs. Custom
#' dataframes without this style of data organization should fill the column
#' with placeholder values.
#' @param area_custom A dataframe including names of land masses and their
#' associated areas. This dataframe should be provided when the user would like
#' to bypass using the built-in database of island names and areas. Please
#' ensure that the custom dataframe includes the land mass's area in a column
#' called "AREA" and the name in a column called "Name". (Optional)
#' @param shapefile A shapefile (.shp) containing spatial information for
#' the geographic locations of interest. (Optional)
#' @param names If the user would like to restrict which polygons in the
#' shapefile are included in the returned occurrence record dataframe, they can
#' be specified here as a vector. If the user does not provide a vector, all of
#' the non-NA names in the shapefile will be included
#' (as found in shapefile$name). (Optional)
#' @return A dataframe of the species name, island name, and island area
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
#' @export

find_areas <- function(
  occs,
  area_custom = NULL,
  shapefile = NULL,
  names = NULL
) {
  # checkmate input verification
  checkmate::assertDataFrame(occs)
  checkmate::testSubset(
    c(
      "acceptedScientificName",
      "genericName",
      "specificEpithet",
      "decimalLongitude",
      "decimalLatitude",
      "First",
      "Second",
      "Third",
      "datasetKey"
    ),
    names(occs)
  )
  # Ensure columns are correct type
  checkmate::assertCharacter(occs$acceptedScientificName)
  checkmate::assertCharacter(occs$genericName)
  checkmate::assertCharacter(occs$specificEpithet)
  checkmate::assertNumeric(occs$decimalLongitude)
  checkmate::assertNumeric(occs$decimalLatitude)
  checkmate::assertCharacter(occs$First)
  checkmate::assertCharacter(occs$Second)
  checkmate::assertCharacter(occs$Third)
  # Not checking datasetKey because it is not relevant to the code and can be
  #  any type, really

  ##### NO SHAPEFILE #####
  if (is.null(shapefile)) {
    # Remove any rows where the "specificEpithet" column is NA
    occs <- occs[!is.na(occs$specificEpithet), ]

    # Remove rows where First, Second, and Third are all NA
    # Create vector to hold row numbers
    minus <- rep(NA, nrow(occs))
    # Loop through dataframe
    for (i in seq_len(nrow(occs))) {
      if (nrow(occs) == 0) {
        if (!getOption("ssarp.silent", FALSE)) {
          cli::cli_alert_warning("No data in occurrence record dataframe")
        }
        break
      }
      if (
        is.na(occs[i, "Third"]) &&
          is.na(occs[i, "Second"]) &&
          is.na(occs[i, "First"])
      ) {
        minus[i] <- i
      }
    }
    # Remove NAs (from initialization) from row number vector
    minus <- minus[!is.na(minus)]

    # If all of minus is NA, that means that there are no rows to delete
    # Only delete rows when minus is not 0
    if (length(minus) != 0) {
      occs <- occs[-minus, ]
    }

    # Add a temporary key-value pair to initialize
    island_dict <- Dict::Dict$new(
      bloop = 108
    )

    # For each island name in the current dataframe,
    # find the area and add the pair to the dictionary

    # First, create an empty list of island names
    islands <- list()

    # Next, go through the occs dataframe and see if the Third column has a name.
    # If yes, add to the island list. If NA, go to the Second column.
    # If Second column is NA, go to the First column.
    if (!getOption("ssarp.silent", FALSE)) {
      cli::cli_alert_info("Recording island names...")
    }
    for (i in seq_len(nrow(occs))) {
      if (nrow(occs) == 0) {
        if (!getOption("ssarp.silent", FALSE)) {
          cli::cli_alert_warning("No data in occurrence record dataframe")
        }
        break
      }
      if (!is.na(occs[i, "Third"])) {
        islands[i] <- occs[i, "Third"]
      } else if (!is.na(occs[i, "Second"])) {
        islands[i] <- occs[i, "Second"]
      } else if (!is.na(occs[i, "First"])) {
        islands[i] <- occs[i, "First"]
      }
    }

    # Next, eliminate duplicate entries in the list
    uniq_islands <- unique(islands)

    # Next, add the island names as keys and their corresponding areas as values
    # If the user did not supply a custom dataframe, get island areas from
    # built-in island area dataset
    if (is.null(area_custom)) {
      area_file <- get_island_areas()
    } else {
      area_file <- area_custom
    }

    # Look through the island area file and find the names in uniq_islands list
    if (!getOption("ssarp.silent", FALSE)) {
      cli::cli_alert_info("Assembling island dictionary...")
    }
    # Initialize vector of island names from island area dataset with
    #  "Island" appended
    area_file_append <- paste0(area_file$Name, " Island")
    # Initialize grep statements as NA
    grep_res <- grep_res2 <- grep_res3 <- NA

    for (i in seq(uniq_islands)) {
      # Use grep for exact match in the area database
      # [1] picks the first match if the query gets multiple matches
      query <- paste0("^", as.character(uniq_islands[i]), "$")
      grep_res <- grep(query, area_file$Name)[1]

      if (!is.na(grep_res)) {
        # If grep found a match, add it to island dictionary
        island_dict[as.character(uniq_islands[i])] <- area_file[
          grep_res,
          "AREA"
        ]
      } else {
        # If it doesn't find the name directly from uniq_islands, try adding
        #  "island" at the end
        query <- paste0("^", as.character(uniq_islands[i]), " Island$")
        grep_res2 <- grep(query, area_file$Name)[1]
        if (!is.na(grep_res2)) {
          # If grep found a match, add it to island dictionary
          island_dict[as.character(uniq_islands[i])] <- area_file[
            grep_res2,
            "AREA"
          ]
        }
      }

      # If it doesn't find the name from uniq_islands, look in area_file_append
      if (is.na(grep_res2)) {
        query <- paste0("^", as.character(uniq_islands[i]), "$")
        grep_res3 <- grep(query, area_file_append)[1]
        if (!is.na(grep_res3)) {
          # If grep found a match, add it to island dictionary
          island_dict[as.character(uniq_islands[i])] <- area_file[
            grep_res3,
            "AREA"
          ]
        }
      }
    }

    # Use the dictionary to add the areas to the final dataframe
    if (!getOption("ssarp.silent", FALSE)) {
      cli::cli_alert_info("Adding areas to final dataframe...")
    }
    areas <- rep(0, times = nrow(occs))

    for (i in seq_len(nrow(occs))) {
      if (!is.na(occs[i, "Third"]) && island_dict$has(occs[i, "Third"])) {
        areas[i] <- island_dict$get(occs[i, "Third"])
      } else if (
        !is.na(occs[i, "Second"]) && island_dict$has(occs[i, "Second"])
      ) {
        areas[i] <- island_dict$get(occs[i, "Second"])
      } else if (
        !is.na(occs[i, "First"]) && island_dict$has(occs[i, "First"])
      ) {
        areas[i] <- island_dict$get(occs[i, "First"])
      } else {
        areas[i] <- NA
      }
    }

    # Create final dataframe
    occs_final <- cbind(occs, areas)
  } else {
    ##### SHAPEFILE #####
    checkmate::assertClass(shapefile, "SpatVector")

    # Remove any rows where the "specificEpithet" column is NA
    occs <- occs[!is.na(occs$specificEpithet), ]

    # If the user input a "names" vector, use it to subset the SpatVector
    if (!is.null(names)) {
      polygons <- terra::subset(shapefile, shapefile$name %in% names)
    } else {
      if (!getOption("ssarp.silent", FALSE)) {
        cli::cli_alert_info(
          "Using all names in the shapefile, this might extend processing time"
        )
      }
      # If the user did not input a "names" vector, use
      #   the full list of polygon names
      # If there are any NAs in shapefile$name, remove them
      all_names <- shapefile$name[!is.na(shapefile$name)]

      # Still subset the shapefile using these names, since NAs were removed
      polygons <- terra::subset(shapefile, shapefile$name %in% all_names)
    }

    # Assign areas (in m^2) to polygons
    polygons$areas <- sf::st_area(sf::st_as_sf(polygons))

    # Assign polygons based on the GPS coordinates in occs
    poly_dat <- terra::extract(
      polygons,
      data.frame(occs$decimalLongitude, occs$decimalLatitude)
    )

    # Trim to only include important columns
    poly_dat <- poly_dat[, c("featurecla", "name", "areas")]

    # Add polygon info for each occurrence record to occs
    occs_final <- cbind(occs, poly_dat)
  }
  # Remove rows with NA in area column
  occs_final <- occs_final[!is.na(occs_final$areas), ]

  # Ensure areas are numeric
  occs_final$areas <- as.numeric(occs_final$areas)

  return(occs_final)
}
