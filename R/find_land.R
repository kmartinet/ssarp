#' Find the name of the land on which the occurrence points were found
#'
#' Use various mapping tools to attempt to find the names of land masses where
#' the occurrence points were found.
#' @param occurrences The dataframe output by `ssarp::get_data()` (or if using
#' a custom dataframe, ensure that it has the following columns:
#' decimalLongitude, decimalLatitude, acceptedScientificName, genericName,
#' specificEpithet, datasetKey). The "datasetKey" column is important for GBIF
#' records and identifies the dataset to which the occurrence record belongs.
#' Custom dataframes without this style of data organization should fill the
#' column with placeholder values.
#' @param fillgaps (logical) Attempt to use Photon API to fill in gaps left by
#' `mapdata::map.where()` (TRUE) or only `mapdata::map.where()` results
#' (FALSE, default). While it is powerful, the Photon API does not have a
#' standard location for island names in its returned information, so using it
#' will likely require the returned dataframe to be cleaned by the user.
#' @return A dataframe of the species name, longitude, latitude, and three parts
#' of occurrence information. "First" is the name used to describe the largest
#' possible area of land where the occurrence point is found. "Second" is the
#' name used to describe the second-largest possible area of land that
#' corresponds with the occurrence point. "Third" is the most specific area of
#' land that corresponds with the occurrence point. Functions later in the ssarp
#' pipeline default to checking whether "Third" has an entry, then look at
#' "Second," and then "First."
#' @examples
#' # The GBIF key for the Anolis genus is 8782549
#' # Read in example dataset filtered from:
#' #  dat <- rgbif::occ_search(taxonKey = 8782549,
#' #                           hasCoordinate = TRUE,
#' #                           limit = 10000)
#' dat <- read.csv(system.file("extdata",
#'                             "ssarp_Example_Dat.csv",
#'                             package = "ssarp"))
#' occs <- find_land(occurrences = dat, fillgaps = FALSE)
#'
#' @import mapdata
#' @import maps
#' @export

find_land <- function(occurrences, fillgaps = FALSE) {
  # checkmate input validation
  checkmate::assertDataFrame(occurrences)
  checkmate::assertLogical(fillgaps)
  checkmate::testSubset(
    c(
      "acceptedScientificName",
      "genericName",
      "specificEpithet",
      "decimalLongitude",
      "decimalLatitude",
      "datasetKey"
    ),
    names(occurrences)
  )
  # Ensure columns are correct type
  checkmate::assertCharacter(occurrences$acceptedScientificName)
  checkmate::assertCharacter(occurrences$genericName)
  checkmate::assertCharacter(occurrences$specificEpithet)
  checkmate::assertNumeric(occurrences$decimalLongitude)
  checkmate::assertNumeric(occurrences$decimalLatitude)
  # Not checking datasetKey because it is not relevant to the code and can be
  #  any type, really

  # Check if there is anything in the given occurrences
  # If not, return an error
  if (nrow(occurrences) == 0) {
    stop("Occurrence record dataframe has no entries", call. = FALSE)
  }

  lon <- as.numeric(occurrences$decimalLongitude)
  lat <- as.numeric(occurrences$decimalLatitude)
  # Use map.where to find landmass names that correspond to GPS points
  # First, use worldHires (world2Hires has a different coordinate system)
  where <- maps::map.where(database = "mapdata::worldHires", x = lon, y = lat)
  # Then, use world just in case it can fill gaps
  where2 <- maps::map.where(database = "world", x = lon, y = lat)

  # Next, combine the two where options (prioritize where2)
  for (i in seq(where2)) {
    # Check if this row is NA
    if (is.na(where2[i])) {
      # If it is NA, check if where is not NA
      if (!is.na(where[i])) {
        # If where is not NA, grab the name it found
        where2[i] <- where[i]
      }
    }
  }

  occs <- as.data.frame(cbind(
    occurrences$acceptedScientificName,
    occurrences$genericName,
    occurrences$specificEpithet,
    lon,
    lat,
    where2,
    occurrences$datasetKey
  ))

  # Separate the where column into two separate columns - Country and Island
  # But sometimes there are three...
  suppressWarnings(
    occs <- occs |>
      tidyr::separate(where2, c("First", "Second", "Third"), sep = ":")
  )
  colnames(occs) <- c(
    "acceptedScientificName",
    "genericName",
    "specificEpithet",
    "decimalLongitude",
    "decimalLatitude",
    "First",
    "Second",
    "Third",
    "datasetKey"
  )

  if (fillgaps == TRUE) {
    # There might still be a lot of NA entries, so use Photon to try to
    #  fill in gaps
    if (!getOption("ssarp.silent", FALSE)) {
      cli::cli_alert_info("Filling gaps using Photon...")
    }
    for (i in seq_len(nrow(occs))) {
      if (nrow(occs) == 0) {
        if (!getOption("ssarp.silent", FALSE)) {
          cli::cli_alert_warning("Occurrence record dataframe has no entries")
        }
        break
      }
      if (is.na(occs[i, "First"])) {
        # Get lon and lat
        longitude <- occs[i, "decimalLongitude"]
        latitude <- occs[i, "decimalLatitude"]

        # Create Photon URL
        url <- paste0(
          "http://photon.komoot.io/reverse?lon=",
          longitude,
          "&lat=",
          latitude
        )

        # Create a user agent to tell Photon that ssarp is making the request
        user <-
          httr::user_agent(
            "ssarp R Package (https://github.com/kmartinet/ssarp)"
          )

        # GET content from the Photon API, including the user agent in the call
        data <- httr::content(
          httr::GET(url = url, config = user),
          encoding = "UTF-8"
        )

        # Sometimes data$features has nothing in it, so first check if it
        # has something
        if (length(data$features) != 0) {
          PhotonInfo <- data$features[[1]]$properties

          # Different information is passed back sometimes, so try to find the
          #  best options
          # First, check if a country is listed and put it in the appropriate
          #  column
          if (length(PhotonInfo$country) != 0) {
            occs[i, 6] <- as.character(PhotonInfo$country)
          }

          # Next, try to find the island name
          # Sometimes, it is in the "name" part of the Photon data
          # If not, the "locality" part of the Photon data often has the island
          #  name
          if (length(PhotonInfo$name) != 0) {
            occs[i, 7] <- as.character(PhotonInfo$name)
          } else if (length(PhotonInfo$locality) != 0) {
            occs[i, 7] <- as.character(PhotonInfo$locality)
          }
        }
      }
    }
  } # End fillgaps

  # Enure that longitude and latitude are numeric
  occs$decimalLatitude <- as.numeric(occs$decimalLatitude)
  occs$decimalLongitude <- as.numeric(occs$decimalLongitude)

  return(occs)
}
