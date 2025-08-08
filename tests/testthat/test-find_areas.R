# Create test dataframe for find_areas occs input
occs <- as.data.frame(matrix(ncol = 9, nrow = 2))
colnames(occs) <- c("acceptedScientificName", "genericName", "specificEpithet", 
                    "decimalLongitude", "decimalLatitude", "First", 
                    "Second", "Third", "datasetKey")

# Test dataframe for find_areas occs input with values
occs_vals <- occs
occs_vals[1,] <- c("Anolis first", "Anolis", "first", -81.948509, 28.028463, 
                   "USA", "Florida", "Lakeland", 1)
occs_vals[2,] <- c("Anolis second", "Anolis", "second", -81.949353, 28.028047, 
                   "USA", "Florida", "Lakeland", 1)
occs_vals[,4] <- as.numeric(occs_vals[,4])
occs_vals[,5] <- as.numeric(occs_vals[,5])

# Test dataframe for find_areas custom area input
custom_area <- as.data.frame(matrix(ncol = 2, nrow = 2))
colnames(custom_area) <- c("Name", "AREA")

# Test matrix for find_areas occs input
occ_mat <- matrix(ncol = 9, nrow = 2)
colnames(occs) <- c("acceptedScientificName", "genericName", "specificEpithet", 
                    "decimalLongitude", "decimalLatitude", "First", 
                    "Second", "Third", "datasetKey")
occ_mat[1,] <- c("Anolis first", "Anolis", "first", -81.948509, 28.028463, 
                 "USA", "Florida", "Lakeland", 1)
occ_mat[2,] <- c("Anolis second", "Anolis", "second", -81.949353, 28.028047, 
                 "USA", "Florida", "Lakeland", 1)

# Smaller dataframe for find_areas occs input
occs_small <- occs_vals[,1:5]

# Uniq_islands to test full find_areas grep sequence
uniq_islands <- c("Aappalaartoq", "Abbotts Harbour", "A Chau Island")

# Occurrence record df with incorrect column names
occ_name <- occs_vals
colnames(occ_name) <- c(1:9)

# Occurrence record df with correct column names, but incorrect types
occ_types <- occs_vals
occ_types$acceptedScientificName <- as.factor(occ_types$acceptedScientificName)
occ_types$genericName <- as.factor(occ_types$genericName)
occ_types$specificEpithet <- as.factor(occ_types$specificEpithet)
occ_types$decimalLongitude <- as.character(occ_types$decimalLongitude)
occ_types$decimalLatitude <- as.character(occ_types$decimalLatitude)
occ_types$First <- as.factor(occ_types$First)
occ_types$Second <- as.factor(occ_types$Second)
occ_types$Third <- as.factor(occ_types$Third)

### Spatial inputs ###
# Create test SpatVector with WKT polygon
# This polygon is around the University of Arizona
test_SpatVector <- terra::vect("POLYGON ((-110.96 32.23, -110.96 32.22, 
                               -110.94 32.227, -110.94 32.234, -110.96 32.23))")
test_SpatVector$name <- "UA"
test_SpatVector$areas <- 791255.92 # m^2
test_SpatVector$featurecla <- "campus"

# New dataframe for spatial testing
occs_spat <- occs
occs_spat[1,] <- c("Aspidoscelis sonorae", "Aspidoscelis", "sonorae",
                   32.2318081, -110.9469145,  "One", "Two", "Three", 1)
occs_spat[2,] <- c("Aspidoscelis sonorae", "Aspidoscelis", "sonorae",
                   32.2292764, -110.9536270,  "One", "Two", "Three", 1)
occs_spat[,4] <- as.numeric(occs_spat[,4])
occs_spat[,5] <- as.numeric(occs_spat[,5])

# names vector for spatial testing
names <- "UA"

########

test_that("Inputting an empty custom area dataframe will result in an empty 
          dataframe", {
 test_areas <- find_areas(occs_vals, custom_area)
 expect_equal(length(test_areas[,1]), 0)
})

test_that("Inputting a matrix instead of a dataframe for occurrence records 
          will cause an error", {
  expect_error(find_areas(occ_mat))
})

test_that("Inputting a dataframe with insufficient columns will cause 
          an error", {
  expect_error(find_areas(occs_small))
})

test_that("The find_areas function returns a dataframe", {
  expect_s3_class(find_areas(occs_vals), "data.frame")
})

test_that("The find_areas function returns a dataframe with 10 columns", {
  expect_equal(ncol(find_areas(occs_vals)), 10)
})

test_that("Inputting a dataframe without the correct column names will cause
          an error", {
          expect_error(find_areas(occ_name))
})

test_that("Inputting a dataframe without the correct types will cause an error",
          {
            expect_error(find_areas(occ_types))
})

test_that("Using the spatial components of find_areas returns a dataframe
          (includes names)", {
  expect_s3_class(find_areas(occs = occs_spat,
                             shapefile = test_SpatVector,
                             names = names), "data.frame")
})

test_that("Using the spatial components of find_areas returns a message
          when no 'names' variable is specified", {
            expect_message(find_areas(occs = occs_spat,
                                       shapefile = test_SpatVector), 
          "Using all names in the shapefile, this might extend processing time")
})

test_that("Inputting something other than a shapefile in 'shapefile' will
          cause an error", {
            expect_error(find_areas(occs = occs_spat,
                                    shapefile = 1))
})