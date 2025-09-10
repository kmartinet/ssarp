library(httptest)

# Test dataframe for find_land occs input without values
occs <- as.data.frame(matrix(ncol = 6, nrow = 2))
colnames(occs) <- c("decimalLongitude", "decimalLatitude", 
                    "acceptedScientificName", "genericName", 
                    "specificEpithet", "datasetKey")

# Test dataframe for find_land occs input with values
occs_vals <- occs
occs_vals[1,] <- c(-81.948509, 28.028463, 
                   "Anolis first", "Anolis", "first", 1)
occs_vals[2,] <- c(-81.949353, 28.028047, 
                   "Anolis second", "Anolis", "second", 1)
occs_vals$decimalLongitude <- as.numeric(occs_vals$decimalLongitude)
occs_vals$decimalLatitude <- as.numeric(occs_vals$decimalLatitude)

# Test matrix for find_land occs input
occ_mat <- matrix(ncol = 6, nrow = 2)
colnames(occ_mat) <- c("decimalLongitude", "decimalLatitude", 
                       "acceptedScientificName", "genericName", 
                       "specificEpithet", "datasetKey")
occ_mat[1,] <- c(-81.948509, 28.028463, "Anolis first", "Anolis", "first", 1)
occ_mat[2,] <- c(-81.949353, 28.028047, "Anolis second", "Anolis", "second", 1)

# Smaller dataframe for find_land occs input
occs_small <- occs_vals[,1:3]

# Dataframe with no rows for find_land occs input
occs <- as.data.frame(matrix(ncol = 6, nrow = 0))
colnames(occs) <- c("decimalLongitude", "decimalLatitude", 
                    "acceptedScientificName", "genericName", 
                    "specificEpithet", "datasetKey")

# Dataframe with an occurrence record point that map.where() doesn't have 
#  info for
# Test dataframe for find_land occs input with values
occs_mystery <- occs_vals
occs_mystery[1,] <- c(-90.66546, -0.611595, "Anolis first", 
                      "Anolis", "first", 1)
occs_mystery$decimalLongitude <- as.numeric(occs_mystery$decimalLongitude)
occs_mystery$decimalLatitude <- as.numeric(occs_mystery$decimalLatitude)

# Occurrence record df with incorrect column names
occ_name <- occs_vals
colnames(occ_name) <- c(1:6)

# Occurrence record df with correct column names, but incorrect types
occ_types <- occs_vals
occ_types$acceptedScientificName <- as.factor(occ_types$acceptedScientificName)
occ_types$genericName <- as.factor(occ_types$genericName)
occ_types$specificEpithet <- as.factor(occ_types$specificEpithet)
occ_types$decimalLongitude <- as.character(occ_types$decimalLongitude)
occ_types$decimalLatitude <- as.character(occ_types$decimalLatitude)

########

test_that("Inputting a matrix instead of a dataframe for occurrence records 
          will cause an error", {
  expect_error(find_land(occ_mat))
})

test_that("Inputting a non-logical for the Fillgaps parameter 
          will cause an error", {
  expect_error(find_land(occs_vals, fillgaps = 0))
})

test_that("The find_land function returns a dataframe", {
  expect_s3_class(find_land(occs_vals), "data.frame")
})

test_that("When occurrence record dataframe input has no rows, 
          expect a warning message", {
  expect_error(find_land(occs))
})

test_that("Inputting a dataframe without the correct column names will cause
          an error", {
            expect_error(find_land(occ_name))
})

test_that("Inputting a dataframe without the correct types will cause an error",
          {
            expect_error(find_land(occ_types))
})

##### API testing #####
# Using a mock API through httptest so that Photon doesn't need to be called 
#  directly.
# Mock API response saved in \tests\testthat\photon.komoot.io

# Wrap with the "with_mock_api" function
with_mock_api({
  test_that("A location not found with the maps package can be found with
            Photon when using fillgaps = TRUE",
            {
              result <- find_land(occs_mystery, fillgaps = TRUE)
              expect_equal(result$second[1], "Isla Pinzón")
            })
})
