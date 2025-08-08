# Create test PAM
pam <- as.data.frame(matrix(ncol = 3, nrow = 2))
colnames(pam) <- c("Island", "G_Sp1", "G_Sp2")
pam[1,] <- c("Cuba", 0, 1)
pam[2,] <- c("Dominican Republic", 1, 0)

# Test PAM with incorrect column names
pam_name <- pam
colnames(pam_name) <- c("one", "two", "three")

# Matrix PAM
pam_mat <- as.matrix(pam)

########

test_that("The find_pam_areas function returns a dataframe", {
  expect_s3_class(find_pam_areas(pam), "data.frame")
})

test_that("The find_pam_areas function returns a dataframe with 6 columns", {
  expect_equal(ncol(find_pam_areas(pam)), 6)
})

test_that("Inputting a dataframe without the correct column names will cause
          an error", {
  expect_error(find_pam_areas(pam_name))
})

test_that("Inputting a matrix instead of a dataframe will cause an error", {
  expect_error(find_pam_areas(pam_mat))
})

test_that("Correct island areas are returned", {
  test <- find_pam_areas(pam)
  expect_equal(test[1,6], 1.22e+11)
})
