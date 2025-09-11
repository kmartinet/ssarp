# Read tree via extdata file
# tree <- ape::read.tree(system.file("extdata", 
#                               "Patton_Anolis_trimmed.tree", 
#                               package = "ssarp"))
# tree <- 
#   ape::read.tree(system.file("testthat/testdata/Patton_Anolis_trimmed.tree", 
#                              package = "ssarp"))

if (!curl::has_internet()) {
  cli::cli_alert_info("No internet connection. Cannot run estimate_DR tests without 
          downloading phylogenetic tree from GitHub.")
} else {
  tree <- ape::read.tree("https://raw.githubusercontent.com/kmartinet/ssarp/refs/heads/main/vignettes/Patton_Anolis_Trimmed.tree")
  
  # Tree from: Patton, A.H., Harmon, L.J., del Rosario Castañeda, M., 
  #   Frank, H.K., Donihue, C.M., Herrel, A., & Losos, J.B. (2021). When 
  #   adaptive radiations collide: Different evolutionary trajectories between 
  #   and within island and mainland lizard clades. PNAS, 118(42): e2024451118.
  
  # The estimate_DR function only really needs "Genus" and "Species" 
  #  columns to work, so a simple dataframe can be created for testing 
  #  (without real occurrence data)
  specificEpithet <- tree$tip.label
  genericName <- rep("Anolis", length(specificEpithet))
  occ_simple <- as.data.frame(cbind(genericName, specificEpithet))
  
  # Create a tree with binomial labels
  tree_b <- tree
  binom_names <- paste(genericName, specificEpithet, sep = " ")
  tree_b$tip.label <- binom_names
  
  # Matrix version of dataframe
  occ_mat <- as.matrix(occ_simple)
  
  # Occurrence record df with incorrect column names
  occ_name <- occ_simple
  colnames(occ_name) <- c("test1", "test2")
  
  # Occurrence record df with correct column names, but incorrect types
  occ_types <- occ_simple
  occ_types$genericName <- as.factor(occ_types$genericName)
  occ_types$specificEpithet <- as.factor(occ_types$specificEpithet)
  
  # Generate speciation rates using epm to compare to Sun and Folk (2020)
  epm_dr <- epm::DRstat(tree)
  # Turn into dataframe
  epm_df <- as.data.frame(epm_dr)
  
  
  
  ########
  
  test_that("Inputting a matrix instead of a dataframe for occurrence records 
            will cause an error", {
    expect_error(estimate_DR(tree = tree, label_type = "epithet", 
                              occurrences = occ_mat))
  })
  
  test_that("Inputting a non-string for the label type will cause an error", {
    expect_error(estimate_DR(tree = tree, label_type = 1, 
                              occurrences = occ_simple))
  })
  
  test_that("The estimate_DR function returns a dataframe (epithet labels)", {
    expect_s3_class(estimate_DR(tree = tree, label_type = "epithet", 
                                 occurrences = occ_simple), "data.frame")
  })
  
  test_that("The estimate_DR function returns a dataframe (binomial labels)", {
    expect_s3_class(estimate_DR(tree = tree_b, label_type = "binomial", 
                                 occurrences = occ_simple), "data.frame")
  })
  
  test_that("When a tree with binomial tip labels is used but the label type is 
            specified as 'epithet', the returned dataframe will be empty", {
    speciation_rates <- estimate_DR(tree = tree_b, label_type = "epithet", 
                                     occurrences = occ_simple)
    expect_length(speciation_rates[,1], 0)
  })
  
  
  test_that("Inputting a dataframe without required column names causes an 
            error", {
              expect_error(estimate_DR(tree = tree,
                                       label_type = "epithet",
                                       occurrences = occ_name))
  })
  
  test_that("Inputting a dataframe with required column names, but incorrect
            types causes an error", {
              expect_error(estimate_DR(tree = tree,
                                       label_type = "epithet",
                                       occurrences = occ_types))
  })
  
  
  ##### Ensuring calculations are correct #####
  test_that("The minimum and maximum speciation rates are correct
              (The rest should also be correct if this is the case)", {
                sp_dr <- estimate_DR(tree = tree, 
                                     label_type = "epithet", 
                                     occurrences = occ_simple)
                max <- round(max(sp_dr$rate), digits = 4)
                min <- round(min(sp_dr$rate), digits = 4)
                expect_setequal(max, 0.2472)
                expect_setequal(min, 0.023)
  })
  
  test_that("estimate_DR rate results are the same as DR_stat from epm", {
    sp_dr <- estimate_DR(tree = tree,
                         label_type = "epithet",
                         occurrences = occ_simple)
    sp_ssarp <- round(sp_dr$rate, digits = 4)
    sp_epm <- round(epm_df$epm_dr, digits = 4)
    expect_setequal(sp_ssarp, sp_epm)
  })
  
}