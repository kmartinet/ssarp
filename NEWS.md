ssarp 0.5.0 (2025-09-16)
=========================

### NEW FEATURES

  * The default versions of `create_SAR()`, `create_SpAR()`, `estimate_BAMM()`, `estimate_DR()`, and `estimate_MS()` are now all-lowercase (`create_sar()`, `create_spar()`, `estimate_bamm()`, `estimate_dr()`, and `estimate_ms()`). The original spellings have been added as aliases to the functions (both spellings may be used).

ssarp 0.4.1 (2025-09-11)
=========================

### NEW FEATURES

  * Added a [new vignette discussing spatial autocorrelation and how to test for it within the `ssarp` pipeline](https://github.com/kmartinet/ssarp/commit/c82cac9bcc8a92e4b213d522cb01a3021944f9c8)

### DOCUMENTATION FIXES

  * Code and text in the README has been clarified, including fixing an error in the example code related to using `find_land()`
  * All references to the depricated `find_land()` function have been removed from documentation
  * Added better descriptions of the outputs for `create_SAR()` and `create_SpAR()` 
  
### OTHER FIXES

  * New `testthat` test cases for `find_areas()` and `find_land()`

ssarp 0.4.0 (2025-07-19)
=========================

### NEW FEATURES

  * Package name is now `ssarp` instead of `SSARP`
  * Added the `ssarp::get_richness()` function, which creates a standard species richness dataframe
  * The `ssarp::get_data()` and `ssarp::get_key()` functions have been replaced by [a helpful vignette describing how to access occurrence records](https://kmartinet.github.io/ssarp/articles/Get_Data.html) using `rgbif`.
  * Messages across the package can be silenced using `options(ssarp.silent = TRUE)` now
  * A new example file for testing `ssarp::estimate_BAMM` has been added (`inst/extdata/event_data_Patton_Anolis.txt`)
  
### DOCUMENTATION FIXES

  * All examples run instead of remaining in a `\dontrun` block
  * More information about using the metadata from plots created with `ssarp::create_SAR()` and `ssarp::create_SpAR()` has been added to their documentation
  * More information about the speciation rate estimation methods in `ssarp` have been added to the `ssarp::create_SpAR()` documentation
  
  
### OTHER FIXES
  
  * New `testthat` test cases to ensure that calculations are done correctly across the package
  * Column names have been standardized across the package
  * Plot printing is now off by default in `ssarp::create_SAR()` and `ssarp::create_SpAR()`
  
### DEPRECATED FUNCTIONS
  
  * `get_data()`
  * `get_key()`
  * `quick_create_SAR()`


SSARP 0.3.0 (2025-07-04)
=========================

### NEW FEATURES

  * Changed the names of all functions to "verb_object" structure
  * Two new example files were added to the package: `Patton_Anolis_Trimmed.tree` and `SSARP_Example_Dat.csv` to allow users to run examples involving a phylogenetic tree of *Anolis* and GBIF data for *Anolis*, respectively
  * Added "get_presence_absence" function, which creates a presence-absence matrix when given a dataframe output by `SSARP::find_areas()`

### DOCUMENTATION FIXES
  * Function names are now in `pkg::function()` notation throughout the documentation
  * Vignettes have been updated to reflect the new function names and example files
  * The majority of examples will now run, instead of remaining in a `\dontrun` block as in 0.2.0

### OTHER FIXES
  * `@import` and `@importFrom` statements were removed in favor of pkg::function() statements across the package

SSARP 0.2.0 (2025-04-29)
=========================

### NEW FEATURES

  * Added NEWS file
  
### DOCUMENTATION FIXES
  * Added badge for status at rOpenSci software peer review to README
  
