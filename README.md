
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SSARP

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/kmartinet/SSARP/branch/main/graph/badge.svg)](https://codecov.io/gh/kmartinet/SSARP?branch=main)
[![R-CMD-check](https://github.com/kmartinet/SSARP/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kmartinet/SSARP/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/685_status.svg)](https://github.com/ropensci/software-review/issues/685)
<!-- badges: end -->

`ssarp` (Species-/Speciation-Area Relationship Projector) is an R package that 
provides a suite of functions to help users **create speciation- and species-
area relationships for island-dwelling taxa** using occurrence data from GBIF
(Global Biodiversity Information Facility) or the user's own occurrence data. 

The species-area relationship (SAR) is an important metric for quantifying
patterns of biodiversity on islands within an island biogeographical framework.
SARs visualize the relationship between species richness (the number of species)
and the area of the island on which they live.

Additionally, the potential for speciation on islands can be visualized using
a speciation-area relationship (SpAR), which plots speciation rates against
the area of the island on which the associated species live. Creating both a
SAR and a SpAR for taxa on a given archipelago can help researchers answer 
questions about the drivers of biodiversity in their systems 
(e.g., Is there a threshold for island size at which point _in situ_ 
speciation drives species richness in the archipelago?).

SARs and SpARs are also useful for determining whether biodiversity is distributed 
in an unusual way in a given island system 
(e.g., species richness decreasing as islands get larger instead of 
increasing). An unusual trend in these relationships may be indicative of 
habitat loss or a negative impact of non-native species.

Please find the [bioRxiv preprint of the manuscript associated with *ssarp* here!](https://www.biorxiv.org/content/10.1101/2024.12.31.630948v1)

## Installation

To install *ssarp*, use the "install_github" function from the *devtools* package:

``` r
# install.packages("devtools")
library(devtools)

install_github("kmartinet/ssarp")
```

## Suppressing Messages

To suppress all messages output by *ssarp*, run

``` r
options(ssarp.silent = TRUE)
```

before using any of *ssarp*'s functions.

## Citing ssarp

Please cite *ssarp* if you use it for your work! The citation is:

Martinet, K.M., Román-Palacios, C., & Harmon, L.J. (2025). 
“SSARP: An R Package for Easily Creating Species- and
Speciation- Area Relationships Using Web Databases.” _bioRxiv_. 
doi:10.1101/2024.12.31.630948

## Extending Your SAR Analysees

If you want a deeper exploration of SAR models once you've completed your
analyses with `ssarp`, the `ssarp::get_richness()` function can create a
species richness dataframe for use in the [sars R package](https://txm676.github.io/sars/articles/sars-r-package.html).
The `sars` package allows users to fit a wide variety of SAR models that `ssarp` 
does not create.

## Example: Creating a Species-Area Relationship

A species-area relationship (SAR) visualizes the relationship between
species richness (the number of species) and the area of the land mass
on which the species live. This brief example covers the *ssarp*
workflow for creating a SAR, and more detailed explanations of the code
and methods can be found [in the Articles on the SSARP pkgdown
website](https://kmartinet.github.io/ssarp/index.html).

In order to construct a species-area relationship with *ssarp*, we will:

- Gather occurrence data from GBIF
- Filter out invalid occurrence records
- Find areas of pertinent land masses
- Create a species-area relationship

#### Step 1: Gather occurrence data from GBIF

In this step, we will find the unique identifying key associated with a 
taxon of interest (the lizard genus *Anolis* in this case) and use that
key to access occurrence points for that taxon from GBIF. We will 
restrict the returned occurrence records to the Caribbean islands using
the "geometry" parameter in the `rgbif::occ_search()` function.

``` r
library(rgbif)

query <- "Anolis"
rank <- "Genus"

suggestions <- rgbif::name_suggest(q = query, rank = rank)

# The correct key is the first element in the first row
key <- as.numeric(suggestions$data[1,1])

# Print key
key
#> [1] 8782549
```

The GBIF key for *Anolis* is 8782549. We will use this key, along with
a Well-Known Text (WKT) polygon around Caribbean islands, to get data
for *Anolis* from GBIF using the `rgbif::occ_search()` function.

``` r

# Get data for Anolis from GBIF in a specified polygon around Caribbean islands
dat <- rgbif::occ_search(taxonKey = key, 
                         limit = 10000,
                         hasCoordinate = TRUE,
                         geometry = 'POLYGON((-84.8 23.9, -84.7 16.4, -65.2 13.9, -63.1 11.0, -56.9 15.5, -60.5 21.9, -79.3 27.8, -79.8 24.8, -84.8 23.9))')

# Print the first 5 lines of dat
head(dat, n = 5)
#> # A tibble: 5 × 156
#>   key        scientificName   decimalLatitude decimalLongitude issues datasetKey
#>   <chr>      <chr>                      <dbl>            <dbl> <chr>  <chr>     
#> 1 5007064248 Anolis distichu…            19.0            -69.0 cdc,c… 50c9509d-…
#> 2 5007082681 Anolis evermann…            18.3            -66.3 cdc,c… 50c9509d-…
#> 3 5007565800 Anolis cristate…            18.4            -66.0 cdc,c… 50c9509d-…
#> 4 5008127474 Anolis sagrei D…            18.4            -64.5 cdc,c… 50c9509d-…
#> 5 5037019266 Anolis leachii …            17.1            -61.8 cdc,c… 50c9509d-…
#> # ℹ 150 more variables: publishingOrgKey <chr>, installationKey <chr>,
#> #   hostingOrganizationKey <chr>, publishingCountry <chr>, protocol <chr>,
#> #   lastCrawled <chr>, lastParsed <chr>, crawlId <int>, basisOfRecord <chr>,
#> #   occurrenceStatus <chr>, taxonKey <int>, kingdomKey <int>, phylumKey <int>,
#> #   classKey <int>, familyKey <int>, genusKey <int>, speciesKey <int>,
#> #   acceptedTaxonKey <int>, acceptedScientificName <chr>, kingdom <chr>,
#> #   phylum <chr>, family <chr>, genus <chr>, species <chr>, …
```

The “dat” dataframe above includes the first 10,000 records from GBIF
for *Anolis* within a specified polygon around the Caribbean islands.

#### Step 2: Filter out invalid occurrence records

Now that we have a dataframe that includes the first 10,000 records for
*Anolis* within a specified polygon around the Caribbean islands, we
will filter that data to include only occurrence records that are on
land. Some occurrence records might have GPS points that are in the
ocean instead of on an island, so it is important to exclude these
invalid records.

``` r
library(ssarp)

# Find land mass names and exclude records not on land
land_dat <- ssarp::find_land(occurrences = dat)

# Print first 5 lines of land_dat
head(land_dat, n = 5)
#>                                  SpeciesName  Genus      Species  Longitude
#> 1                Anolis distichus Cope, 1861 Anolis    distichus -69.010239
#> 2           Anolis evermanni Stejneger, 1904 Anolis    evermanni -66.314592
#> 3 Anolis cristatellus Duméril & Bibron, 1837 Anolis cristatellus -65.957558
#> 4       Anolis sagrei Duméril & Bibron, 1837 Anolis       sagrei -64.512895
#> 5      Anolis leachii Duméril & Bibron, 1837 Anolis      leachii -61.847698
#>    Latitude       First Second Third                           datasetKey
#> 1 19.015033        <NA>   <NA>  <NA> 50c9509d-22c7-4a22-a47d-8c48425ef4a7
#> 2  18.29657 Puerto Rico   <NA>  <NA> 50c9509d-22c7-4a22-a47d-8c48425ef4a7
#> 3 18.396785 Puerto Rico   <NA>  <NA> 50c9509d-22c7-4a22-a47d-8c48425ef4a7
#> 4 18.384578        <NA>   <NA>  <NA> 50c9509d-22c7-4a22-a47d-8c48425ef4a7
#> 5 17.118499     Antigua   <NA>  <NA> 50c9509d-22c7-4a22-a47d-8c48425ef4a7
```

The “land_dat” dataframe above is a filtered version of the “dat”
dataframe that we created by gathering data from GBIF. The “land_dat”
dataframe includes occurrence records with GPS points that fall on a
land mass, along with the locality information for that land mass.

The locality information is split across three columns: “First,”
“Second,” and “Third.” The mapping utilities that *ssarp* uses sometimes
output different levels of specificity for locality information (up to
three different levels), so these columns provide space for these
different levels. The island name that we are interested in will be in
the last filled-in column of the three. For example, if there are two
columns of locality information for a given occurrence record, the
island name will be in the second. If there is only one column of
locality information, it will contain the island name (as with Puerto
Rico and Antigua above). If all columns have NA, the occurrence record
is invalid and will be filtered out in the next step.

#### Step 3: Find areas of pertinent land masses

Next, we will use the dataframe of occurrence records with their 
associated land mass names (the "land_dat" object created above) 
with the `ssarp::find_areas()` function to find the area of each 
land mass.

``` r

# Use the land mass names to get their areas
area_dat <- ssarp::find_areas(occs = land_dat)

# Print the first 5 lines of area_dat
head(area_dat, n = 5)
#>                                  SpeciesName  Genus      Species  Longitude
#> 2           Anolis evermanni Stejneger, 1904 Anolis    evermanni -66.314592
#> 3 Anolis cristatellus Duméril & Bibron, 1837 Anolis cristatellus -65.957558
#> 5      Anolis leachii Duméril & Bibron, 1837 Anolis      leachii -61.847698
#> 6      Anolis leachii Duméril & Bibron, 1837 Anolis      leachii -61.845561
#> 7                  Anolis grahami Gray, 1845 Anolis      grahami -77.060327
#>    Latitude       First Second Third                           datasetKey
#> 2  18.29657 Puerto Rico   <NA>  <NA> 50c9509d-22c7-4a22-a47d-8c48425ef4a7
#> 3 18.396785 Puerto Rico   <NA>  <NA> 50c9509d-22c7-4a22-a47d-8c48425ef4a7
#> 5 17.118499     Antigua   <NA>  <NA> 50c9509d-22c7-4a22-a47d-8c48425ef4a7
#> 6 17.121707     Antigua   <NA>  <NA> 50c9509d-22c7-4a22-a47d-8c48425ef4a7
#> 7  18.39293     Jamaica   <NA>  <NA> 50c9509d-22c7-4a22-a47d-8c48425ef4a7
#>         areas
#> 2  9710687500
#> 3  9710687500
#> 5   301187500
#> 6   301187500
#> 7 12225750000
```

The “area_dat” dataframe includes records with GPS points that are
associated with a land mass, along with the areas of those land masses
(in m^2).

#### Step 4: Create the species-area relationship

Finally, we will generate the SAR using the `ssarp::create_SAR()` 
function. The `ssarp::create_SAR()` function creates multiple regression 
objects with breakpoints up to the user-specified “npsi” parameter. For 
example, if “npsi” is two, `ssarp::create_SAR()` will generate regression 
objects with zero (linear regression), one, and two breakpoints. The 
function will then return the regression object with the lowest AIC score. 
The “npsi” parameter will be set to one in this example. Note that if 
linear regression (zero breakpoints) is better-supported than segmented 
regression with one breakpoint, the linear regression will be returned 
instead.

``` r

ssarp::create_SAR(occurrences = area_dat, npsi = 1)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

    #> 
    #>  ***Regression Model with Segmented Relationship(s)***
    #> 
    #> Call: 
    #> segmented.lm(obj = linear, seg.Z = ~x, npsi = 1, control = seg.control(display = FALSE))
    #> 
    #> Estimated Break-Point(s):
    #>           Est. St.Err
    #> psi1.x 22.021  0.712
    #> 
    #> Coefficients of the linear terms:
    #>             Estimate Std. Error t value Pr(>|t|)  
    #> (Intercept) -0.96715    0.99558  -0.971   0.3382  
    #> x            0.09202    0.05340   1.723   0.0939 .
    #> U1.x         0.70904    0.21351   3.321       NA  
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #> 
    #> Residual standard error: 0.5596 on 34 degrees of freedom
    #> Multiple R-Squared: 0.6515,  Adjusted R-squared: 0.6207 
    #> 
    #> Boot restarting based on 6 samples. Last fit:
    #> Convergence attained in 2 iterations (rel. change 1.0388e-09)

This is the species-area relationship (SAR) for Anolis including
island-based occurrences within a polygon around Caribbean islands from
the first 10,000 records for the genus in GBIF! The best-fit model was a
segmented regression with one breakpoint. The R console will also output
statistical information about the model.

### Workflow Summary for using data from GBIF to create a species-area relationship plot

1. Use `rgbif` to gather occurrence records, or input your own dataframe of 
occurrence records.
2. Use `find_land(occ, fillgaps)` with the dataframe obtained in Step 1 to 
figure out the names of landmasses using the occurrence record GPS points and 
the [*maps* R package](https://cran.r-project.org/web/packages/maps/index.html).
Setting the "fillgaps" parameter to `TRUE` will enable the use of 
[Photon API](https://photon.komoot.io/) to fill in any missing landmass names 
left by the *maps* R package.
3. Use `find_areas(occ, area_custom)` with the dataframe obtained in Step 2 to 
match the landmass names to a dataset that includes names of most islands on 
the planet and their areas. If the user would like to use a custom island area 
dataset instead of the built-in one, the "area_custom" parameter can be set to 
the name of the custom island area dataframe.
3a. If you'd like to only include occurrence records from islands, you can 
remove continental records by using `remove_continents(occ)` with the dataframe
returned by `find_areas()`
4. Use `create_SAR(occ, npsi)` with the dataframe obtained in Step 3 to create 
a species-area relationship plot that reports information important to the 
associated regression. The "npsi" parameter indicates the maximum number of 
breakpoints the user would like to compare for model selection. The returned 
model and plot correspond with the best-fit model.

### Workflow summary for using data from GBIF and a user-provided phylogenetic tree to create a speciation-area relationship plot

1. Use `rgbif` to gather occurrence records, or input your own dataframe of 
occurrence records.
2. Use `find_land(occ, fillgaps)` with the dataframe obtained in Step 1 to 
figure out the names of landmasses using the occurrence record GPS points and 
the [*maps* R package](https://cran.r-project.org/web/packages/maps/index.html).
Setting the "fillgaps" parameter to `TRUE` will enable the use of 
[Photon API](https://photon.komoot.io/) to fill in any missing landmass names 
left by the *maps* R package.
3. Use `find_areas(occ, area_custom)` with the dataframe obtained in Step 2 to 
match the landmass names to a dataset that includes names of most islands on 
the planet and their areas. If the user would like to use a custom island area 
dataset instead of the built-in one, the "area_custom" parameter can be set to 
the name of the custom island area dataframe.
3a. If you'd like to only include occurrence records from islands, you can 
remove continental records by using `remove_continents(occ)` with the dataframe 
returned by `find_areas()`
4. Use either `estimate_DR(tree, label_type, occ)` or 
`estimate_MS(tree, label_type, occ)` with your own phylogenetic tree that 
corresponds with the taxa signified in previous steps, a classifier that 
describes your tip labels (whether the tip labels are simply species epithets 
or full scientific names), and the dataframe obtained in Step 3 to add tip 
speciation rates using the DR statistic (Jetz et al. 2012) or the lambda 
calculation for crown groups from Magallόn and Sanderson (2001) respectively 
to the occurrence dataframe. The user may also choose to estimate tip 
speciation rates from a BAMM analysis (Rabosky 2014) by using 
`estimate_BAMM(label_type, occ, edata)` with a classifier that describes your 
tip labels (whether the tip labels are simply species epithets or full 
scientific names), the occurrence record dataframe obtained in Step 3, and a 
bammdata object generated by reading the event data file from a BAMM analysis 
with the *BAMMtools* package (Rabosky et al. 2014).
5. Use `create_SpAR(occ, npsi)` with the dataframe obtained in Step 4 to create 
a speciation-area relationship plot that reports information important to the 
associated regression. The "npsi" parameter indicates the maximum number of 
breakpoints the user would like to compare for model selection. The returned 
model and plot correspond with the best-fit model.

### Some helpful notes about well-known text (WKT) representation of geometry
When running `getData()`, the user can specify a well-known text (WKT) 
representation of geometry to restrict the geographic location of the returned
occurrence records. The rgbif::occ_search function that `getData()` calls 
requires a counter-clockwise winding order for WKT. I find it helpful to think 
about WKT polygons in this way: imagine a square around your geographic area of 
interest and pick one of the corners as a starting point. The order of points 
in WKT format should follow counter-clockwise from the corner you picked first,
and the final entry in the WKT string needs to be the same as the first entry. 
Additionally, while GPS points are typically represented in "latitude, 
longitude" format, WKT expects them in "longitude latitude" format with commas 
separating the points rather than individual longitude and latitude values. 
WKT polygons can have more specified points than included in this simple square
example, and even include polygons nested within others or polygons with holes
in the middle. 

#### Literature Cited

- Jetz, W., Thomas, G.H, Joy, J.B., Harmann, K., & Mooers, A.O. (2012).
  The global diversity of birds in space and time. *Nature*, 491:
  444-448.
- Magallόn, S. & Sanderson, M.J. (2001). Absolute Diversification Rates
  in Angiosperm Clades. *Evolution*, 55(9): 1762-1780.
- Rabosky, D.L. (2014). Automatic Detection of Key Innovations, Rate
  Shifts, and Diversity-Dependence on Phylogenetic Trees. PLOS ONE,
  9(2): e89543.436
- Rabosky, D.L., Grundler, M., Anderson, C., Title, P., Shi, J.J.,
  Brown, J.W., Huang, H., & Larson, J.G. (2014). BAMMtools: an R package
  for the analysis of evolutionary dynamics on phylogenetic trees.
  Methods in Ecology and Evolution, 5: 701-707.
