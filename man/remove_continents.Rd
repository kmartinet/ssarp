% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_continents.R
\name{remove_continents}
\alias{remove_continents}
\title{Remove continents from area dataframe.}
\usage{
remove_continents(occs)
}
\arguments{
\item{occs}{The dataframe that is returned by \code{ssarp::find_areas()}. I do not
recommend using a custom dataframe for this function because it references
areas given by the area database used in \code{ssarp::find_areas()}. If you must
use a custom dataframe, please ensure  that the landmass areas are in a
column called "areas"}
}
\value{
A dataframe of the species name, island name, and island area
(without continents)
}
\description{
Reference a list of continental areas to remove them from the dataframe
output by \code{ssarp::find_areas()}.
}
\examples{
# The GBIF key for the Anolis genus is 8782549
# Read in example dataset filtered from:
#  dat <- rgbif::occ_search(taxonKey = 8782549,
#                           hasCoordinate = TRUE,
#                           limit = 10000)
dat <- read.csv(system.file("extdata",
                            "ssarp_Example_Dat.csv",
                            package = "ssarp"))
occs <- find_land(occurrences = dat)
areas <- find_areas(occs = occs)
new_areas <- remove_continents(areas)

}
