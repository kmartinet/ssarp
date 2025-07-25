% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_sources.R
\name{get_sources}
\alias{get_sources}
\title{Gather sources from GBIF data for citation}
\usage{
get_sources(occs)
}
\arguments{
\item{occs}{The occurrence record dataframe returned by \code{ssarp::get_data()}
or \code{ssarp::find_areas()}.}
}
\value{
A dataframe of dataset keys and the number of occurrence records
associated with each key that were gathered with \code{ssarp::get_data()} and/or
filtered with \code{ssarp::find_areas()}.
}
\description{
When using data obtained via \code{ssarp::get_data()} and filtered with
\code{ssarp::find_areas()} for a publication, you must keep a record of the
datasets used in your analysis. This function assists in creating the
dataframe necessary to follow GBIF's citation guidelines (see References).
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
source_df <- get_sources(occs = dat)

}
\references{
\itemize{
\item \href{https://www.gbif.org/citation-guidelines}{GBIF citation guidelines}
\item Data obtained via \code{ssarp::get_data()} and filtered with
\code{ssarp::find_areas()} falls under \href{https://www.gbif.org/derived-dataset/about}{the derived datasets distinction}
\item \href{https://data-blog.gbif.org/post/derived-datasets/}{More information about creating derived datasets}
}
}
