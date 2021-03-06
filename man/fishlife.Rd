% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fishlife.R
\name{fishlife}
\alias{fishlife}
\title{Get FishLife life history traits}
\usage{
fishlife(species)
}
\arguments{
\item{species}{A character vector of species scientific names}
}
\value{
A dataframe with life history trait predictions from FishLife for each species
}
\description{
Retrieves life history trait predictions from FishLife (Thorson et al. 2018). This
is a wrapper for the Plot_taxa() function in the FishLife R package.
}
\examples{
# Look up life history traits
species <- c("Gadus morhua", "Centropristis striata", "Paralichthys dentatus")
fishlife(species)
}
