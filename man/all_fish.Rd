% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_fish.R
\name{all_fish}
\alias{all_fish}
\title{All fish and invertebrates in FishBase and SeaLifeBase}
\usage{
all_fish()
}
\value{
A dataframe of taxonomic information for all FB/SLB fish and invertebrates
}
\description{
Returns a dataframe with the taxonomic information for all fish and invertebrates in FishBase and SeaLifeBase.
There are >34,000 fish and >117,000 invertebrates described by these two databases.
}
\examples{
# Get all FishBase/SeaLifeBase species
spp <- all_fish()
}
