
#' Check marine fish and invertebrate scientific names
#'
#' Checks marine fish and invertebrate scientific names against species in FishBase.
#'
#' @param species A character vector of species scientific names
#' @return A character vector of species scientific names not in FishBase
#' @examples
#' # Check species names (Gadus morhoa is incorrect)
#' species <- c("Gadus morhua", "Gadus morhoa")
#' check_names(species)
#' @export
check_names <- function(species){

  # All FB/SLB taxa
  fbtaxa <- freeR::all_fish()

  # Check that species are in FB/SLB
  spp_wrong <- sort(unique(species[!species %in% fbtaxa$sciname]))
  return(spp_wrong)

}
