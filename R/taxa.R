
#' Get taxonomic info for marine fish and invertebrates
#'
#' Get taxonomic info for marine fish and invertebrates from FishBase/SeaLifeBase.
#'
#' @param species A character vector of species scientific names
#' @return A dataframe of taxonomic information
#' @examples
#' # Get taxonomic info
#' species <- c("Gadus morhua", "Centropristis striata", "Crassostrea gigas")
#' taxa(species)
#' @export
taxa <- function(species){

  # Build FB/SLB taxa key
  taxa_key <-  freeR::all_fish()

  # Check that species are in FB/SLB
  spp <- species
  key <- filter(taxa_key, sciname %in% spp)
  return(key)

}





