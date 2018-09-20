
#' Get taxonomic info for marine fish and invertebrates
#'
#' Get taxonomic info for marine fish and invertebrates from FishBase/SeaLifeBase.
#'
#' @param species A character vector of species scientific names
#' @return A dataframe of taxonomic information
#' @examples
#' # Get taxonomic info
#' species <- c("Gadus morhua", "Centropristis striata")
#' taxa(species)
#' @export
taxa <- function(species){

  # Build FB/SLB taxa key
  taxa_key_fb <- rfishbase::load_taxa(server="https://fishbase.ropensci.org") %>% mutate(type="fish") %>% select(type, everything())
  taxa_key_slb <- rfishbase::sealifebase %>% mutate(type="invert") %>% select(type, everything())
  taxa_key <-  taxa_key_fb %>%
    bind_rows(taxa_key_slb) %>%
    setNames(tolower(names(.))) %>%
    mutate(sciname=paste(genus, species)) %>%
    select(type, class, order, family, genus, species, sciname) %>%
    unique()

  # Check that species are in FB/SLB
  spp <- species
  key <- filter(taxa_key, sciname %in% spp)
  return(key)

}