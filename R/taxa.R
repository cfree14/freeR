
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
  taxa_key_fb <- rfishbase::load_taxa(server="https://fishbase.ropensci.org") %>%
    as.data.frame() %>%
    mutate(type="fish") %>%
    select(type, everything()) %>%
    setNames(tolower(colnames(.))) %>%
    rename(sciname=species) %>%
    mutate(species=stringr::word(sciname, start=2, end=sapply(strsplit(sciname, " "), length)))
  taxa_key_slb <- rfishbase::sealifebase %>%
    as.data.frame() %>%
    mutate(type="invert") %>%
    select(type, everything()) %>%
    setNames(tolower(colnames(.))) %>%
    mutate(sciname=paste(genus, species))
  taxa_key <-  taxa_key_fb %>%
    bind_rows(taxa_key_slb) %>%
    setNames(tolower(names(.))) %>%
    select(type, class, order, family, genus, species, sciname) %>%
    unique()

  # Check that species are in FB/SLB
  spp <- species
  key <- filter(taxa_key, sciname %in% spp)
  return(key)

}





