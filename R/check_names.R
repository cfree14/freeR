
#' Check marine fish and invertebrate scientific names
#'
#' Checks marine fish and invertebrate scientiic names against species in FishBase.
#'
#' @param x A character vector of species scientific names
#' @return A character vector of species scientific names not in FishBase
#' @examples
#' # Check names (Gadus morhoa is incorrect)
#' species <- c("Gadus morhua", "Gadus morhoa")
#' check_species(species)
#' @export
check_species <- function(species){

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
  spp_wrong <- species[!species %in% taxa_key$sciname]
  return(spp_wrong)

}
