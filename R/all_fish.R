
#' All fish and invertebrates in FishBase and SeaLifeBase
#'
#' Returns a dataframe with the taxonomic information for all fish and invertebrates in FishBase and SeaLifeBase.
#' There are >34,000 fish and >117,000 invertebrates described by these two databases.
#'
#' @return A dataframe of taxonomic information for all FB/SLB fish and invertebrates
#' @examples
#' # Get all FishBase/SeaLifeBase species
#' spp <- all_fish()
#' @export
all_fish <- function(){

  # Build FB key
  taxa_key_fb <- rfishbase::fishbase %>% # (1-old) rfishbase::load_taxa(server="fishbase") (2-old)# server="https://fishbase.ropensci.org"
    as.data.frame() %>%
    mutate(type="fish") %>%
    select(type, everything()) %>%
    setNames(tolower(colnames(.))) %>%
    mutate(sciname=paste(genus, species)) %>%
    mutate_all(as.character)

  # Build SLB key
  taxa_key_slb <- rfishbase::sealifebase %>% # (1-old) rfishbase::load_taxa(server="sealifebase") (2-old) rfishbase::sealifebase %>%
    as.data.frame() %>%
    mutate(type="invert") %>%
    select(type, everything()) %>%
    setNames(tolower(colnames(.))) %>%
    mutate(sciname=paste(genus, species)) %>%
    mutate_all(as.character)

  taxa_key <-  taxa_key_fb %>%
    bind_rows(taxa_key_slb) %>%
    setNames(tolower(names(.))) %>%
    select(type, class, order, family, genus, species, sciname) %>%
    unique()

  return(taxa_key)

}
