
#' Suggest correct names for invalid scientific names
#'
#' Suggests correct names for invalid scientific names of marine fish and invertebrates.
#' The function first suggests valid synonyms on FishBase and SeaLifeBase. If no valid
#' synonyms are available, the function suggests potential names by fuzzy matching
#' the available FishBase/SeaLifeBase names. The function is essentially an extension
#' of rfishbase::validate_names() but produces a more organized and thorough output.
#'
#' @param species A character vector of species scientific names
#' @return A list of suggestions of correct scientific names for invalid scientific names.
#' @examples
#' # Suggest scientific names for invalid scientific names
#' species <- c("Neoplatycephalus richardsoni", "Tetrapturus albidus", "Gadus morhuaa", "Loligo pealeii")
#' suggest_names(species)
#' @export
suggest_names <- function(species){

  # Suppress warnings temporarily
  options(warn=-1)

  # Build FB/SLB taxa key
  taxa_key_fb <- rfishbase::load_taxa(server="https://fishbase.ropensci.org") %>%
    mutate(type="fish") %>%
    select(type, everything()) %>%
    setNames(tolower(colnames(.)))
  taxa_key_slb <- rfishbase::sealifebase %>%
    mutate(type="invert") %>%
    select(type, everything()) %>%
    setNames(tolower(colnames(.)))
  taxa_key <-  taxa_key_fb %>%
    bind_rows(taxa_key_slb) %>%
    setNames(tolower(names(.))) %>%
    mutate(sciname=paste(genus, species)) %>%
    select(type, class, order, family, genus, species, sciname) %>%
    unique()

  # Identify incorrect names
  wrong <- sort(unique(species[!species %in% taxa_key$sciname]))

  # Check FishBase synonyms
  options(FISHBASE_API = "https://fishbase.ropensci.org")
  check1 <- lapply(wrong, function(x) rfishbase::validate_names(x))
  names(check1) <- wrong

  # Check SeaLifeBase synonyms
  options(FISHBASE_API = "https://fishbase.ropensci.org/sealifebase")
  wrong_still <- names(check1[is.na(check1)])
  check2 <- lapply(wrong_still, function(x) rfishbase::validate_names(x))
  names(check2) <- wrong_still

  # Fuzzy match against FB/SLB
  wrong_still <- names(check2[is.na(check2)])
  check3 <- lapply(wrong_still, function(x) taxa_key$sciname[agrep(x, taxa_key$sciname)])
  names(check3) <- wrong_still

  # Turn warnings back on
  options(warn=0)

  # Merge results
  check1_use <- check1[!is.na(check1)]
  check2_use <- check2[!is.na(check2)]
  merge1 <- RCurl::merge.list(check1_use, check2_use)
  suggestions <- RCurl::merge.list(merge1, check3)
  return(suggestions)

}
