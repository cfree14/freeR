
#' Download resilience values from FishBase/SeaLifeBase
#'
#' Downloads resilience values from FishBase/SeaLifeBase.
#'
#' @param species A character vector of species scientific names to look up
#' @return A dataframe of resilience values from FishBase/SeaLifeBase
#' @examples
#' # Download cleaned FishBase life history data
#' species <- c("Callinectes sapidus", "Gadus morhua")
#' resilience(species=species)
#' @export
resilience <- function(species){

  # Get resilience
  spp_info <- freeR::fishbase(dataset="stocks", species=sort(unique(species)), cleaned = F, level="species", add_taxa = F)
  res_info <- spp_info %>%
    # Clean names
    janitor::clean_names("snake") %>%
    # Simplify
    select(species, resilience) %>%
    # Unique names and sorted
    unique() %>%
    arrange(species) %>%
    # Eliminate NA values
    filter(!is.na(resilience)) %>%
    # Select first resilience value
    group_by(species) %>%
    slice(1) %>%
    ungroup()

  # Return
  return(res_info)

}
