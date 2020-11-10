
#' Get common name for marine fish and invertebrates
#'
#' Gets ONE common name for marine fish and invertebrate scientific names. The common name comes from FishBase species table, FishBase English common names,
#' FishBase non-English common names.
#'
#' @param species A character vector of species scientific names
#' @return A dataframe of species common names and their sources
#' @examples
#' # Get common names
#' species <- c("Gadus morhua", "Clupea harengus", "Callinectes sapidus")
#' fb_comm_name(species)
#' @export
fb_comm_name <- function(species){

  # Try 1. FishBase species page
  spp_key1_orig <- freeR::fishbase(dataset="species", species=species, cleaned=T)
  spp_key1 <- spp_key1_orig %>%
    select(species, comm_name) %>%
    filter(!is.na(comm_name)) %>%
    mutate(source="FishBase species table")

  # Try 2. FishBase common name page (English names)
  spp_missing_from_try1 <- species[!species %in% spp_key1$species]
  spp_key2_fin_orig <- rfishbase::sci_to_common(species_list= spp_missing_from_try1, server = "fishbase")
  spp_key2_inv_orig <- rfishbase::sci_to_common(species_list= spp_missing_from_try1, server = "sealifebase")
  spp_key2_orig <- rbind(spp_key2_fin_orig, spp_key2_inv_orig) %>%
    filter(!is.na(ComName))
  spp_key2 <- spp_key2_orig %>%
    filter(Language=="English") %>%
    select(Species, ComName) %>%
    rename(species=Species, comm_name=ComName) %>%
    mutate(source="FishBase common name table - English")

  # Try 3. FishBase common name page (not English but no punctuation in name)
  spp_done <- c(spp_key1$species, spp_key2$species)
  spp_missing_from_try12 <- species[!species %in% spp_done]
  spp_key3 <- spp_key2_orig %>%
    # Remove species already completed
    filter(Species %in% spp_missing_from_try12) %>%
    # Remove common name with ?s
    filter(!grepl("\\?", ComName)) %>%
    # Remove common names with punctuation
    filter(!grepl("[:punct:]", ComName)) %>%
    # Take first option
    group_by(Species) %>%
    slice(1) %>%
    # Format columns
    select(Species, ComName, Language) %>%
    rename(species=Species, comm_name=ComName, source=Language) %>%
    ungroup()

  # Try 4. FishBase common name page (not English and punctuation in name)
  spp_done <- c(spp_key1$species, spp_key2$species, spp_key3$species)
  spp_missing_from_try123 <- species[!species %in% spp_done]
  spp_key4 <- spp_key2_orig %>%
    # Remove species already completed
    filter(Species %in% spp_missing_from_try12) %>%
    # Remove common name with ?s
    filter(!grepl("\\?", ComName)) %>%
    # Take first option
    group_by(Species) %>%
    slice(1) %>%
    # Format columns
    select(Species, ComName, Language) %>%
    rename(species=Species, comm_name=ComName, source=Language) %>%
    ungroup()

  # Merge names
  spp_key <- rbind(spp_key1, spp_key2, spp_key3, spp_key4) %>%
    arrange(species) %>%
    # Remove any duplicates
    group_by(species) %>%
    slice(1) %>%
    ungroup() %>%
    # Replace <> with *
    mutate(comm_name=gsub( " *<.*?> *", "*", comm_name)) %>%
    # Capitalize first letter
    mutate(comm_name=stringr::str_to_sentence(comm_name))

  # Check for duplicates
  freeR::which_duplicated(spp_key$species)

  # Return
  return(spp_key)

}
