
#' Download cleaned FishBase/SeaLifeBase life history data
#'
#' Downloads FishBase and SeaLifeBase life history data using rfishbase. The download comes cleaned and the function includes the option to return life history for all of the species included in either the genera or the families of the species requested (this is useful when calculating genus- or family-level life history averages.)
#'
#' @param dataset FishBase/SeaLifeBase dataset to download: "lw" or "vonb"
#' @param species A character vector of species scientific names to look up
#' @param level Download life history data for just the provided species ("species") or for all species in the genera ("genus") or families ("family") represented in the requested species list.
#' @return A cdataframe if life history traits from FishBase/SeaLifeBase
#' @examples
#' # Download cleaned FishBase life history data
#' spp <- c("Magallana gigas", "Gadus morhua")
#' lh_data <- fishbase(dataset="vonb", species=spp, level="genus")
#' @export
fishbase <- function(dataset, species, level="species"){

  # All FB/SLB taxa
  fbtaxa <- all_fish()

  # What species to look up?
  if(level=="species"){
    spp_list <- tibble(sciname=species)
  }
  if(level=="genus"){
    spp_genera <- freeR::taxa(species) %>%
      select(genus) %>%
      unique() %>% arrange()
    spp_list <- fbtaxa %>%
      filter(genus %in% spp_genera$genus)
  }
  if(level=="family"){
    spp_families <- freeR::taxa(species) %>%
      select(family) %>%
      unique() %>% arrange()
    spp_list <- fbtaxa %>%
      filter(family %in% spp_families$family)
  }

  # LW parameters
  if(dataset=="lw"){
    fin <- rfishbase::length_weight(spp_list$sciname, server="fishbase") %>% mutate(database="FishBase") %>% select(database, everything())
    inv <- rfishbase::length_weight(spp_list$sciname, server="sealifebase") %>% mutate(database="SeaLifeBase") %>% select(database, everything())
    fbdata <- rbind(fin, inv) %>%
      filter(!is.na(Species)) %>%
      select(database, Species, Type, Sex, LengthMin, LengthMax,  a, aTL, b, EsQ) %>%
      rename(length_min_cm=LengthMin, length_max_cm=LengthMax, a_tl=aTL, doubtful=EsQ) %>%
      setNames(tolower(colnames(.))) %>%
      arrange(database, species) %>%
      mutate(sex=tolower(sex),
             doubtful=tolower(doubtful))
  }

  # Von B parameters
  if(dataset=="vonb"){
    fin <- rfishbase::popgrowth(spp_list$sciname, server="fishbase") %>% mutate(database="FishBase") %>% select(database, everything())
    inv <- rfishbase::popgrowth(spp_list$sciname, server="sealifebase") %>% mutate(database="SeaLifeBase") %>% select(database, everything())
    fbdata <- rbind(fin, inv) %>%
      filter(!is.na(Species)) %>%
      select(database, Species, Sex, Data,MethodGrowth, Type, Loo, TLinfinity, K, to, Winfinity, Auxim, tmax, tm, M, MethodM, Mquality) %>%
      rename(linf_cm=Loo, tl_linf_cm=TLinfinity, winf_g=Winfinity, tmax_yr=tmax, tmat_yr=tm, m_method=MethodM, m_quality=Mquality,
             vonb_data=Data, vonb_method=MethodGrowth, t0=to, vonb_quality=Auxim) %>%
      setNames(tolower(colnames(.))) %>%
      arrange(database, species) %>%
      mutate(vonb_quality=tolower(vonb_quality),
             m_quality=tolower(m_quality))
  }

  # Return
  return(fbdata)

}
