
#' Download cleaned FishBase/SeaLifeBase life history data
#'
#' Downloads FishBase and SeaLifeBase life history data using rfishbase. The download comes cleaned and the function includes the option to return life history for all of the species included in either the genera or the families of the species requested (this is useful when calculating genus- or family-level life history averages.)
#'
#' @param dataset FishBase/SeaLifeBase dataset to download: species, lw, vonb, ecology, maturity, fecundity, reproduction, morphology
#' @param species A character vector of species scientific names to look up
#' @param level Download life history data for just the provided species ("species") or for all species in the genera ("genus") or families ("family") represented in the requested species list.
#' @param cleaned FALSE means you get all of the data and TRUE means you get a cleaned subset of important columns
#' @param add_taxa TRUE means taxonomic information is added to the life history data
#' @return A cdataframe if life history traits from FishBase/SeaLifeBase
#' @examples
#' # Download cleaned FishBase life history data
#' species <- c("Callinectes sapidus", "Gadus morhua")
#' lh_data <- fishbase(dataset="ecology", species=species, level="species", cleaned=T)
#' @export
fishbase <- function(dataset, species, level="species", cleaned=F, add_taxa=T){

  # Functionality to add
  # 1) Append taxanomic info to output
  # 2) rfishbase functions: eggs, morphology,

  # All FB/SLB taxa
  fbtaxa <- freeR::all_fish()

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
    fbdata <- plyr::rbind.fill(fin, inv) %>%
      filter(!is.na(Species))
    if(cleaned==T){
      fbdata <- fbdata %>%
          filter(!is.na(Species)) %>%
          select(database, Species, Type, Sex, LengthMin, LengthMax,  a, aTL, b, EsQ) %>%
          rename(length_min_cm=LengthMin, length_max_cm=LengthMax, a_tl=aTL, doubtful=EsQ) %>%
          setNames(tolower(colnames(.))) %>%
          arrange(database, species) %>%
          mutate(sex=tolower(sex),
                 doubtful=tolower(doubtful))
    }
  }

  # Von B parameters
  if(dataset=="vonb"){
    fin <- rfishbase::popgrowth(spp_list$sciname, server="fishbase") %>% mutate(database="FishBase") %>% select(database, everything())
    inv <- rfishbase::popgrowth(spp_list$sciname, server="sealifebase") %>% mutate(database="SeaLifeBase") %>% select(database, everything())
    fbdata <- plyr::rbind.fill(fin, inv) %>%
      filter(!is.na(Species))
    if(cleaned==T){
      fbdata <- fbdata %>%
        filter(!is.na(Species)) %>%
        select(database, Species, Sex, Data,MethodGrowth, Type, Loo, TLinfinity, K, to, Winfinity, Auxim, tmax, tm, M, MethodM, Mquality) %>%
        rename(linf_cm=Loo, tl_linf_cm=TLinfinity, winf_g=Winfinity, tmax_yr=tmax, tmat_yr=tm, m_method=MethodM, m_quality=Mquality,
               vonb_data=Data, vonb_method=MethodGrowth, t0=to, vonb_quality=Auxim) %>%
        setNames(tolower(colnames(.))) %>%
        arrange(database, species) %>%
        mutate(vonb_quality=tolower(vonb_quality),
               m_quality=tolower(m_quality))
    }
  }

  # Ecology
  if(dataset=="ecology"){
    fin <- rfishbase::ecology(spp_list$sciname, server="fishbase") %>% mutate(database="FishBase") %>% select(database, everything())
    inv <- rfishbase::ecology(spp_list$sciname, server="sealifebase") %>% mutate(database="SeaLifeBase") %>% select(database, everything())
    fbdata <- plyr::rbind.fill(fin, inv) %>%
      filter(!is.na(Species))
    if(cleaned==T){
      fbdata <- fbdata %>%
        filter(!is.na(Species)) %>%
        select(database, Species, Herbivory2, FeedingType, DietTroph, DietSeTroph, DietRemark, FoodTroph, FoodSeTroph, FoodRemark) %>%
        janitor::clean_names("snake") %>%
        rename(prey_type=herbivory2, troph_diet=diet_troph, troph_diet_se=diet_se_troph, troph_diet_notes=diet_remark,
               troph_food=food_troph, troph_food_se=food_se_troph, troph_food_notes=food_remark)
    }
  }

  # Species
  if(dataset=="species"){
    # Get all data
    fin <- rfishbase::species(spp_list$sciname, server="fishbase") %>% mutate(database="FishBase") %>% select(database, everything())
    inv <- rfishbase::species(spp_list$sciname, server="sealifebase") %>% mutate(database="SeaLifeBase") %>% select(database, everything())
    fbdata_orig <- plyr::rbind.fill(fin, inv) %>%
      filter(!is.na(Species))
    fbdata <- fbdata_orig
    # Clean data
    if(cleaned==T){
      fbdata <- fbdata_orig %>%
        filter(!is.na(Species)) %>%
        # Select columns
        select(database, Species, FBname, BodyShapeI, DemersPelag, AnaCat,
               LongevityWild, Vulnerability, Length, LTypeMaxM, Weight,
               Importance, PriceCateg, PriceReliability, MainCatchingMethod,
               UsedforAquaculture, UsedasBait, Aquarium, Dangerous, Comments) %>%
        # Rename columns
        janitor::clean_names("snake") %>%
        rename(comm_name=f_bname, body_shape=body_shape_i, habitat=demers_pelag, migratory=ana_cat,
               tmax_wild_yr=longevity_wild, lmax_cm=length, lmax_type=l_type_max_m, wmax_g=weight,
               price_catg=price_categ, main_gear=main_catching_method, aquaculture=usedfor_aquaculture, bait=usedas_bait)
    }
  }

  # Maturity
  if(dataset=="maturity"){
    # Get all data
    fin <- rfishbase::maturity(spp_list$sciname, server="fishbase") %>% mutate(database="FishBase") %>% select(database, everything())
    inv <- rfishbase::maturity(spp_list$sciname, server="sealifebase") %>% mutate(database="SeaLifeBase") %>% select(database, everything())
    fbdata_orig <- plyr::rbind.fill(fin, inv) %>%
      filter(!is.na(Species))
    fbdata <- fbdata_orig
    # Clean data
    if(cleaned==T){
      fbdata <- fbdata_orig %>%
        filter(!is.na(Species)) %>%
        # Select columns
        select(database, Species, Sex, Locality, tm, AgeMatMin, AgeMatMin2,  Type1, Lm, LengthMatMin, LengthMatMin2) %>%
        # Rename columns
        rename(species=Species, sex=Sex, location=Locality,
               tmat_yr_lo=AgeMatMin, tmat_yr_hi=AgeMatMin2, tmat_yr=tm,
               lmat_cm_lo=LengthMatMin, lmat_cm_hi=LengthMatMin2, lmat_cm=Lm, lmat_type=Type1)
    }
  }

  # Fecundity
  if(dataset=="fecundity"){
    # Get all data
    fin <- rfishbase::fecundity(spp_list$sciname, server="fishbase") %>% mutate(database="FishBase") %>% select(database, everything())
    inv <- rfishbase::fecundity(spp_list$sciname, server="sealifebase") %>% mutate(database="SeaLifeBase") %>% select(database, everything())
    fbdata_orig <- plyr::rbind.fill(fin, inv) %>%
      filter(!is.na(Species))
    fbdata <- fbdata_orig
    # Clean data
    if(cleaned==T){
      fbdata <- fbdata_orig %>%
        filter(!is.na(Species)) %>%
        # Select columns
        select(database, Species, Locality,
               FecundityMin, FecundityMax,
               RelFecundityMin, RelFecundityMax, RelFecundityMean,
               a, b,
               WeightMin, WeightMax,
               LengthFecunMin, LengthFecunMax,
               LengthTypeFecMin, LengthTypeFecMax,
               FecComment) %>%
        # Rename columns
        rename(species=Species, location=Locality,
               weight_g_min=WeightMin, weight_g_max=WeightMax,
               length_cm_min=LengthFecunMin, length_cm_max=LengthFecunMax,
               length_cm_min_type=LengthTypeFecMin, length_cm_max_type=LengthTypeFecMax,
               comments=FecComment,
               fecundity_abs_min=FecundityMin, fecundity_abs_max=FecundityMax,
               fecundity_rel_min=RelFecundityMin, fecundity_rel_max=RelFecundityMax, fecundity_rel_avg=RelFecundityMean,
               length_fecundity_a=a, length_fecundity_b=b)
    }
  }

  # Reproduction
  if(dataset=="reproduction"){
    # Get all data
    fin <- rfishbase::reproduction(spp_list$sciname, server="fishbase") %>% mutate(database="FishBase") %>% select(database, everything())
    inv <- rfishbase::reproduction(spp_list$sciname, server="sealifebase") %>% mutate(database="SeaLifeBase") %>% select(database, everything())
    fbdata_orig <- plyr::rbind.fill(fin, inv) %>%
      filter(!is.na(Species))
    fbdata <- fbdata_orig
    # Clean data
    if(cleaned==T){
      fbdata <- fbdata_orig %>%
        filter(!is.na(Species)) %>%
        # Select columns
        select(database, Species, ReproMode, Fertilization, MatingSystem, MonogamyType, MatingQuality, Spawning, RepGuild1, RepGuild2, ParentalCare, AddInfos) %>%
        # Rename columns
        janitor::clean_names("snake") %>%
        rename(repro_guild1=rep_guild1, repro_guild2=rep_guild2, comments=add_infos)
    }
  }

  # Morphology
  # No cleaning performed
  if(dataset=="morphology"){
    # Get all data
    fin <- rfishbase::morphology(spp_list$sciname, server="fishbase") %>% mutate(database="FishBase") %>% select(database, everything())
    inv <- rfishbase::morphology(spp_list$sciname, server="sealifebase") %>% mutate(database="SeaLifeBase") %>% select(database, everything())
    fbdata_orig <- plyr::rbind.fill(fin, inv) %>%
      filter(!is.na(Species))
    fbdata <- fbdata_orig
    if(cleaned==T){print("No cleaning performed. Complicated dataset!")}
  }

  # Add taxonomic information
  if(add_taxa & cleaned){
    fbdata <- fbdata %>%
      left_join(select(fbtaxa, class, order, family, genus, sciname), by=c("species"="sciname")) %>%
      select(database, class, order, family, genus, species, everything())
  }
  if(add_taxa & cleaned==F){
    fbdata <- fbdata %>%
      left_join(select(fbtaxa, class, order, family, genus, sciname), by=c("Species"="sciname")) %>%
      select(database, class, order, family, genus, Species, everything())
  }

  # Return
  return(fbdata)

}
