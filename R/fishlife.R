
#' Get FishLife life history traits
#'
#' Retrieves life history trait predictions from FishLife (Thorson et al. 2018). This
#' is a wrapper for the Plot_taxa() function in the FishLife R package.
#'
#' @param species A character vector of species scientific names
#' @return A dataframe with life history trait predictions from FishLife for each species
#' @examples
#' # Look up life history traits
#' species <- c("Gadus morhua", "Centropristis striata", "Paralichthys dentatus")
#' fishlife(species)
#' @export
fishlife <- function(species){

  # Setup container
  spp <- sort(unique(species))

  # Loop through species
  x <- spp[1]
  fl_orig <- purrr::map_df(spp, function(x){

    # Get spp info
    sciname <- x
    genus <- stringr::word(sciname, 1)
    nwords_in_spp <- length(strsplit(sciname, " ")[[1]])
    species1 <- stringr::word(sciname, start=2, end=nwords_in_spp)
    species1 <- ifelse(species1=="spp", "predictive", species1)

    # Try looking up in FishLife
    spp_info <- try(FishLife::Plot_taxa(FishLife::Search_species(Genus=genus, Species=species1)$match_taxonomy))
    if(inherits(spp_info, "try-error")){
      # Record blanks
      spp_lh_vals_log <- rep(NA, 20)
      names(spp_lh_vals_log)<-c("Loo", "K", "Winfinity", "tmax", "tm", "M", "Lm",
                               "Temperature", "ln_var", "rho", "ln_MASPS", "ln_margsd", "h", "logitbound_h",
                               "ln_Fmsy_over_M", "ln_Fmsy", "ln_r", "r", "ln_G", "G")
    }else{
      # Values are in log-scale except temperature
      spp_lh_vals_log <- spp_info[[1]]$Mean_pred
    }

    # Return
    spp_lh_vals_log

  })

  # Format
  fl <- fl_orig %>%
    # Reduce
    select(Loo, K, Winfinity, tmax, tm, M, Lm, Temperature, ln_var, rho, ln_MASPS, ln_margsd, h,
           ln_Fmsy_over_M, ln_Fmsy, r, G) %>%
    # Rename
    rename(linf_cm=Loo,
           k=K,
           winf_g=Winfinity,
           tmax_yr=tmax,
           tmat_yr=tm,
           m=M,
           lmat_cm=Lm,
           temp_c=Temperature,
           ln_var=ln_var,
           rho=rho,
           masps=ln_MASPS,
           margsd=ln_margsd,
           h=h,
           fmsy_div_m=ln_Fmsy_over_M,
           fmsy=ln_Fmsy,
           r=r,
           g_yr=G) %>%
    # Unlog
    # r, G, FMSY/M, temperature are unlogged?
    mutate_at(.vars=c("linf_cm", "k", "winf_g", "tmax_yr", "tmat_yr", "m", "lmat_cm",
                      "ln_var", "masps", "margsd", "fmsy"), .funs=exp) %>%
    # Add species
    mutate(sci_name=spp) %>%
    # Arrange
    select(sci_name, everything())
  fl

  # Return
  return(fl)

}
