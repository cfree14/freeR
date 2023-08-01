
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
    rename(#Predicted
           k=K,                       # Von B growth coefficient
           linf_cm=Loo,               # Asymptotic length (cm)
           winf_g=Winfinity,          # Asymptotic weight (g)
           m=M,                       # Natural mortality (1/yr)
           tmax_yr=tmax,              # Maximum age (yr)
           tmat_yr=tm,                # Age at maturity (yr)
           lmat_cm=Lm,                # Length at maturity (cm)
           temp_c=Temperature,        # Temperature (°C)
           recruit_sd_cond=ln_var,    # Conditional standard deviation of recruitment variability (τ)
           recruit_sd_auto=rho,       # Autocorrelation of recruitment variability (ρ)
           masps=ln_MASPS,            # Maximum annual spawners per spawner in excess of recruitment (r)
           # Derived
           recruit_sd_marg=ln_margsd, # Marginal standard deviation for recruitment (σ): σ = sqrt(τ^2 / (1-ρ^2))
           h=h,                       # Steepness
           fmsy_div_m=ln_Fmsy_over_M, # FMSY / M
           fmsy=ln_Fmsy,              # FMSY
           r=r,                       # Intrinsic growth rate
           g_yr=G) %>%                # Generation time (yr)
    # Exponentiate:
    # Predicted: Temperature, recruitment autocorrelation, MASPS are unlogged
    # r, G, FMSY/M, temperature are unlogged?
    mutate_at(.vars=c("k", "linf_cm", "winf_g",
                      "m",  "tmax_yr", "tmat_yr", "lmat_cm",
                      "recruit_sd_cond",
                      "recruit_sd_marg", "fmsy", "fmsy_div_m"), .funs=exp) %>%
    # Add species
    mutate(sci_name=spp) %>%
    # Arrange
    select(sci_name,
           k, linf_cm, winf_g,
           m, tmax_yr,  tmat_yr, lmat_cm,
           recruit_sd_cond, recruit_sd_marg, recruit_sd_auto, masps, h,
           r, g_yr, fmsy, fmsy_div_m,
           everything())
  fl

  # Return
  return(fl)

}
