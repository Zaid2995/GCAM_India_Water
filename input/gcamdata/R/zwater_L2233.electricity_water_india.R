#' module_water_L2233.electricity_water_india
#'
#' Generates India level model inputs for the downscaled electricity sector with cooling system types disaggregated, at the basin level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2233.DeleteSupplysector_Indiaelec}, \code{L2233.DeletePassthroughsector_Indiaelec}, \code{L2233.out_EJ_elec_B_F_tech_Yh_cool_India}, \code{L2233.IndiaStubTecheleccool},
#' \code{L223.IndiaSectorUseTrialMarket_elec}, \code{L2233.IndiaSupplysectorelecLogit}, \code{L2233.IndiaBasinsElecMarket}, \code{L2233.IndiaBasinelecLogit}, \code{L2233.IndiaSubsectorelec_logit},
#' \code{L2233.IndiaSubsectorelec_shrwt}, \code{L2233.IndiaSubsectorelec_interp},
#' \code{L223.IndiaSubsector0Shrwt}, \code{L2233.Indiaplantelec_logit}, \code{L2233.Indiacoolingelec_logit}, \code{L2233.IndiaTechShrwtFlltelec}, \code{L2233.IndiaTechInterpelec},
#' \code{L2233.IndiaTechInterpToelec}, \code{L2233.IndiaTechShrwtnucelec}, \code{L2233.IndiaTechShrwtrenewelec},
#' \code{L223.IndiaTechEff_elec}, \code{L223.IndiaIntTechEff_elec}, \code{L2233.IndiaCSPReserve},
#' \code{L2233.IndiaElecTechShrwtBasinscool}, \code{L2233.IndiaStubTechShrwteleccool}, \code{L2233.IndiaStubTechCapfactorelecCSP},
#' \code{L2233.IndiaStubTechEffeleccool}, \code{L2233.IndiaGlobalIntTechCapital_elec_cool},
#' \code{L2233.IndiaGlobalTechCapital_elec_cool}, \code{L2233.IndiaAvgFossilEffKeyword_elec_cool}, \code{L2233.IndiaGlobalIntTechBackup_elec_cool},
#' \code{L2233.IndiaGlobalIntTechCapFac_elec_cool},\code{L2233.IndiaGlobalIntTechLifetime_elec_cool},\code{L2233.IndiaGlobalIntTechShrwt_elec_cool},
#' \code{L2233.IndiaGlobalTechCapFac_elec_cool},\code{L2233.IndiaGlobalTechCapture_elec_cool},\code{L2233.IndiaGlobalTechLifetime_elec_cool},
#' \code{L2233.IndiaGlobalTechProfitShutdown_elec_cool},\code{L2233.IndiaGlobalTechSCurve_elec_cool},\code{L2233.IndiaGlobalTechShrwt_elec_cool},
#' \code{L2233.IndiaGlobalTechCoef_elec_cool},\code{L2233.IndiaGlobalIntTechCoef_elec_cool},
#' \code{L2233.IndiaPrimaryRenewKeyword_elec_cool},\code{L2233.IndiaPrimaryRenewKeywordInt_elec_cool},\code{L2233.IndiaBasinsStubTechTrackCapital_elec},
#' \code{L2233.IndiaBasinsStubTechFixOut_hydro}.
#'
#' The corresponding file in the
#' original data system was \code{L2233.electricity_water_india.R} (water level2).
#' @details Disaggregates electricity sector for all cooling system types at the basin level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter first if_else group_by left_join mutate right_join select summarise first
#' @importFrom tidyr complete gather nesting
#' @author Zaid Khan May 2024
module_water_L2233.electricity_water_india <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A23.globalinttech",
             FILE = "energy/A23.subsector_logit",
             FILE = "energy/A23.subsector_shrwt",
             FILE = "energy/A23.subsector_interp",
             FILE = "water/elec_tech_water_map",
             FILE = "water/Exogenous/IndiaElecPassthruShrwtBasinseleccool",
             FILE = "water/Exogenous/IndiaElecReserveeleccool",
             FILE = "water/Exogenous/IndiaPassThroughSectoreleccool",
             FILE = "water/Exogenous/IndiaStubTechCapFactoreleccool",
             FILE = "water/Exogenous/IndiaStubTechEffeleccool",
             FILE = "water/Exogenous/IndiaStubTecheleccool",
             FILE = "water/Exogenous/IndiaStubTechShrwteleccool",
             FILE = "water/Exogenous/IndiaSupplysectorelec",
             FILE = "water/Exogenous/IndiaTechInterpelec",
             FILE = "water/Exogenous/IndiaTechInterpToelec",
             FILE = "water/Exogenous/IndiaTechShrwtFlltelec",
             FILE = "water/Exogenous/IndiaTechShrwtnucelec",
             FILE = "water/Exogenous/IndiaTechShrwtrenewelec",
             FILE = "water/basin_to_country_mapping",
             FILE = "water/A23.CoolingSystemCosts",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.AvgFossilEffKeyword_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.GlobalIntTechBackup_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.GlobalIntTechCapFac_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.GlobalIntTechLifetime_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.GlobalIntTechShrwt_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.GlobalTechCapFac_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.GlobalTechCapture_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.GlobalTechLifetime_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.GlobalTechProfitShutdown_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.GlobalTechSCurve_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.GlobalTechShrwt_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.PrimaryRenewKeyword_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.PrimaryRenewKeywordInt_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.StubTechTrackCapital_elec",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.StubTechCapFactor_elec_cool",
             FILE = "water/Exogenous/elecwatergcamfiles/L2233.StubTechFixOut_hydro",
             "L103.water_mapping_R_B_W_Ws_share",
             "L1233.out_EJ_R_elec_F_tech_Yh_cool",
             "L223.GlobalTechCapital_elec",
             "L223.GlobalIntTechCapital_elec",
             "L223.GlobalTechOMfixed_elec",
             "L223.GlobalIntTechOMfixed_elec",
             "L223.GlobalTechOMvar_elec",
             "L223.GlobalIntTechOMvar_elec",
             "L223.GlobalTechEff_elec",
             "L223.GlobalIntTechEff_elec",
             "L2233.GlobalTechShrwt_elecPassthru",
             "L2233.GlobalTechCoef_elec_cool",
             "L2233.GlobalIntTechCoef_elec_cool"
    ))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2233.DeleteSupplysector_Indiaelec",
             "L2233.DeletePassthroughsector_Indiaelec",
             "L2233.out_EJ_elec_B_F_tech_Yh_cool_India",
             "L2233.IndiaStubTecheleccool",
             "L223.IndiaSectorUseTrialMarket_elec",
             "L2233.IndiaSupplysectorelecLogit",
             "L2233.IndiaBasinsElecMarket",
             "L2233.IndiaBasinelecLogit",
             "L2233.IndiaSubsectorelec_logit",
             "L2233.IndiaSubsectorelec_shrwt",
             "L2233.IndiaSubsectorelec_interp",
             "L223.IndiaGlobalTechCapital_elec",
             "L223.IndiaGlobalIntTechCapital_elec",
             "L223.IndiaGlobalTechOMfixed_elec",
             "L223.IndiaGlobalIntTechOMfixed_elec",
             "L223.IndiaGlobalTechOMvar_elec",
             "L223.IndiaGlobalIntTechOMvar_elec",
             "L223.IndiaSubsector0Shrwt",
             "L2233.Indiaplantelec_logit",
             "L2233.Indiacoolingelec_logit",
             "L2233.IndiaTechShrwtFlltelec",
             "L2233.IndiaTechInterpelec",
             "L2233.IndiaTechInterpToelec",
             "L2233.IndiaTechShrwtnucelec",
             "L2233.IndiaTechShrwtrenewelec",
             "L223.IndiaTechEff_elec",
             "L223.IndiaIntTechEff_elec",
             "L2233.IndiaCSPReserve",
             "L2233.IndiaElecTechShrwtBasinscool",
             "L2233.IndiaStubTechShrwteleccool",
             "L2233.IndiaStubTechCapfactorelecCSP",
             "L2233.IndiaStubTechEffeleccool",
             "L2233.IndiaGlobalIntTechCapital_elec_cool",
             "L2233.IndiaGlobalTechCapital_elec_cool",
             "L2233.IndiaAvgFossilEffKeyword_elec_cool",
             "L2233.IndiaGlobalIntTechBackup_elec_cool",
             "L2233.IndiaGlobalIntTechCapFac_elec_cool",
             "L2233.IndiaGlobalIntTechLifetime_elec_cool",
             "L2233.IndiaGlobalIntTechShrwt_elec_cool",
             "L2233.IndiaGlobalTechCapFac_elec_cool",
             "L2233.IndiaGlobalTechCapture_elec_cool",
             "L2233.IndiaGlobalTechLifetime_elec_cool",
             "L2233.IndiaGlobalTechProfitShutdown_elec_cool",
             "L2233.IndiaGlobalTechSCurve_elec_cool",
             "L2233.IndiaGlobalTechShrwt_elec_cool",
             "L2233.IndiaGlobalTechCoef_elec_cool",
             "L2233.IndiaGlobalIntTechCoef_elec_cool",
             "L2233.IndiaPrimaryRenewKeyword_elec_cool",
             "L2233.IndiaPrimaryRenewKeywordInt_elec_cool",
             "L2233.IndiaBasinsStubTechTrackCapital_elec",
             "L2233.IndiaBasinsStubTechFixOut_hydro"
             ))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    from.supplysector <- from.subsector <- from.technology <- to.supplysector <-
      to.subsector <- to.technology <- year <- share.weight <- supplysector <-
      subsector <- technology <- minicam.energy.input <- calibration <-
      secondary.output <- value <- region <- sector <- calOutputValue <-
      subs.share.weight <- average.grid.capacity.factor <- plant_type <-
      cooling_system <- water_type <- fuel <- sector.name <- subsector.name <-
      input.capital <- capital.overnight_USD2005 <- capital.overnight <-
      water_withdrawals <- water_consumption <- coefficient <- water_sector <-
      share.weight.year <- emiss.coef <- efficiency <- emiss.coeff <- NULL  # silence package check notes

    # Load required inputs
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping", strip_attributes = TRUE)
    A23.subsector_shrwt <- get_data(all_data,"energy/A23.subsector_shrwt", strip_attributes = TRUE)
    A23.subsector_interp <- get_data(all_data,"energy/A23.subsector_interp", strip_attributes = TRUE)
    A23.globalinttech <- get_data(all_data, "energy/A23.globalinttech", strip_attributes = TRUE)
    A23.CoolingSystemCosts <- get_data(all_data, "water/A23.CoolingSystemCosts", strip_attributes = TRUE)
    A23.subsector_logit <- get_data(all_data, "energy/A23.subsector_logit", strip_attributes = TRUE)
    elec_tech_water_map <- get_data(all_data, "water/elec_tech_water_map",strip_attributes = TRUE)
    IndiaElecPassthruShrwtBasinseleccool <- get_data(all_data, "water/Exogenous/IndiaElecPassthruShrwtBasinseleccool",strip_attributes = TRUE)
    IndiaElecReserveeleccool <- get_data(all_data, "water/Exogenous/IndiaElecReserveeleccool",strip_attributes = TRUE)
    IndiaPassThroughSectoreleccool <- get_data(all_data, "water/Exogenous/IndiaPassThroughSectoreleccool",strip_attributes = TRUE)
    IndiaStubTechCapFactoreleccool <- get_data(all_data, "water/Exogenous/IndiaStubTechCapFactoreleccool",strip_attributes = TRUE)
    IndiaStubTechEffeleccool <- get_data(all_data, "water/Exogenous/IndiaStubTechEffeleccool",strip_attributes = TRUE)
    IndiaStubTecheleccool <- get_data(all_data, "water/Exogenous/IndiaStubTecheleccool",strip_attributes = TRUE)
    IndiaStubTechShrwteleccool <- get_data(all_data, "water/Exogenous/IndiaStubTechShrwteleccool",strip_attributes = TRUE)
    IndiaSupplysectorelec <- get_data(all_data, "water/Exogenous/IndiaSupplysectorelec",strip_attributes = TRUE)
    IndiaTechInterpelec <- get_data(all_data, "water/Exogenous/IndiaTechInterpelec",strip_attributes = TRUE)
    IndiaTechInterpToelec <- get_data(all_data, "water/Exogenous/IndiaTechInterpToelec",strip_attributes = TRUE)
    IndiaTechShrwtFlltelec <- get_data(all_data, "water/Exogenous/IndiaTechShrwtFlltelec",strip_attributes = TRUE)
    IndiaTechShrwtnucelec <- get_data(all_data, "water/Exogenous/IndiaTechShrwtnucelec",strip_attributes = TRUE)
    IndiaTechShrwtrenewelec <- get_data(all_data, "water/Exogenous/IndiaTechShrwtrenewelec",strip_attributes = TRUE)
    L103.water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share",strip_attributes = TRUE)
    L1233.out_EJ_R_elec_F_tech_Yh_cool <- get_data(all_data, "L1233.out_EJ_R_elec_F_tech_Yh_cool",strip_attributes = TRUE)
    L223.GlobalTechCapital_elec <- get_data(all_data, "L223.GlobalTechCapital_elec",strip_attributes = TRUE)
    L223.GlobalIntTechCapital_elec <- get_data(all_data, "L223.GlobalIntTechCapital_elec",strip_attributes = TRUE)
    L223.GlobalTechOMfixed_elec <- get_data(all_data, "L223.GlobalTechOMfixed_elec",strip_attributes = TRUE)
    L223.GlobalIntTechOMfixed_elec <- get_data(all_data, "L223.GlobalIntTechOMfixed_elec",strip_attributes = TRUE)
    L223.GlobalTechOMvar_elec <- get_data(all_data, "L223.GlobalTechOMvar_elec",strip_attributes = TRUE)
    L223.GlobalIntTechOMvar_elec <- get_data(all_data, "L223.GlobalIntTechOMvar_elec",strip_attributes = TRUE)
    L223.GlobalTechEff_elec <- get_data(all_data, "L223.GlobalTechEff_elec", strip_attributes = TRUE)
    L223.GlobalIntTechEff_elec <- get_data(all_data, "L223.GlobalIntTechEff_elec", strip_attributes = TRUE)
    L2233.GlobalTechShrwt_elecPassthru <- get_data(all_data, "L2233.GlobalTechShrwt_elecPassthru", strip_attributes = TRUE)
    L2233.AvgFossilEffKeyword_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.AvgFossilEffKeyword_elec_cool", strip_attributes = TRUE)
    L2233.GlobalIntTechBackup_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.GlobalIntTechBackup_elec_cool", strip_attributes = TRUE)
    L2233.GlobalIntTechCapFac_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.GlobalIntTechCapFac_elec_cool", strip_attributes = TRUE)
    L2233.GlobalIntTechLifetime_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.GlobalIntTechLifetime_elec_cool", strip_attributes = TRUE)
    L2233.GlobalIntTechShrwt_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.GlobalIntTechShrwt_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechCapFac_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.GlobalTechCapFac_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechCapture_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.GlobalTechCapture_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechLifetime_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.GlobalTechLifetime_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechProfitShutdown_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.GlobalTechProfitShutdown_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechSCurve_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.GlobalTechSCurve_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechShrwt_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.GlobalTechShrwt_elec_cool", strip_attributes = TRUE)
    L2233.GlobalTechCoef_elec_cool <- get_data(all_data, "L2233.GlobalTechCoef_elec_cool",strip_attributes = TRUE)
    L2233.GlobalIntTechCoef_elec_cool <- get_data(all_data, "L2233.GlobalIntTechCoef_elec_cool",strip_attributes = TRUE)
    L2233.PrimaryRenewKeyword_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.PrimaryRenewKeyword_elec_cool", strip_attributes = TRUE)
    L2233.PrimaryRenewKeywordInt_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.PrimaryRenewKeywordInt_elec_cool", strip_attributes = TRUE)
    L2233.StubTechTrackCapital_elec <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.StubTechTrackCapital_elec", strip_attributes = TRUE)
    L2233.StubTechCapFactor_elec_cool <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.StubTechCapFactor_elec_cool", strip_attributes = TRUE)
    L2233.StubTechFixOut_hydro <- get_data(all_data, "water/Exogenous/elecwatergcamfiles/L2233.StubTechFixOut_hydro", strip_attributes = TRUE)

    #====================================================================================================================================================#

    # Need to delete the power sector in the India region (supplysector)
    # L2233.DeleteSupplysector_Indiaelec: Delete electricity and elect_td_bld supplysectors of the India region to build a new structure that includes basins

    elec_tech_water_map %>%
      select(to.supplysector) -> Supplysector_separated

    Supplysector_separated <- Supplysector_separated %>%  distinct()

      filter(Supplysector_separated, to.supplysector %in% c("electricity", "elect_td_bld")) %>%
        rename (supplysector = to.supplysector) ->
      L2233.DeleteSupplysector_Indiaelec

      L2233.DeleteSupplysector_Indiaelec$region = c("India")

      L2233.DeleteSupplysector_Indiaelec <- L2233.DeleteSupplysector_Indiaelec [c(
        "region", "supplysector")]  #OUTPUT

      #L2233.DeletePassthroughsector_Indiaelec: Delete all the existing passthrough sectors in the India region

      filter(Supplysector_separated, to.supplysector %in% c("elec_CSP", "elec_coal (conv pul)", "elec_coal (IGCC)", "elec_coal (conv pul CCS)",
        "elec_coal (IGCC CCS)", "elec_gas (steam/CT)", "elec_gas (CC)", "elec_gas (CC CCS)", "elec_refined liquids (steam/CT)",
        "elec_refined liquids (CC)", "elec_refined liquids (CC CCS)", "elec_biomass (conv)",
        "elec_biomass (IGCC)", "elec_biomass (conv CCS)", "elec_biomass (IGCC CCS)", "elec_Gen_II_LWR",
        "elec_Gen_III", "elec_CSP_storage", "elec_geothermal")) %>%
        rename (supplysector = to.supplysector) -> L2233.DeletePassthroughsector_Indiaelec

      L2233.DeletePassthroughsector_Indiaelec$region = c("India")

      L2233.DeletePassthroughsector_Indiaelec <- L2233.DeletePassthroughsector_Indiaelec [c(
        "region", "supplysector")]  #OUTPUT


    #Downscaling electricity generation at the basin level for India

    L103.water_mapping_R_B_W_Ws_share %>%
      filter(water_type == "water withdrawals" & water_sector == "Electricity") -> L103.water_mapping_R_B_W_wdraw_share

    L103.water_mapping_R_B_W_wdraw_share %>% select(-water_type) -> L103.water_mapping_R_B_W_wdraw_share

    L103.water_mapping_R_B_W_wdraw_share %>%
      rename(region = GCAM_region_ID) %>%
      filter(region == "17") -> L103.water_mapping_R_B_W_wdraw_share_renamed

    #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
    #L103.water_mapping_R_B_W_wdraw_share_renamed <- L103.water_mapping_R_B_W_wdraw_share_renamed[L103.water_mapping_R_B_W_wdraw_share_renamed$GCAM_basin_ID != "89", ]
    #L103.water_mapping_R_B_W_wdraw_share_renamed <- L103.water_mapping_R_B_W_wdraw_share_renamed[L103.water_mapping_R_B_W_wdraw_share_renamed$GCAM_basin_ID != "116", ]
    #L103.water_mapping_R_B_W_wdraw_share_renamed <- L103.water_mapping_R_B_W_wdraw_share_renamed[L103.water_mapping_R_B_W_wdraw_share_renamed$GCAM_basin_ID != "137", ]
    #Removed comment (not doing this) - Reallocating the remaining of the total share to the other 17 basins in India since Indus and Myanmar are out


    #L103.water_mapping_R_B_W_wdraw_share_renamed %>%
      #mutate(value = sum(share)) %>%
      #mutate(value2 = (1-value)) %>%
      #mutate(share = share+(value2/16)) %>%
      #mutate(value3 = sum(share)) %>%
      #select(-value, -value2, -value3) -> L103.water_mapping_R_B_W_wdraw_share_renamed

    L103.water_mapping_R_B_W_wdraw_share_renamed %>%
      rename(GCAM_region_ID = region) -> L103.water_mapping_R_B_W_wdraw_share_renamed

    L1233.out_EJ_R_elec_F_tech_Yh_cool %>%
    filter(GCAM_region_ID == "17") -> L1233.out_EJ_R_elec_F_tech_Yh_cool_India

    L1233.out_EJ_R_elec_F_tech_Yh_cool_India %>%
      inner_join(L103.water_mapping_R_B_W_wdraw_share_renamed,
                by =  c("GCAM_region_ID")) %>%
      mutate(calOutputValue = value * share) %>%
      select(- water_sector, - share, - value, -plant_type) ->
      L2233.out_EJ_elec_B_F_tech_Yh_cool_India

    #To resolve the calibration error: Multiplying the calOutputValues by 0.704
    L2233.out_EJ_elec_B_F_tech_Yh_cool_India$calOutputValue[L2233.out_EJ_elec_B_F_tech_Yh_cool_India$year == 1990] <- L2233.out_EJ_elec_B_F_tech_Yh_cool_India$calOutputValue[L2233.out_EJ_elec_B_F_tech_Yh_cool_India$year == 1990]*0.704

    #Joining with the original elec-water map to get the desired columns and associated cooling technologies

    L2233.out_EJ_elec_B_F_tech_Yh_cool_India %>%
      rename(from.technology = technology, sector1 = sector) %>%
    inner_join(elec_tech_water_map, by = c("from.technology", "cooling_system", "water_type")) %>%
      select(-to.subsector, -to.supplysector,
             -plant_type, -sector, -fuel,
             -technology, -minicam.energy.input) %>%
      rename(subsector2 = from.technology, stub.technology = technology,
             supplysector = from.supplysector, subsector1 = from.subsector,
             stub.technology = to.technology) %>%
      select(-sector1) %>%
      rename(subsector = subsector2) %>%
      left_join(basin_to_country_mapping, by = c("GCAM_basin_ID")) %>%
      select(-Basin_long_name, -GCAM_basin_ID, -GLU_code, -ISO, -ISO_NUM, -Country_name, -desal, -cooling_system, -water_type, -fuel.x, -fuel.y) %>%
      rename(subsector0 = GLU_name) -> L2233.out_EJ_elec_B_F_tech_Yh_cool_India
      L2233.out_EJ_elec_B_F_tech_Yh_cool_India$GCAM_region_ID[L2233.out_EJ_elec_B_F_tech_Yh_cool_India$GCAM_region_ID == "17"] <- "India"
      L2233.out_EJ_elec_B_F_tech_Yh_cool_India$subsector[L2233.out_EJ_elec_B_F_tech_Yh_cool_India$subsector == "PV"] <- "solar"

      #Filtering for model base years

      L2233.out_EJ_elec_B_F_tech_Yh_cool_India %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
        rename(region = GCAM_region_ID, calOutputValue = value) %>%
        mutate(share.weight.year = year) ->
        L2233.out_EJ_elec_B_F_tech_Yh_cool_India

      #Defining subsector and tech shareweights

      L2233.out_EJ_elec_B_F_tech_Yh_cool_India %>%
        group_by(region, supplysector, subsector0, subsector1, subsector, share.weight.year) %>%
        summarise(value = sum(calOutputValue)) %>% ungroup %>%
        mutate(subs.share.weight = if_else(value > 0, 1, 0)) %>%
        select(-value) -> L2233.out_EJ_elec_B_F_tech_Yh_cool_India_agg

      L2233.out_EJ_elec_B_F_tech_Yh_cool_India %>%
        left_join_error_no_match(L2233.out_EJ_elec_B_F_tech_Yh_cool_India_agg,
                   by = c("share.weight.year", "region", "supplysector", "subsector0", "subsector1", "subsector")) %>%
      mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) ->
      L2233.out_EJ_elec_B_F_tech_Yh_cool_India


      #Defining the order of columns

      L2233.out_EJ_elec_B_F_tech_Yh_cool_India <- L2233.out_EJ_elec_B_F_tech_Yh_cool_India [c(
        "region", "supplysector", "subsector0", "subsector1",
        "subsector", "stub.technology", "year", "calOutputValue", "share.weight.year", "subs.share.weight", "tech.share.weight")]  #OUTPUT


    #CODE FROM HERE ON IS EDITING INPUT CSVs TO CREATE INDIA-LEVEL FILES

    #NEW NESTING STRUCTURE

      #New mapping

    IndiaStubTecheleccool %>%
      rename (to.technology = stub.technology, subsector0 = subsector) %>%
      inner_join(elec_tech_water_map, by = "to.technology") %>%
      select(-to.supplysector, -to.subsector, -supplysector,
             -plant_type, -cooling_system, -water_type, -sector,
             -fuel, -technology, -minicam.energy.input) %>%
      rename (from.subsector = fuel, subsector = from.technology,
              supplysector = from.supplysector, subsector1 = from.subsector,
              stub.technology = to.technology) ->
      L2233.IndiaStubTecheleccool

    #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
    #L2233.IndiaStubTecheleccool <- L2233.IndiaStubTecheleccool[L2233.IndiaStubTecheleccool$subsector0 != "IndusR", ]
    #L2233.IndiaStubTecheleccool <- L2233.IndiaStubTecheleccool[L2233.IndiaStubTecheleccool$subsector0 != "BengalBay", ]
    #L2233.IndiaStubTecheleccool <- L2233.IndiaStubTecheleccool[L2233.IndiaStubTecheleccool$subsector0 != "IrrawaddyR", ]

    L2233.IndiaStubTecheleccool$subsector[L2233.IndiaStubTecheleccool$subsector == 'wind_offshore'] <- 'wind'
    L2233.IndiaStubTecheleccool$subsector[L2233.IndiaStubTecheleccool$subsector == 'wind_storage'] <- 'wind'
    L2233.IndiaStubTecheleccool$subsector[L2233.IndiaStubTecheleccool$subsector == 'PV'] <- 'solar'
    L2233.IndiaStubTecheleccool$subsector[L2233.IndiaStubTecheleccool$subsector == 'PV_storage'] <- 'solar'

      L2233.IndiaStubTecheleccool <- L2233.IndiaStubTecheleccool[c(
        "region", "supplysector", "subsector0", "subsector1",
        "subsector", "stub.technology")]  #OUTPUT


      #PROVIDING INFORMATION FOR THE BASINS (NESTING.SUBSECTOR)

      #Defining marginal revenue market & sector information

      IndiaPassThroughSectoreleccool %>%
        rename(subsector0 = subsector, to.supplysector = pass.through.sector) %>%
        inner_join(elec_tech_water_map, by = "to.supplysector") %>%
        select(-from.subsector, -from.technology,
               -to.subsector, -to.technology, -plant_type,
               -cooling_system, -water_type, -sector, -fuel, -technology,
               -minicam.energy.input) %>%
        select(-to.supplysector) %>%
        rename(supplysector = from.supplysector) -> L2233.IndiaBasinsElecMarket

      L2233.IndiaBasinsElecMarket <- L2233.IndiaBasinsElecMarket %>% distinct()

      #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
      #L2233.IndiaBasinsElecMarket <- L2233.IndiaBasinsElecMarket[L2233.IndiaBasinsElecMarket$subsector0 != "IndusR", ]
      #L2233.IndiaBasinsElecMarket <- L2233.IndiaBasinsElecMarket[L2233.IndiaBasinsElecMarket$subsector0 != "BengalBay", ]
      #L2233.IndiaBasinsElecMarket <- L2233.IndiaBasinsElecMarket[L2233.IndiaBasinsElecMarket$subsector0 != "IrrawaddyR", ]

      L2233.IndiaBasinsElecMarket <- L2233.IndiaBasinsElecMarket [c(
        "region", "supplysector", "subsector0",
        "marginal.revenue.sector", "marginal.revenue.market")]  #OUTPUT


      #Create a trial market to help with simultaneities related to electricity

      L223.IndiaSectorUseTrialMarket_elec <- L2233.IndiaBasinsElecMarket %>%
        select(region, supplysector, subsector0) %>%
        mutate(use.trial.market = 1)

      #Correcting units for price.unit in Supplysectorelec file

      IndiaSupplysectorelec$price.unit[IndiaSupplysectorelec$price.unit == '1975'] <- '1975$/GJ'


      #Defining logit coefficients at the supplysector and basin level

      IndiaSupplysectorelec %>%
        select(-subsector) %>%
        distinct() ->
        L2233.IndiaSupplysectorelecLogit

      IndiaSupplysectorelec %>%
      rename(nesting.subsector = subsector) %>%
      select(-output.unit, -input.unit, -price.unit) -> L2233.IndiaBasinelecLogit

      #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
      #L2233.IndiaBasinelecLogit <- L2233.IndiaBasinelecLogit[L2233.IndiaBasinelecLogit$nesting.subsector != "IndusR", ]
      #L2233.IndiaBasinelecLogit <- L2233.IndiaBasinelecLogit[L2233.IndiaBasinelecLogit$nesting.subsector != "BengalBay", ]
      #L2233.IndiaBasinelecLogit <- L2233.IndiaBasinelecLogit[L2233.IndiaBasinelecLogit$nesting.subsector != "IrrawaddyR", ]


      L2233.IndiaBasinelecLogit <- L2233.IndiaBasinelecLogit[c(
         "region", "supplysector", "nesting.subsector", "logit.year.fillout", "logit.exponent", "logit.type")]  #OUTPUT


      #Separating basin level map for later use

      L2233.IndiaBasinsElecMarket %>%
        select(-supplysector, -marginal.revenue.sector, -marginal.revenue.market) ->
        L2233.IndiaBasins


      #FUEL (SUBSECTOR) LEVEL INFORMATION

      #Logit

      L2233.IndiaStubTecheleccool %>%
        select(-subsector, -stub.technology) %>%
        rename(subsector = subsector1)->
        L2233.IndiaStubTecheleccool_fuel

      L2233.IndiaStubTecheleccool_fuel %>%
        inner_join(A23.subsector_logit, by = "subsector") %>%
        select(-supplysector.y) %>%
        rename(supplysector = supplysector.x) %>%
        mutate(logit.year.fillout = '1975', logit.type = 'relative.cost.logit') %>%
        distinct()->
        L2233.IndiaSubsectorelec_logit

      L2233.IndiaSubsectorelec_logit <- L2233.IndiaSubsectorelec_logit[c(
        "region", "supplysector", "subsector0", "subsector", "logit.year.fillout", "logit.exponent", "logit.type")]  #OUTPUT


      #Share-weights

      L2233.IndiaStubTecheleccool_fuel %>%
        inner_join(A23.subsector_shrwt, by = "subsector") %>%
        select(-supplysector.y, -year) %>%
        rename(supplysector = supplysector.x) %>%
        distinct()->
        L2233.IndiaSubsectorelec_shrwt

      L2233.IndiaSubsectorelec_shrwt$year.fillout[L2233.IndiaSubsectorelec_shrwt$year.fillout == 'start-year'] <- '1975'

      #OUTPUT

      #Interpolation rules

      L2233.IndiaStubTecheleccool_fuel %>%
        inner_join(A23.subsector_interp, by = "subsector") %>%
        select(-supplysector.y) %>%
        rename(supplysector = supplysector.x) %>%
        distinct()->
        L2233.IndiaSubsectorelec_interp

      L2233.IndiaSubsectorelec_interp$to.value <- L2233.IndiaSubsectorelec_interp$to.value %>% replace_na(1)
      L2233.IndiaSubsectorelec_interp$from.year[L2233.IndiaSubsectorelec_interp$from.year == 'final-calibration-year'] <- '2015'
      L2233.IndiaSubsectorelec_interp$from.year[L2233.IndiaSubsectorelec_interp$from.year == 'initial-future-year'] <- '2020'
      L2233.IndiaSubsectorelec_interp$to.year[L2233.IndiaSubsectorelec_interp$to.year == 'initial-future-year'] <- '2020'
      #OUTPUT


      #POWER PLANT (SUBSECTOR) LEVEL INFORMATION

      #Costs - Capital

      #Standard technologies

      L223.IndiaGlobalTechCapital_elec <- L223.GlobalTechCapital_elec   #OUTPUT

      #Intermittent technologies

      L223.IndiaGlobalIntTechCapital_elec <- L223.GlobalIntTechCapital_elec
      # now remove rooftop_pv from the global tech to avoid double accounting
      L223.IndiaGlobalIntTechCapital_elec <- slice(L223.IndiaGlobalIntTechCapital_elec, c(23:n()))

      #Costs - O&M Fixed

      #Standard technologies

      L223.IndiaGlobalTechOMfixed_elec <- L223.GlobalTechOMfixed_elec    #OUTPUT

      #Intermittent technologies

      L223.IndiaGlobalIntTechOMfixed_elec <- L223.GlobalIntTechOMfixed_elec   #OUTPUT

      #Costs - O&M Variable

      #Standard technologies

      L223.IndiaGlobalTechOMvar_elec <- L223.GlobalTechOMvar_elec   #OUTPUT

      #Intermittent technologies

      L223.IndiaGlobalIntTechOMvar_elec <- L223.GlobalIntTechOMvar_elec   #OUTPUT


      #Defining historical and future Shareweights at the powerplant level (Subsector0)

      L2233.GlobalTechShrwt_elecPassthru$region  <- "India"
      L2233.GlobalTechShrwt_elecPassthru_2 <- L2233.GlobalTechShrwt_elecPassthru %>%
        distinct()

      L2233.GlobalTechShrwt_elecPassthru_2 %>%
        inner_join(L2233.IndiaBasins, by = "region") %>%
        rename(supplysector = sector.name, subsector1 = subsector.name,
               subsector = technology) -> L223.IndiaSubsector0Shrwt
      L223.IndiaSubsector0Shrwt <- L223.IndiaSubsector0Shrwt[c("region", "supplysector", "subsector0",
                                 "subsector1", "subsector", "year", "share.weight")]  #OUTPUT


      #Defining Sharweight Fillout at the Subsector level (Power Plant level)

      IndiaTechShrwtFlltelec %>%
        rename(to.subsector = technology, subsector0 = subsector) %>%
        inner_join(elec_tech_water_map, by = "to.subsector") %>%
        select(-from.supplysector, -to.supplysector,
               -from.technology, -to.technology, -plant_type,
               -cooling_system, -water_type, -sector, -fuel,
               -technology, -minicam.energy.input) %>%
        rename (supplysector = from.supplysector, subsector1 = from.subsector,
                subsector = to.subsector) -> L2233.IndiaTechShrwtFlltelec

      L2233.IndiaTechShrwtFlltelec <- L2233.IndiaTechShrwtFlltelec %>%
        distinct()

      #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
      #L2233.IndiaTechShrwtFlltelec <- L2233.IndiaTechShrwtFlltelec[L2233.IndiaTechShrwtFlltelec$subsector0 != "IndusR", ]
      #L2233.IndiaTechShrwtFlltelec <- L2233.IndiaTechShrwtFlltelec[L2233.IndiaTechShrwtFlltelec$subsector0 != "BengalBay", ]
      #L2233.IndiaTechShrwtFlltelec <- L2233.IndiaTechShrwtFlltelec[L2233.IndiaTechShrwtFlltelec$subsector0 != "IrrawaddyR", ]


      L2233.IndiaTechShrwtFlltelec <- L2233.IndiaTechShrwtFlltelec[c
                  ("region", "supplysector", "subsector0",
  "subsector1", "subsector", "year.fillout", "share.weight")]    #OUTPUT

      #DEFINING SHRWT INTERPOLATION RULE AT THE POWER PLANT LEVEL

      IndiaTechInterpelec %>%
        rename(from.technology = technology, subsector0 = subsector) %>%
        inner_join(elec_tech_water_map, by = "from.technology") %>%
        select(-from.supplysector, -to.supplysector, -to.subsector, -to.technology, -plant_type,
               -cooling_system, -water_type, -sector, -fuel,
               -technology, -minicam.energy.input) %>%
        rename (supplysector = from.supplysector, subsector1 = from.subsector,
                subsector = from.technology) -> L2233.IndiaTechInterpelec

      L2233.IndiaTechInterpelec$subsector <- sub("^(wind_).*", "wind", L2233.IndiaTechInterpelec$subsector)
      L2233.IndiaTechInterpelec$subsector <- sub("^(PV).*", "solar", L2233.IndiaTechInterpelec$subsector)

      L2233.IndiaTechInterpelec <- L2233.IndiaTechInterpelec %>%
        distinct()

      #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
      #L2233.IndiaTechInterpelec <- L2233.IndiaTechInterpelec[L2233.IndiaTechInterpelec$subsector0 != "IndusR", ]
      #L2233.IndiaTechInterpelec <- L2233.IndiaTechInterpelec[L2233.IndiaTechInterpelec$subsector0 != "BengalBay", ]
      #L2233.IndiaTechInterpelec <- L2233.IndiaTechInterpelec[L2233.IndiaTechInterpelec$subsector0 != "IrrawaddyR", ]

      #Removing nuclear sharweight details since they are already added in the next file being generated
      L2233.IndiaTechInterpelec <- L2233.IndiaTechInterpelec[L2233.IndiaTechInterpelec$subsector1 != "nuclear", ]

          L2233.IndiaTechInterpelec <- L2233.IndiaTechInterpelec[c
            ("region", "supplysector", "subsector0",
    "subsector1", "subsector", "apply.to", "from.year", "to.year",
                "interpolation.function")]   #OUTPUT


      #DEFINING LONG-TERM SHRWT INTERPOLATION RULES AT THE POWER PLANT LEVEL

      IndiaTechInterpToelec %>%
        rename(from.technology = technology, subsector0 = subsector) %>%
        inner_join(elec_tech_water_map, by = "from.technology") %>%
        select(-from.supplysector, -to.supplysector, -to.subsector, -to.technology, -plant_type,
               -cooling_system, -water_type, -sector, -fuel,
               -technology, -minicam.energy.input) %>%
        rename (supplysector = from.supplysector, subsector1 = from.subsector,
                subsector = from.technology) -> L2233.IndiaTechInterpToelec

      #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
      #L2233.IndiaTechInterpToelec <- L2233.IndiaTechInterpToelec[L2233.IndiaTechInterpToelec$subsector0 != "IndusR", ]
      #L2233.IndiaTechInterpToelec <- L2233.IndiaTechInterpToelec[L2233.IndiaTechInterpToelec$subsector0 != "BengalBay", ]
      #L2233.IndiaTechInterpToelec <- L2233.IndiaTechInterpToelec[L2233.IndiaTechInterpToelec$subsector0 != "IrrawaddyR", ]


      L2233.IndiaTechInterpToelec <- L2233.IndiaTechInterpToelec[c
          ("region", "supplysector", "subsector0",
  "subsector1", "subsector", "apply.to", "from.year", "to.year",
                  "interpolation.function")]

      L2233.IndiaTechInterpToelec <- L2233.IndiaTechInterpToelec %>%
        distinct()   #OUTPUT

      #Combining the two shareweight files

      L2233.IndiaTechInterpToelec <- rbind(L2233.IndiaTechInterpToelec, L2233.IndiaTechInterpelec)

      L2233.IndiaTechInterpToelec <- L2233.IndiaTechInterpToelec %>%
        distinct()

      L2233.IndiaTechInterpToelec$to.value <- "1"

      L2233.IndiaTechInterpToelec <- L2233.IndiaTechInterpToelec[c
                                  ("region", "supplysector", "subsector0",
                                  "subsector1", "subsector", "apply.to", "from.year", "to.year",
                                    "to.value", "interpolation.function")]    #OUTPUT

      #DEFINING NUCLEAR TECHNOLOGY LEVEL SHAREWEIGHT

      IndiaTechShrwtnucelec %>%
        rename(from.technology = technology, subsector0 = subsector) %>%
        inner_join(elec_tech_water_map, by = "from.technology") %>%
        select(-from.supplysector, -to.supplysector,
               -to.subsector, -plant_type, -to.technology,
               -cooling_system, -water_type, -sector, -fuel,
               -technology, -minicam.energy.input) %>%
        rename (supplysector = from.supplysector, subsector1 = from.subsector,
                subsector = from.technology) -> L2233.IndiaTechShrwtnucelec

      L2233.IndiaTechShrwtnucelec <- L2233.IndiaTechShrwtnucelec %>% distinct()

      #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
      #L2233.IndiaTechShrwtnucelec <- L2233.IndiaTechShrwtnucelec[L2233.IndiaTechShrwtnucelec$subsector0 != "IndusR", ]
      #L2233.IndiaTechShrwtnucelec <- L2233.IndiaTechShrwtnucelec[L2233.IndiaTechShrwtnucelec$subsector0 != "BengalBay", ]
      #L2233.IndiaTechShrwtnucelec <- L2233.IndiaTechShrwtnucelec[L2233.IndiaTechShrwtnucelec$subsector0 != "IrrawaddyR", ]


      L2233.IndiaTechShrwtnucelec <- L2233.IndiaTechShrwtnucelec [c
              ("region", "supplysector", "subsector0",
               "subsector1", "subsector",
               "apply.to", "from.year", "to.year", "from.value",
               "to.value", "interpolation.function")]   #OUTPUT


      #DEFINING RENEWABLE-BASED TECHNOLOGIES SHRWT UNTIL 2050 & 2100

      IndiaTechShrwtrenewelec %>%
        rename(to.subsector = technology, subsector0 = subsector) %>%
        inner_join(elec_tech_water_map, by = "to.subsector") %>%
        select(-from.supplysector, -from.technology, -to.supplysector,
               -to.technology, -plant_type, -cooling_system,
               -water_type, -sector, -fuel,
               -technology, -minicam.energy.input) %>%
        rename (supplysector = from.supplysector, subsector1 = from.subsector ,subsector = to.subsector) ->
        L2233.IndiaTechShrwtrenewelec

      L2233.IndiaTechShrwtrenewelec <- L2233.IndiaTechShrwtrenewelec %>%
        distinct()

      #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
      #L2233.IndiaTechShrwtrenewelec <- L2233.IndiaTechShrwtrenewelec[L2233.IndiaTechShrwtrenewelec$subsector0 != "IndusR", ]
      #L2233.IndiaTechShrwtrenewelec <- L2233.IndiaTechShrwtrenewelec[L2233.IndiaTechShrwtrenewelec$subsector0 != "BengalBay", ]
      #L2233.IndiaTechShrwtrenewelec <- L2233.IndiaTechShrwtrenewelec[L2233.IndiaTechShrwtrenewelec$subsector0 != "IrrawaddyR", ]

      L2233.IndiaTechShrwtrenewelec <- L2233.IndiaTechShrwtrenewelec [c
                                      ("region", "supplysector", "subsector0",
                                       "subsector1", "subsector",
                                       "year", "share.weight")]   #OUTPUT


      #Defining Logit information at the power plant level

      L2233.IndiaSubsectorelec_logit %>%
        rename(subsector1 = subsector) %>%
        inner_join(L2233.IndiaStubTecheleccool, by = "subsector1") %>%
        select(-region.y, -supplysector.y, -subsector0.y, -stub.technology) %>%
        rename(region = region.x, supplysector = supplysector.x, subsector0 = subsector0.x) %>%
        distinct() %>%
        mutate(logit.year.fillout = '1975') -> L2233.Indiaplantelec_logit

      L2233.Indiaplantelec_logit <- L2233.Indiaplantelec_logit [c
                                  ("region", "supplysector", "subsector0",
                                 "subsector1", "subsector", "logit.year.fillout", "logit.exponent",
                                  "logit.type")]  #OUTPUT

      #Defining Logit information at the cooling tech level

      L2233.Indiaplantelec_logit %>%
      inner_join(L2233.IndiaStubTecheleccool, by = "subsector") %>%
        select(-region.y, -supplysector.y, -subsector0.y, -subsector1.y) %>%
        rename(region = region.x, supplysector = supplysector.x, subsector0 = subsector0.x, subsector1 = subsector1.x) %>%
        distinct() -> L2233.Indiacoolingelec_logit

      L2233.Indiacoolingelec_logit <- L2233.Indiacoolingelec_logit [c
                                                                ("region", "supplysector", "subsector0",
                                                                  "subsector1", "subsector", "stub.technology", "logit.year.fillout", "logit.exponent",
                                                                  "logit.type")]  #OUTPUT


      #Efficiency

      #Standard technologies

      L223.GlobalTechEff_elec$region  <- "India"
      L223.GlobalTechEff_elec %>%
        inner_join(L2233.IndiaBasins, by = "region") %>%
        rename(supplysector = sector.name, subsector1 = subsector.name,
               subsector = technology) -> L223.IndiaTechEff_elec
      L223.IndiaTechEff_elec <- L223.IndiaTechEff_elec[c("region", "supplysector", "subsector0",
         "subsector1", "subsector", "year", "minicam.energy.input",
         "efficiency")]   #OUTPUT


      #Intermittent technologies

      L223.GlobalIntTechEff_elec$region  <- "India"
      L223.GlobalIntTechEff_elec %>%
        inner_join(L2233.IndiaBasins, by = "region") %>%
        rename(supplysector = sector.name, subsector1 = subsector.name,
               subsector = intermittent.technology) -> L223.IndiaIntTechEff_elec
      L223.IndiaIntTechEff_elec <- L223.IndiaIntTechEff_elec[c("region", "supplysector", "subsector0",
        "subsector1", "subsector", "year", "minicam.energy.input",
            "efficiency", "type")]  #OUTPUT


      #DEFINING RESERVE MARGIN & AVG GRID FACTOR FOR CSP TECH

      IndiaElecReserveeleccool %>%
        rename (to.supplysector = supplysector, subsector0 = subsector ) %>%
        inner_join(elec_tech_water_map, by = "to.supplysector") %>%
        select(-to.subsector, -to.technology, -plant_type,
               -cooling_system, -water_type, -sector, -fuel, -technology,
               -minicam.energy.input) %>%
        select(-to.supplysector) %>%
        rename(subsector1 = from.subsector, subsector = from.technology, supplysector = from.supplysector) -> L2233.IndiaCSPReserve

      L2233.IndiaCSPReserve <- L2233.IndiaCSPReserve %>% distinct()

       #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
      #L2233.IndiaCSPReserve <- L2233.IndiaCSPReserve[L2233.IndiaCSPReserve$subsector0 != "IndusR", ]
      #L2233.IndiaCSPReserve <- L2233.IndiaCSPReserve[L2233.IndiaCSPReserve$subsector0 != "BengalBay", ]
      #L2233.IndiaCSPReserve <- L2233.IndiaCSPReserve[L2233.IndiaCSPReserve$subsector0 != "IrrawaddyR", ]


      L2233.IndiaCSPReserve <- L2233.IndiaCSPReserve[c(
        "region", "supplysector", "subsector0", "subsector1", "subsector",
        "electricity.reserve.margin", "average.grid.capacity.factor")] #OUTPUT


      #COOLING TECH LEVEL (STUB.TECHNOLOGY) INFORMATION

      #Defining historical shrwt at the cooling tech level

    IndiaElecPassthruShrwtBasinseleccool %>%
    rename (to.technology = stub.technology, subsector0 = subsector) %>%
      inner_join(elec_tech_water_map, by = "to.technology") %>%
                               select(-fuel, -from.technology, -plant_type,
                               -to.supplysector, -supplysector, -cooling_system, -water_type, -sector, -technology,
                               -minicam.energy.input) %>%
      rename(subsector1 = from.subsector, supplysector = from.supplysector,
             subsector = to.subsector, stub.technology = to.technology) -> L2233.IndiaElecTechShrwtBasinscool

    #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
    #L2233.IndiaElecTechShrwtBasinscool <- L2233.IndiaElecTechShrwtBasinscool[L2233.IndiaElecTechShrwtBasinscool$subsector0 != "IndusR", ]
    #L2233.IndiaElecTechShrwtBasinscool <- L2233.IndiaElecTechShrwtBasinscool[L2233.IndiaElecTechShrwtBasinscool$subsector0 != "BengalBay", ]
    #L2233.IndiaElecTechShrwtBasinscool <- L2233.IndiaElecTechShrwtBasinscool[L2233.IndiaElecTechShrwtBasinscool$subsector0 != "IrrawaddyR", ]


     L2233.IndiaElecTechShrwtBasinscool <- L2233.IndiaElecTechShrwtBasinscool[c(
      "region", "supplysector", "subsector0", "subsector1",
      "subsector", "stub.technology", "year",
      "share.weight.year", "tech.share.weight")] #OUTPUT


    #DEFINING COOLING TECH LEVEL SHAREWEIGHTS FOR FUTURE YEARS

    IndiaStubTechShrwteleccool %>%
      rename (to.technology = stub.technology, subsector0 = subsector) %>%
      inner_join(elec_tech_water_map, by = "to.technology") %>%
      select(-to.subsector,
             -plant_type, -cooling_system, -water_type, -sector,
             -fuel, -technology, -minicam.energy.input) %>%
      select(-supplysector, -to.supplysector) %>%
      rename (subsector1 = from.subsector, supplysector = from.supplysector,
              subsector = from.technology, stub.technology = to.technology) -> L2233.IndiaStubTechShrwteleccool

    #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
    #L2233.IndiaStubTechShrwteleccool <- L2233.IndiaStubTechShrwteleccool[L2233.IndiaStubTechShrwteleccool$subsector0 != "IndusR", ]
    #L2233.IndiaStubTechShrwteleccool <- L2233.IndiaStubTechShrwteleccool[L2233.IndiaStubTechShrwteleccool$subsector0 != "BengalBay", ]
    #L2233.IndiaStubTechShrwteleccool <- L2233.IndiaStubTechShrwteleccool[L2233.IndiaStubTechShrwteleccool$subsector0 != "IrrawaddyR", ]


    L2233.IndiaStubTechShrwteleccool <- L2233.IndiaStubTechShrwteleccool[c
              ("region", "supplysector", "subsector0",
   "subsector1", "subsector", "stub.technology", "year", "share.weight")]  #OUTPUT


    #DEFINING CSP RELATED CAPACITY FACTORS

    IndiaStubTechCapFactoreleccool %>%
      rename (to.supplysector = supplysector, subsector0 = subsector)%>%
      inner_join(elec_tech_water_map, by = "to.supplysector") %>%
                        select(-plant_type, -cooling_system, -water_type,
                               -technology, -sector, -fuel, -minicam.energy.input) %>%
      rename(subsector1 = from.subsector, supplysector = from.supplysector, subsector = to.subsector) %>%
      select (-from.technology, -to.technology, -to.supplysector) -> L2233.IndiaStubTechCapfactorelecCSP

      L2233.IndiaStubTechCapfactorelecCSP <- L2233.IndiaStubTechCapfactorelecCSP %>% distinct()

      #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
      #L2233.IndiaStubTechCapfactorelecCSP <- L2233.IndiaStubTechCapfactorelecCSP[L2233.IndiaStubTechCapfactorelecCSP$subsector0 != "IndusR", ]
      #L2233.IndiaStubTechCapfactorelecCSP <- L2233.IndiaStubTechCapfactorelecCSP[L2233.IndiaStubTechCapfactorelecCSP$subsector0 != "BengalBay", ]
      #L2233.IndiaStubTechCapfactorelecCSP <- L2233.IndiaStubTechCapfactorelecCSP[L2233.IndiaStubTechCapfactorelecCSP$subsector0 != "IrrawaddyR", ]


      L2233.IndiaStubTechCapfactorelecCSP  <- L2233.IndiaStubTechCapfactorelecCSP[c
                                    ("region", "supplysector", "subsector0", "subsector1", "subsector", "stub.technology",
                                      "year", "capacity.factor")] #OUTPUT


    #DEFINING EFFICIENCY AT THE COOLING TECH LEVEL FOR HISTORICAL YEARS

    IndiaStubTechEffeleccool %>%
      rename (to.supplysector = supplysector, subsector0 = subsector) %>%
      inner_join(elec_tech_water_map, by = "to.supplysector") %>%
                        select(-from.subsector, -from.technology, -plant_type,
                               -cooling_system, -water_type, -sector, -technology,
                               -minicam.energy.input.y) %>%
      rename(minicam.energy.input = minicam.energy.input.x, subsector = to.subsector,
             supplysector = from.supplysector, subsector1 = fuel) %>%
      select (-to.supplysector, -to.technology) -> L2233.IndiaStubTechEffeleccool

    #Removing Indus, Irrawaddy and BengalBay basins since they are mapped to Pakistan and Myanmar respectively
    #L2233.IndiaStubTechEffeleccool <- L2233.IndiaStubTechEffeleccool[L2233.IndiaStubTechEffeleccool$subsector0 != "IndusR", ]
    #L2233.IndiaStubTechEffeleccool <- L2233.IndiaStubTechEffeleccool[L2233.IndiaStubTechEffeleccool$subsector0 != "BengalBay", ]
    #L2233.IndiaStubTechEffeleccool <- L2233.IndiaStubTechEffeleccool[L2233.IndiaStubTechEffeleccool$subsector0 != "IrrawaddyR", ]


    L2233.IndiaStubTechEffeleccool <- L2233.IndiaStubTechEffeleccool %>% distinct()

    #To resolve the calibration error: Multiplying the calOutputValues by 0.704
    L2233.IndiaStubTechEffeleccool$efficiency[L2233.IndiaStubTechEffeleccool$year == 1990] <- L2233.IndiaStubTechEffeleccool$efficiency[L2233.IndiaStubTechEffeleccool$year == 1990]*0.704

    L2233.IndiaStubTechEffeleccool <- L2233.IndiaStubTechEffeleccool[c
          ("region", "supplysector", "subsector0",
           "subsector1", "subsector", "stub.technology", "year",
           "minicam.energy.input", "efficiency", "market.name")]  #OUTPUT


      # CAPITAL COSTS OF COOLING SYSTEMS FOR INTERMITTENT AND STANDARD TECHNOLOGIES

      A23.CoolingSystemCosts %>%
        gather_years %>%
        complete(nesting(cooling_system, input.capital),
                 year = c(year, MODEL_YEARS)) %>%
        group_by(cooling_system, input.capital) %>%
        mutate(value = approx_fun(year, value)) %>% ungroup() %>%
        filter(year %in% MODEL_YEARS) %>% rename(capital.overnight_USD2005 = value) %>%
        mutate(capital.overnight = round(capital.overnight_USD2005 * gdp_deflator(1975, 2005), 0)) ->
        L2233.CoolingSystemCosts

      elec_tech_water_map %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        left_join_error_no_match(L2233.CoolingSystemCosts, by = c("cooling_system", "year")) %>%
        mutate(fixed.charge.rate = water.COOLING_SYSTEM_FCR, capacity.factor = water.COOLING_SYSTEM_CAPACITY_FACTOR) %>%
        select(-technology) %>%
        rename(sector.name = to.supplysector,
               subsector.name = to.subsector,
               technology = to.technology) %>%
        filter(capital.overnight > 0) ->
        L2233.GlobalTechCapital_elec_cool_all # includes both standard and intermittent.


      # L2233.GlobalTechCapital_elec_cool_all needs to be partitioned into standard into intermittent techs
      L2233.GlobalTechCapital_elec_cool_all %>%
        filter(from.supplysector %in% A23.globalinttech$supplysector &
                 from.subsector %in% A23.globalinttech$subsector &
                 from.technology %in% A23.globalinttech$technology) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]]) ->
        L2233.GlobalIntTechCapital_elec_cool

      L2233.GlobalIntTechCapital_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.GlobalIntTechCapital_elec_cool$sector.name)

      L2233.IndiaGlobalIntTechCapital_elec_cool <- L2233.GlobalIntTechCapital_elec_cool #OUTPUT


      L2233.GlobalTechCapital_elec_cool_all %>%
        filter(!(from.supplysector %in% A23.globalinttech$supplysector &
                   from.subsector %in% A23.globalinttech$subsector &
                   from.technology %in% A23.globalinttech$technology)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]]) ->
        L2233.GlobalTechCapital_elec_cool


      L2233.GlobalTechCapital_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.GlobalTechCapital_elec_cool$sector.name)

      L2233.IndiaGlobalTechCapital_elec_cool <- L2233.GlobalTechCapital_elec_cool  #OUTPUT


      #AVERAGE FOSSIL EFFICIENCY FOR COOLING TECHS

      L2233.AvgFossilEffKeyword_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.AvgFossilEffKeyword_elec_cool$sector.name)

      L2233.IndiaAvgFossilEffKeyword_elec_cool <- L2233.AvgFossilEffKeyword_elec_cool #OUTPUT


      #BACKUP INFO FOR INTERMITTENT COOLING TECHS

      L2233.GlobalIntTechBackup_elec_cool$sector.name[L2233.GlobalIntTechBackup_elec_cool$sector.name == "elec_CSP"] <- 'electricity'

      L2233.IndiaGlobalIntTechBackup_elec_cool <- L2233.GlobalIntTechBackup_elec_cool


      #CAPACITY FACTOR INFORMATION INTERMITTENT COOLING TECHS

      L2233.GlobalIntTechCapFac_elec_cool$sector.name[L2233.GlobalIntTechCapFac_elec_cool$sector.name == "elec_CSP"] <- 'electricity'

      L2233.IndiaGlobalIntTechCapFac_elec_cool <- L2233.GlobalIntTechCapFac_elec_cool


      #LIFETIME INFORMATION FOR INTERMITTENT COOLING TECHS

      L2233.GlobalIntTechLifetime_elec_cool$sector.name[L2233.GlobalIntTechLifetime_elec_cool$sector.name == "elec_CSP"] <- 'electricity'

      L2233.IndiaGlobalIntTechLifetime_elec_cool <- L2233.GlobalIntTechLifetime_elec_cool

      #SHRWT INFORMATION FOR INTERMITTENT COOLING TECHS

      L2233.GlobalIntTechShrwt_elec_cool$sector.name[L2233.GlobalIntTechShrwt_elec_cool$sector.name == "elec_CSP"] <- 'electricity'

      L2233.IndiaGlobalIntTechShrwt_elec_cool <- L2233.GlobalIntTechShrwt_elec_cool

      #CAPACITY FACTOR INFORMATION FOR CONVENTIONAL COOLING TECHS

      L2233.GlobalTechCapFac_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.GlobalTechCapFac_elec_cool$sector.name)

      L2233.IndiaGlobalTechCapFac_elec_cool <- L2233.GlobalTechCapFac_elec_cool

      #CAPTURE INFORMATION FOR CONV COOLING TECHS

      L2233.GlobalTechCapture_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.GlobalTechCapture_elec_cool$sector.name)

      L2233.IndiaGlobalTechCapture_elec_cool <- L2233.GlobalTechCapture_elec_cool

      #LIFETIME INFORMATION FOR CONV COOLING TECHS

      L2233.GlobalTechLifetime_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.GlobalTechLifetime_elec_cool$sector.name)

      L2233.IndiaGlobalTechLifetime_elec_cool <- L2233.GlobalTechLifetime_elec_cool

      #PROFIT SHUTDOWN INFORMATION FOR CONV COOLING TECHS

      L2233.GlobalTechProfitShutdown_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.GlobalTechProfitShutdown_elec_cool$sector.name)

      L2233.IndiaGlobalTechProfitShutdown_elec_cool <- L2233.GlobalTechProfitShutdown_elec_cool

      #S-CURVE INFORMATION FOR CONV COOLING TECHS

      L2233.GlobalTechSCurve_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.GlobalTechSCurve_elec_cool$sector.name)

      L2233.IndiaGlobalTechSCurve_elec_cool <- L2233.GlobalTechSCurve_elec_cool

      #SHRWT INFORMATION FOR CONV COOLING TECHS

      L2233.GlobalTechShrwt_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.GlobalTechShrwt_elec_cool$sector.name)

      L2233.IndiaGlobalTechShrwt_elec_cool <- L2233.GlobalTechShrwt_elec_cool

      #WATER CONSUMPTION AND WITHDRAWAL COEFFICIENTS FOR COOLING TECHS

      L2233.GlobalTechCoef_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.GlobalTechCoef_elec_cool$sector.name)

      L2233.IndiaGlobalTechCoef_elec_cool <- L2233.GlobalTechCoef_elec_cool


      L2233.GlobalIntTechCoef_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.GlobalIntTechCoef_elec_cool$sector.name)

      L2233.IndiaGlobalIntTechCoef_elec_cool <- L2233.GlobalIntTechCoef_elec_cool


      #ALTERATIONS TO THE WATER COEFFICIENT FILES TO MAKE THEM EXOGENOUSLY COMPATIBLE ONLY FOR INDIA
      #Conventional techs
      #Add region
      L2233.IndiaGlobalTechCoef_elec_cool2 <- L2233.IndiaGlobalTechCoef_elec_cool

      L2233.IndiaGlobalTechCoef_elec_cool2$region <- "India"
      #Add Basins
      L2233.IndiaGlobalTechCoef_elec_cool2 %>%
        inner_join(L2233.IndiaBasins, by = "region") %>%
        rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) ->
        L2233.IndiaGlobalTechCoef_elec_cool2
      #Add the new structure to this
      L2233.IndiaGlobalTechCoef_elec_cool2 %>%
      inner_join(L2233.IndiaStubTecheleccool, by = "subsector") -> L2233.IndiaGlobalTechCoef_elec_cool2
      L2233.IndiaGlobalTechCoef_elec_cool2 %>%
       rename(region = region.x, supplysector = supplysector.x, subsector0 = subsector0.x,
               stub.technology = stub.technology.x) %>%
        select(region, supplysector, subsector0, subsector1, subsector, stub.technology, year, minicam.energy.input, coefficient)->
        L2233.IndiaGlobalTechCoef_elec_cool2
      #Remove repetitions
      L2233.IndiaGlobalTechCoef_elec_cool2 <- L2233.IndiaGlobalTechCoef_elec_cool2 %>%  distinct()

      #Intermittent techs
      #Add region
      L2233.IndiaGlobalIntTechCoef_elec_cool2 <- L2233.IndiaGlobalIntTechCoef_elec_cool

      L2233.IndiaGlobalIntTechCoef_elec_cool2$region <- "India"
      #Add Basins
      L2233.IndiaGlobalIntTechCoef_elec_cool2 %>%
        inner_join(L2233.IndiaBasins, by = "region") %>%
        rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) ->
        L2233.IndiaGlobalIntTechCoef_elec_cool2
      #Add the new structure to this
      L2233.IndiaGlobalIntTechCoef_elec_cool2 %>%
        inner_join(L2233.IndiaStubTecheleccool, by = "subsector") -> L2233.IndiaGlobalIntTechCoef_elec_cool2
      L2233.IndiaGlobalIntTechCoef_elec_cool2 %>%
        rename(region = region.x, supplysector = supplysector.x, subsector0 = subsector0.x,
               stub.technology = stub.technology.x) %>%
        select(region, supplysector, subsector0, subsector1, subsector, stub.technology, year, minicam.energy.input, coefficient)->
        L2233.IndiaGlobalIntTechCoef_elec_cool2
      #Remove repetitions
      L2233.IndiaGlobalIntTechCoef_elec_cool2 <- L2233.IndiaGlobalIntTechCoef_elec_cool2 %>%  distinct()


      #KEYWORD INFO

      L2233.PrimaryRenewKeyword_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.PrimaryRenewKeyword_elec_cool$sector.name)

      L2233.IndiaPrimaryRenewKeyword_elec_cool <- L2233.PrimaryRenewKeyword_elec_cool

      L2233.PrimaryRenewKeywordInt_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.PrimaryRenewKeywordInt_elec_cool$sector.name)

      L2233.IndiaPrimaryRenewKeywordInt_elec_cool <- L2233.PrimaryRenewKeywordInt_elec_cool

      #CAPITAL RELATED INFORMATION FOR INDIA

      L2233.StubTechTrackCapital_elec %>%
        filter(region == "India") -> L2233.IndiaStubTechTrackCapital_elec

      L2233.IndiaStubTechTrackCapital_elec %>%
        inner_join(L2233.IndiaStubTecheleccool, by = "supplysector") %>%
        select(-region.x, -subsector.x, -stub.technology.x) %>%
        rename(region = region.y, subsector = subsector.y, stub.technology = stub.technology.y) ->
        L2233.IndiaBasinsStubTechTrackCapital_elec

      L2233.IndiaBasinsStubTechTrackCapital_elec <- L2233.IndiaBasinsStubTechTrackCapital_elec[c
        ("supplysector", "subsector0", "subsector1", "subsector",
        "stub.technology", "year", "minicam.non.energy.input", "input.cost", "capital.coef", "tracking.market",
          "depreciation.rate")]  #OUTPUT


      #HYDRO FIX OUTPUT (Currently it is distributed according to the methodology
      #followed in the creation of the main output (L2233.out_EJ_elec_B_F_tech_Yh_cool_India)).
      #Can/will be changed later according to the hydro power station locations.


      L103.water_mapping_R_B_W_wdraw_share_renamed$GCAM_region_ID <- sub("^(17).*", "India", L103.water_mapping_R_B_W_wdraw_share_renamed$GCAM_region_ID)

      L103.water_mapping_R_B_W_wdraw_share_renamed %>%
        inner_join(basin_to_country_mapping, by = "GCAM_basin_ID") %>%
        rename(region = GCAM_region_ID) -> L2233.IndiaStubTechFixOut_hydro


      L2233.IndiaStubTechFixOut_hydro %>%
        inner_join(L2233.StubTechFixOut_hydro,
                   by =  c("region")) %>%
        mutate(fixedOutput = fixedOutput * share) %>%
        mutate(subsector1 = "hydro")  %>%
        select(region, supplysector, GLU_name, subsector1, subsector, stub.technology, year, fixedOutput,
               share.weight.year, subs.share.weight, tech.share.weight) %>%
        rename(subsector0 = GLU_name) %>%
        filter(year > 2015) ->
        L2233.IndiaBasinsStubTechFixOut_hydro  #OUTPUT

#=======================================================================================================================================#

    L2233.DeleteSupplysector_Indiaelec %>%
      add_title("Deletes India level electricity sector") %>%
      add_units("NA") %>%
      add_comments("NA") %>%
      add_legacy_name("L2233.DeleteSupplysector_Indiaelec") %>%
      add_precursors("water/elec_tech_water_map") ->  L2233.DeleteSupplysector_Indiaelec


    L2233.DeletePassthroughsector_Indiaelec %>%
      add_title("Deletes India level passthrough sectors for the electricity_water intersection") %>%
      add_units("NA") %>%
      add_comments("NA") %>%
      add_legacy_name("L2233.DeletePassthroughsector_Indiaelec") %>%
      add_precursors("water/elec_tech_water_map") ->  L2233.DeletePassthroughsector_Indiaelec



    L2233.out_EJ_elec_B_F_tech_Yh_cool_India %>%
      add_title("Downscaling electricity generation at the basin level for India") %>%
      add_units("EJ") %>%
      add_comments("Downscaling electricity generation at the basin level for India") %>%
      add_legacy_name("L2233.out_EJ_elec_B_F_tech_Yh_cool_India") %>%
      add_precursors("L103.water_mapping_R_B_W_Ws_share",
                     "water/basin_to_country_mapping",
                     "water/elec_tech_water_map",
                     "L1233.out_EJ_R_elec_F_tech_Yh_cool") ->  L2233.out_EJ_elec_B_F_tech_Yh_cool_India


    L2233.IndiaStubTecheleccool %>%
      add_title("Map for the new nesting structure") %>%
      add_units("NA") %>%
      add_comments("Contributes to the mapping of the new nesting structure") %>%
      add_legacy_name("L2233.IndiaStubTecheleccool") %>%
      add_precursors("water/Exogenous/IndiaStubTecheleccool",
                     "water/elec_tech_water_map") ->  L2233.IndiaStubTecheleccool

    L2233.IndiaSupplysectorelecLogit %>%
      add_title("Logit for the supplysector") %>%
      add_units("NA") %>%
      add_comments("Logit for the electricity supplysector") %>%
      add_legacy_name("L2233.IndiaSupplysectorelecLogit") %>%
      add_precursors("water/Exogenous/IndiaSupplysectorelec",
                     "water/elec_tech_water_map") -> L2233.IndiaSupplysectorelecLogit


    L2233.IndiaBasinsElecMarket %>%
      add_title("Reserve margin and avg grid factor for CSP tech") %>%
      add_units("NA") %>%
      add_comments("Defines the marginal revenue sector and market at the basin level") %>%
      add_legacy_name("L2233.IndiaBasinsElecMarket") %>%
      add_precursors("water/Exogenous/IndiaPassThroughSectoreleccool",
                     "water/elec_tech_water_map") -> L2233.IndiaBasinsElecMarket


    L2233.IndiaBasinelecLogit %>%
      add_title("Future shareweights at the cooling technology level") %>%
      add_units("NA") %>%
      add_comments("Defines share weights at the cooling technology level for future years") %>%
      add_legacy_name("L2233.IndiaBasinelecLogit") %>%
      add_precursors("water/Exogenous/IndiaSupplysectorelec",
                     "water/elec_tech_water_map") -> L2233.IndiaBasinelecLogit


    L2233.IndiaSubsectorelec_logit %>%
      add_title("Subsector (fuel) level logit") %>%
      add_units("NA") %>%
      add_comments("Defines subsector (fuel) level logit") %>%
      add_legacy_name("L2233.IndiaSubsectorelec_logit") %>%
      add_precursors("water/Exogenous/IndiaStubTecheleccool",
                     "water/elec_tech_water_map",
                     "energy/A23.subsector_logit") -> L2233.IndiaSubsectorelec_logit

    L2233.IndiaSubsectorelec_shrwt %>%
      add_title("Subsector (fuel) level shrwt") %>%
      add_units("NA") %>%
      add_comments("Defines subsector (fuel) level shrwt") %>%
      add_legacy_name("L2233.IndiaSubsectorelec_shrwt") %>%
      add_precursors("water/Exogenous/IndiaStubTecheleccool",
                     "water/elec_tech_water_map",
                     "energy/A23.subsector_shrwt") -> L2233.IndiaSubsectorelec_shrwt

    L2233.IndiaSubsectorelec_interp %>%
      add_title("Subsector (fuel) level interp") %>%
      add_units("NA") %>%
      add_comments("Defines subsector (fuel) level interpolation rule") %>%
      add_legacy_name("L2233.IndiaSubsectorelec_interp") %>%
      add_precursors("water/Exogenous/IndiaStubTecheleccool",
                     "water/elec_tech_water_map",
                     "energy/A23.subsector_interp") -> L2233.IndiaSubsectorelec_interp


      L2233.Indiaplantelec_logit %>%
        add_title("Power plant level logit applied") %>%
        add_units("NA") %>%
        add_comments("Logit at the power plant level") %>%
        add_legacy_name("L2233.Indiaplantelec_logit") %>%
        add_precursors("water/Exogenous/IndiaStubTecheleccool",
                       "water/elec_tech_water_map",
                       "energy/A23.subsector_logit") -> L2233.Indiaplantelec_logit


      L2233.Indiacoolingelec_logit %>%
        add_title("Cooling tech level logit applied") %>%
        add_units("NA") %>%
        add_comments("Logit at the cooling tech level") %>%
        add_legacy_name("L2233.Indiacoolingelec_logit") %>%
        add_precursors("water/Exogenous/IndiaStubTecheleccool",
                       "water/elec_tech_water_map",
                       "energy/A23.subsector_logit") -> L2233.Indiacoolingelec_logit


      L2233.IndiaTechShrwtFlltelec %>%
        add_title("Shareweight fillout for power plant technologies") %>%
        add_units("NA") %>%
        add_comments("Sharweight at the power plant level") %>%
        add_legacy_name("L2233.IndiaTechShrwtFlltelec") %>%
        add_precursors("water/Exogenous/IndiaTechShrwtFlltelec",
                       "water/elec_tech_water_map") ->  L2233.IndiaTechShrwtFlltelec


      L2233.IndiaTechInterpelec %>%
        add_title("Shareweight interpolation for power plant technologies") %>%
        add_units("NA") %>%
        add_comments("Defines share weight interpolation rules for power plant technologies") %>%
        add_legacy_name("L2233.IndiaTechInterpelec") %>%
        add_precursors("water/Exogenous/IndiaTechInterpelec",
                       "water/elec_tech_water_map") ->  L2233.IndiaTechInterpelec



      L2233.IndiaTechInterpToelec %>%
        add_title("Long-term interpolation rules for power plant technologies") %>%
        add_units("NA") %>%
        add_comments("Defines long-term interpolation rules for power plant technologies") %>%
        add_legacy_name("L2233.IndiaTechInterpToelec") %>%
        add_precursors("water/Exogenous/IndiaTechInterpToelec",
                       "water/elec_tech_water_map") ->  L2233.IndiaTechInterpToelec


      L2233.IndiaTechShrwtnucelec %>%
        add_title("Shareweights for subsector0 nuclear technologies") %>%
        add_units("NA") %>%
        add_comments("Defines shareweights for nuclear technologies until 2050") %>%
        add_legacy_name("L2233.IndiaTechShrwtnucelec") %>%
        add_precursors("water/Exogenous/IndiaTechShrwtnucelec",
                       "water/elec_tech_water_map") ->  L2233.IndiaTechShrwtnucelec


      L2233.IndiaTechShrwtrenewelec %>%
        add_title("Shareweights for subsector0 renewable technologies") %>%
        add_units("NA") %>%
        add_comments("Defines shareweights for renewable technologies until 2050 & 2100") %>%
        add_legacy_name("L2233.IndiaTechShrwtrenewelec") %>%
        add_precursors("water/Exogenous/IndiaTechShrwtrenewelec",
                       "water/elec_tech_water_map") ->  L2233.IndiaTechShrwtrenewelec


      L223.IndiaTechEff_elec %>%
        add_title("Efficiencies for standard technologies at the subsector0 (power plant) level") %>%
        add_units("NA") %>%
        add_comments("Efficiencies at the power plant level for standard techs") %>%
        add_legacy_name("L223.IndiaTechEff_elec") %>%
        add_precursors("L223.GlobalTechEff_elec",
                         "water/Exogenous/IndiaTechShrwtrenewelec",
                         "water/elec_tech_water_map") -> L223.IndiaTechEff_elec


      L223.IndiaIntTechEff_elec %>%
        add_title("Efficiencies for intermittent technologies at the subsector0 (power plant) level") %>%
        add_units("NA") %>%
        add_comments("Efficiencies at the power plant level for intermittent techs") %>%
        add_legacy_name("L223.IndiaIntTechEff_elec") %>%
        add_precursors("L223.GlobalIntTechEff_elec",
                         "water/Exogenous/IndiaTechShrwtrenewelec",
                         "water/elec_tech_water_map") -> L223.IndiaIntTechEff_elec


      L2233.IndiaCSPReserve %>%
        add_title("Reserve margin and avg grid factor for CSP tech") %>%
        add_units("NA") %>%
        add_comments("Includes reserve margin and avg grid factor for CSP technologies") %>%
        add_legacy_name("L2233.IndiaCSPReserve") %>%
        add_precursors("water/Exogenous/IndiaElecReserveeleccool",
                       "water/elec_tech_water_map") -> L2233.IndiaCSPReserve


      L2233.IndiaElecTechShrwtBasinscool %>%
        add_title("Shareweights at the cooling technology level") %>%
        add_units("NA") %>%
        add_comments("Defining shareweights at the cooling technology level (stub.technology)") %>%
        add_legacy_name("L2233.IndiaElecTechShrwtBasinscool") %>%
        add_precursors("water/Exogenous/IndiaElecPassthruShrwtBasinseleccool",
                       "water/elec_tech_water_map") -> L2233.IndiaElecTechShrwtBasinscool


      L2233.IndiaStubTechShrwteleccool %>%
        add_title("Future shareweights at the cooling technology level") %>%
        add_units("NA") %>%
        add_comments("Defines share weights at the cooling technology level for future years") %>%
        add_legacy_name("L2233.IndiaStubTechShrwteleccool") %>%
        add_precursors("water/Exogenous/IndiaStubTechShrwteleccool",
                       "water/elec_tech_water_map") ->  L2233.IndiaStubTechShrwteleccool


      L2233.IndiaStubTechCapfactorelecCSP %>%
        add_title("Capacity Factor for CSP technologies") %>%
        add_units("NA") %>%
        add_comments("Defines the capacity factors for CSP cooling technologies") %>%
        add_legacy_name("L2233.IndiaStubTechCapfactorelecCSP") %>%
        add_precursors("water/Exogenous/IndiaStubTechCapFactoreleccool",
                       "water/elec_tech_water_map") -> L2233.IndiaStubTechCapfactorelecCSP


      L2233.IndiaStubTechEffeleccool %>%
        add_title("Efficiency values for CSP technologies") %>%
        add_units("NA") %>%
        add_comments("Defines efficiency at the cooling technology level for historical years") %>%
        add_legacy_name("L2233.IndiaStubTechEffeleccool") %>%
        add_precursors("water/Exogenous/IndiaStubTechEffeleccool",
                       "water/elec_tech_water_map") -> L2233.IndiaStubTechEffeleccool


      L2233.IndiaGlobalIntTechCapital_elec_cool %>%
        add_title("Overnight capital, charge rates and capacity factors for standard cooling technologies") %>%
        add_units("1975USD") %>%
        add_comments("Capital costs converted from 2005USD") %>%
        add_comments("Fixed input assumptions for capacity factor and FCR") %>%
        add_legacy_name("L2233.IndiaGlobalIntTechCapital_elec_cool") %>%
        add_precursors("water/elec_tech_water_map",
                       "water/A23.CoolingSystemCosts") -> L2233.IndiaGlobalIntTechCapital_elec_cool


      L2233.IndiaGlobalTechCapital_elec_cool %>%
        add_title("Overnight capital, charge rates and capacity factors for standard cooling technologies") %>%
        add_units("1975USD") %>%
        add_comments("Capital costs converted from 2005USD") %>%
        add_comments("Fixed input assumptions for capacity factor and FCR") %>%
        add_legacy_name("L2233.IndiaGlobalTechCapital_elec_cool") %>%
        add_precursors("water/elec_tech_water_map",
                       "water/A23.CoolingSystemCosts") -> L2233.IndiaGlobalTechCapital_elec_cool



      L2233.IndiaAvgFossilEffKeyword_elec_cool %>%
      add_title("Average fossil efficiency for cooling techs") %>%
        add_units("NA") %>%
        add_comments("NA") %>%
        add_legacy_name("L2233.IndiaAvgFossilEffKeyword_elec_cool") %>%
        add_precursors("water/Exogenous/elecwatergcamfiles/L2233.AvgFossilEffKeyword_elec_cool",
                       "water/Exogenous/IndiaStubTecheleccool",
                       "water/elec_tech_water_map") -> L2233.IndiaAvgFossilEffKeyword_elec_cool


      L223.IndiaGlobalTechCapital_elec %>%
        add_title("Capital costs of conventional electricity generation technologies") %>%
        add_units("1975 USD/kW/yr") ->
        L223.IndiaGlobalTechCapital_elec


      L223.IndiaGlobalIntTechCapital_elec %>%
        add_title("Capital costs of intermittent electricity generation technologies") %>%
        add_units("1975 USD/kW/yr") ->
        L223.IndiaGlobalIntTechCapital_elec


      L223.IndiaGlobalTechOMfixed_elec %>%
        add_title("O&M fixed costs of conventional electricity generation technologies") %>%
        add_units("1975 USD/kW/yr") ->
        L223.IndiaGlobalTechOMfixed_elec


      L223.IndiaGlobalIntTechOMfixed_elec %>%
        add_title("O&M fixed costs of intermittent electricity generation technologies") %>%
        add_units("1975 USD/kW/yr") ->
        L223.IndiaGlobalIntTechOMfixed_elec


      L223.IndiaGlobalTechOMvar_elec %>%
        add_title("O&M variable costs of conventional electricity generation technologies") %>%
        add_units("1975 USD/kW/yr") ->
        L223.IndiaGlobalTechOMvar_elec


      L223.IndiaGlobalIntTechOMvar_elec %>%
        add_title("O&M variable costs of intermittent electricity generation technologies") %>%
        add_units("1975 USD/kW/yr") ->
        L223.IndiaGlobalIntTechOMvar_elec


      L2233.IndiaGlobalIntTechBackup_elec_cool %>%
        add_title("Capital costs of backup technologies for intermittent techs") %>%
        add_units("1975 USD/kW/yr") ->
        L2233.IndiaGlobalIntTechBackup_elec_cool


      L2233.IndiaGlobalIntTechCapFac_elec_cool %>%
        add_title("Interpolated intermittent technologies") %>%
        add_units("NA") %>%
        add_precursors("water/Exogenous/elecwatergcamfiles/L2233.GlobalIntTechCapFac_elec_cool") ->
        L2233.IndiaGlobalIntTechCapFac_elec_cool


      L2233.IndiaGlobalIntTechLifetime_elec_cool %>%
        add_title("Lifetimes of intermittent electricity generating technologies") %>%
        add_units("Years") ->
        L2233.IndiaGlobalIntTechLifetime_elec_cool


       L2233.IndiaGlobalIntTechShrwt_elec_cool %>%
        add_title("Shareweights of intermittent electricity generating technologies") %>%
        add_units("Unitless") ->
        L2233.IndiaGlobalIntTechShrwt_elec_cool


       L2233.IndiaGlobalTechCapFac_elec_cool %>%
         add_title("Interpolated non-intermittent technologies") %>%
         add_units("NA") %>%
         add_precursors("water/Exogenous/elecwatergcamfiles/L2233.GlobalTechCapFac_elec_cool") ->
         L2233.IndiaGlobalTechCapFac_elec_cool


       L2233.IndiaGlobalTechCapture_elec_cool %>%
         add_title("Storage markets and remove fractions for CCS tech by cooling type") %>%
         add_units("Unitless") ->
         L2233.IndiaGlobalTechCapture_elec_cool


       L2233.IndiaGlobalTechLifetime_elec_cool %>%
         add_title("Lifetimes for standard electricity generating technologies") %>%
         add_units("Years") ->
         L2233.IndiaGlobalTechLifetime_elec_cool


       L2233.IndiaGlobalTechProfitShutdown_elec_cool %>%
         add_title("Shutdown points and profit shutdown steepness for standard electricity generating technologies") %>%
         add_units("Unitless") ->
         L2233.IndiaGlobalTechProfitShutdown_elec_cool


       L2233.IndiaGlobalTechSCurve_elec_cool %>%
         add_title("Global tech lifetime for techs with s-curve retirement function") %>%
         add_units("Lifetime in years, half-life in years") ->
         L2233.IndiaGlobalTechSCurve_elec_cool


       L2233.IndiaGlobalTechShrwt_elec_cool %>%
         add_title("Global shareweights for non-intermittent technologies for the electricity sector") %>%
         add_units("Unitless") ->
         L2233.IndiaGlobalTechShrwt_elec_cool


       L2233.IndiaGlobalTechCoef_elec_cool %>%
         add_title("Coefficients for water elec sector water withdrawal and consumption (standard techs)") %>%
         add_units("m^3/GJ") %>%
         add_comments("Water footprints mapped onto supplysector-subsector-technology tables") %>%
         add_comments("m^3/MWh converted to m^3/GJ") %>%
         add_legacy_name("L2233.IndiaGlobalTechCoef_elec_cool") %>%
         add_precursors("L2233.GlobalTechCoef_elec_cool") ->
         L2233.IndiaGlobalTechCoef_elec_cool


       L2233.IndiaGlobalIntTechCoef_elec_cool %>%
         add_title("Coefficients for water elec sector water withdrawal and consumption (intermittent techs)") %>%
         add_units("m^3/GJ") %>%
         add_comments("Water footprints mapped onto supplysector-subsector-technology tables") %>%
         add_comments("m^3/MWh converted to m^3/GJ") %>%
         add_legacy_name("L2233.IndiaGlobalIntTechCoef_elec_cool") %>%
         add_precursors("L2233.GlobalIntTechCoef_elec_cool") ->
         L2233.IndiaGlobalIntTechCoef_elec_cool


       L2233.IndiaPrimaryRenewKeyword_elec_cool %>%
         add_title("keywords for non-intermittent renewable technologies for the electricity sector") %>%
         add_units("NA") ->
         L2233.IndiaPrimaryRenewKeyword_elec_cool


       L2233.IndiaPrimaryRenewKeywordInt_elec_cool %>%
         add_title("keywords for intermittent renewable technologies") %>%
         add_units("NA") ->
         L2233.IndiaPrimaryRenewKeywordInt_elec_cool


       L2233.IndiaBasinsStubTechTrackCapital_elec %>%
      add_title("Stub tech to treat capital tracking for rooftop_pv seperately") %>%
        add_units("1975$/GJ") %>%
        add_comments("Since rooftop_pv does not have vintaging we need to track its capital") %>%
        add_comments("with explicit assumptions about depreciation.") %>%
        add_legacy_name("L2233.IndiaBasinsStubTechTrackCapital_elec") %>%
        add_precursors("water/Exogenous/elecwatergcamfiles/L2233.StubTechTrackCapital_elec",
                       "water/Exogenous/IndiaStubTecheleccool",
                       "water/elec_tech_water_map") -> L2233.IndiaBasinsStubTechTrackCapital_elec


      L2233.IndiaBasinsStubTechFixOut_hydro %>%
      add_title("Fixed output for hydropower") %>%
        add_units("EJ") %>%
        add_comments("Created by binding base future year fixed hydro outputs") %>%
        add_legacy_name("L2233.IndiaBasinsStubTechFixOut_hydro") %>%
        add_precursors("L103.water_mapping_R_B_W_Ws_share",
                       "water/basin_to_country_mapping",
                       "water/Exogenous/elecwatergcamfiles/L2233.StubTechFixOut_hydro") -> L2233.IndiaBasinsStubTechFixOut_hydro


    return_data(L2233.DeleteSupplysector_Indiaelec,
                L2233.DeletePassthroughsector_Indiaelec,
                L2233.out_EJ_elec_B_F_tech_Yh_cool_India,
                L2233.IndiaStubTecheleccool,
                L223.IndiaSectorUseTrialMarket_elec,
                L2233.IndiaSupplysectorelecLogit,
                L2233.IndiaBasinsElecMarket,
                L2233.IndiaBasinelecLogit,
                L2233.IndiaSubsectorelec_logit,
                L2233.IndiaSubsectorelec_shrwt,
                L2233.IndiaSubsectorelec_interp,
                L223.IndiaGlobalTechCapital_elec,
                L223.IndiaGlobalIntTechCapital_elec,
                L223.IndiaGlobalTechOMfixed_elec,
                L223.IndiaGlobalIntTechOMfixed_elec,
                L223.IndiaGlobalTechOMvar_elec,
                L223.IndiaGlobalIntTechOMvar_elec,
                L223.IndiaSubsector0Shrwt,
                L2233.Indiaplantelec_logit,
                L2233.Indiacoolingelec_logit,
                L2233.IndiaTechShrwtFlltelec,
                L2233.IndiaTechInterpelec,
                L2233.IndiaTechInterpToelec,
                L2233.IndiaTechShrwtnucelec,
                L2233.IndiaTechShrwtrenewelec,
                L223.IndiaTechEff_elec,
                L223.IndiaIntTechEff_elec,
                L2233.IndiaCSPReserve,
                L2233.IndiaElecTechShrwtBasinscool,
                L2233.IndiaStubTechShrwteleccool,
                L2233.IndiaStubTechCapfactorelecCSP,
                L2233.IndiaStubTechEffeleccool,
                L2233.IndiaGlobalIntTechCapital_elec_cool,
                L2233.IndiaGlobalTechCapital_elec_cool,
                L2233.IndiaAvgFossilEffKeyword_elec_cool,
                L2233.IndiaGlobalIntTechBackup_elec_cool,
                L2233.IndiaGlobalIntTechCapFac_elec_cool,
                L2233.IndiaGlobalIntTechLifetime_elec_cool,
                L2233.IndiaGlobalIntTechShrwt_elec_cool,
                L2233.IndiaGlobalTechCapFac_elec_cool,
                L2233.IndiaGlobalTechCapture_elec_cool,
                L2233.IndiaGlobalTechLifetime_elec_cool,
                L2233.IndiaGlobalTechProfitShutdown_elec_cool,
                L2233.IndiaGlobalTechSCurve_elec_cool,
                L2233.IndiaGlobalTechShrwt_elec_cool,
                L2233.IndiaGlobalTechCoef_elec_cool,
                L2233.IndiaGlobalIntTechCoef_elec_cool,
                L2233.IndiaPrimaryRenewKeyword_elec_cool,
                L2233.IndiaPrimaryRenewKeywordInt_elec_cool,
                L2233.IndiaBasinsStubTechTrackCapital_elec,
                L2233.IndiaBasinsStubTechFixOut_hydro)

  } else {
    stop("Unknown command")
  }
}

