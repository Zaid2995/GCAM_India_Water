#' module_water_electricity_water_india_xml
#'
#' Construct XML data structure for \code{electricity_water_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_water_india.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_water_india.xml.R} (water XML).
module_water_electricity_water_india_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2233.DeleteSupplysector_Indiaelec",
             "L2233.IndiaStubTecheleccool",
             "L2233.IndiaSubsectorelec_shrwt",
             "L2233.IndiaSubsectorelec_interp",
             "L2233.IndiaTechShrwtFlltelec",
             "L2233.IndiaTechInterpToelec",
             "L2233.IndiaTechShrwtnucelec",
             "L2233.IndiaTechShrwtrenewelec",
             "L2233.IndiaStubTechShrwteleccool",
             "L2233.IndiaStubTechCapfactorelecCSP",
             "L2233.IndiaStubTechEffeleccool",
             "L2233.IndiaGlobalIntTechCapital_elec_cool",
             "L2233.IndiaGlobalTechCapital_elec_cool",
             "L223.IndiaGlobalTechCapital_elec",
             "L223.IndiaGlobalIntTechCapital_elec",
             "L223.IndiaGlobalTechOMfixed_elec",
             "L223.IndiaGlobalIntTechOMfixed_elec",
             "L223.IndiaGlobalTechOMvar_elec",
             "L223.IndiaGlobalIntTechOMvar_elec",
             "L2233.IndiaAvgFossilEffKeyword_elec_cool",
             "L2233.IndiaGlobalIntTechBackup_elec_cool",
             "L2233.GlobalIntTechEff_elec_cool",
             "L2233.IndiaGlobalIntTechCapFac_elec_cool",
             "L2233.IndiaGlobalIntTechLifetime_elec_cool",
             "L2233.IndiaGlobalIntTechShrwt_elec_cool",
             "L2233.GlobalTechEff_elec_cool",
             "L2233.IndiaGlobalTechCapFac_elec_cool",
             "L2233.IndiaGlobalTechCapture_elec_cool",
             "L2233.IndiaGlobalTechLifetime_elec_cool",
             "L2233.IndiaGlobalTechProfitShutdown_elec_cool",
             "L2233.IndiaGlobalTechSCurve_elec_cool",
             "L2233.IndiaGlobalTechShrwt_elec_cool",
             "L2233.IndiaPrimaryRenewKeyword_elec_cool",
             "L2233.IndiaPrimaryRenewKeywordInt_elec_cool",
             "L2233.IndiaBasinsStubTechTrackCapital_elec",
             "L2233.IndiaBasinsStubTechFixOut_hydro"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_water_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2233.DeleteSupplysector_Indiaelec <- get_data(all_data, "L2233.DeleteSupplysector_Indiaelec")
    L2233.IndiaStubTecheleccool <- get_data(all_data, "L2233.IndiaStubTecheleccool")
    L2233.IndiaSubsectorelec_shrwt <- get_data(all_data, "L2233.IndiaSubsectorelec_shrwt")
    L2233.IndiaSubsectorelec_interp <- get_data(all_data, "L2233.IndiaSubsectorelec_interp")
    L2233.IndiaTechShrwtFlltelec <- get_data(all_data, "L2233.IndiaTechShrwtFlltelec")
    L2233.IndiaTechInterpToelec <- get_data(all_data, "L2233.IndiaTechInterpToelec")
    L2233.IndiaTechShrwtnucelec <- get_data(all_data, "L2233.IndiaTechShrwtnucelec")
    L2233.IndiaTechShrwtrenewelec <- get_data(all_data, "L2233.IndiaTechShrwtrenewelec")
    L2233.IndiaStubTechShrwteleccool <- get_data(all_data, "L2233.IndiaStubTechShrwteleccool")
    L2233.IndiaStubTechCapfactorelecCSP <- get_data(all_data, "L2233.IndiaStubTechCapfactorelecCSP")
    L2233.IndiaStubTechEffeleccool <- get_data(all_data, "L2233.IndiaStubTechEffeleccool")
    L2233.IndiaGlobalIntTechCapital_elec_cool <- get_data(all_data, "L2233.IndiaGlobalIntTechCapital_elec_cool")
    L2233.IndiaGlobalTechCapital_elec_cool <- get_data(all_data, "L2233.IndiaGlobalTechCapital_elec_cool")
    L223.IndiaGlobalTechCapital_elec <- get_data(all_data, "L223.IndiaGlobalTechCapital_elec")
    L223.IndiaGlobalIntTechCapital_elec <- get_data(all_data, "L223.IndiaGlobalIntTechCapital_elec")
    L223.IndiaGlobalTechOMfixed_elec <- get_data(all_data, "L223.IndiaGlobalTechOMfixed_elec")
    L223.IndiaGlobalIntTechOMfixed_elec <- get_data(all_data, "L223.IndiaGlobalIntTechOMfixed_elec")
    L223.IndiaGlobalTechOMvar_elec <- get_data(all_data, "L223.IndiaGlobalTechOMvar_elec")
    L223.IndiaGlobalIntTechOMvar_elec <- get_data(all_data, "L223.IndiaGlobalIntTechOMvar_elec")
    L2233.IndiaAvgFossilEffKeyword_elec_cool <- get_data(all_data, "L2233.IndiaAvgFossilEffKeyword_elec_cool")
    L2233.IndiaGlobalIntTechBackup_elec_cool <- get_data(all_data, "L2233.IndiaGlobalIntTechBackup_elec_cool")
    L2233.GlobalIntTechEff_elec_cool <- get_data(all_data, "L2233.GlobalIntTechEff_elec_cool")
    L2233.IndiaGlobalIntTechCapFac_elec_cool <- get_data(all_data, "L2233.IndiaGlobalIntTechCapFac_elec_cool")
    L2233.IndiaGlobalIntTechLifetime_elec_cool <- get_data(all_data, "L2233.IndiaGlobalIntTechLifetime_elec_cool")
    L2233.IndiaGlobalIntTechShrwt_elec_cool <- get_data(all_data, "L2233.IndiaGlobalIntTechShrwt_elec_cool")
    L2233.GlobalTechEff_elec_cool <- get_data(all_data, "L2233.GlobalTechEff_elec_cool")
    L2233.IndiaGlobalTechCapFac_elec_cool <- get_data(all_data, "L2233.IndiaGlobalTechCapFac_elec_cool")
    L2233.IndiaGlobalTechCapture_elec_cool <- get_data(all_data, "L2233.IndiaGlobalTechCapture_elec_cool")
    L2233.IndiaGlobalTechLifetime_elec_cool <- get_data(all_data, "L2233.IndiaGlobalTechLifetime_elec_cool")
    L2233.IndiaGlobalTechProfitShutdown_elec_cool <- get_data(all_data, "L2233.IndiaGlobalTechProfitShutdown_elec_cool")
    L2233.IndiaGlobalTechSCurve_elec_cool <- get_data(all_data, "L2233.IndiaGlobalTechSCurve_elec_cool")
    L2233.IndiaGlobalTechShrwt_elec_cool <- get_data(all_data, "L2233.IndiaGlobalTechShrwt_elec_cool")
    L2233.IndiaPrimaryRenewKeyword_elec_cool <- get_data(all_data, "L2233.IndiaPrimaryRenewKeyword_elec_cool")
    L2233.IndiaPrimaryRenewKeywordInt_elec_cool <- get_data(all_data, "L2233.IndiaPrimaryRenewKeywordInt_elec_cool")
    L2233.IndiaBasinsStubTechTrackCapital_elec <- get_data(all_data, "L2233.IndiaBasinsStubTechTrackCapital_elec")
    L2233.IndiaBasinsStubTechFixOut_hydro <- get_data(all_data, "L2233.IndiaBasinsStubTechFixOut_hydro")


    # Silence package checks
    technology <- NULL

    #ALTERING HEADERS FOR A FEW FILES

    #L2233.IndiaSubsectorelec_interp %>%
    #rename(nesting-subsector = subsector, nesting-subsector0 = subsector0) ->
    #L2233.IndiaSubsectorelec_interp


    #L2233.IndiaSubsectorelec_shrwt %>%
    #rename(nesting-subsector = subsector, nesting-subsector0 = subsector0) ->
    #L2233.IndiaSubsectorelec_shrwt


    #L2233.IndiaBasinelecLogit %>%
     # rename("nesting-subsector" = nesting.subsector) ->
    #  L2233.IndiaBasinelecLogit


    L2233.IndiaBasinsStubTechTrackCapital_elec$region <- 'India'

    L2233.IndiaBasinsStubTechTrackCapital_elec <- L2233.IndiaBasinsStubTechTrackCapital_elec[c(
      "region", "supplysector", "subsector0", "subsector1", "subsector", "stub.technology",
      "year", "minicam.non.energy.input", "input.cost", "capital.coef", "tracking.market", "depreciation.rate")]

   #Making alterations to global Efficiency files

    L2233.GlobalIntTechEff_elec_cool <- rename(L2233.GlobalIntTechEff_elec_cool, `intermittent.technology` = technology)
    L2233.GlobalIntTechEff_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.GlobalIntTechEff_elec_cool$sector.name)

    L2233.GlobalTechEff_elec_cool$sector.name <- sub("^(elec_).*", "electricity", L2233.GlobalTechEff_elec_cool$sector.name)

    #"L2233.out_EJ_elec_B_F_tech_Yh_cool_India",
    #L2233.out_EJ_elec_B_F_tech_Yh_cool_India <- get_data(all_data, "L2233.out_EJ_elec_B_F_tech_Yh_cool_India")
    #add_xml_data_generate_levels(L2233.out_EJ_elec_B_F_tech_Yh_cool_India,
    #"StubTechProd","subsector","nesting-subsector",2,FALSE) %>%
    #"L2233.out_EJ_elec_B_F_tech_Yh_cool_India",
    #gcamdata::run_xml_conversion()

    #"L223.IndiaSubsector0Shrwt",
    # add_xml_data_generate_levels(L223.IndiaSubsector0Shrwt,
    #"SubsectorShrwt","subsector","nesting-subsector",2,FALSE) %>%
    #"L223.IndiaSubsector0Shrwt",
    #L223.IndiaSubsector0Shrwt <- get_data(all_data, "L223.IndiaSubsector0Shrwt")

    #"L2233.IndiaTechInterpelec",
    #add_xml_data_generate_levels(L2233.IndiaTechInterpelec,
    #"SubsectorInterp","subsector","nesting-subsector",2,FALSE) %>%
    #"L2233.IndiaTechInterpelec",
    #L2233.IndiaTechInterpelec <- get_data(all_data, "L2233.IndiaTechInterpelec")

    #CONVENTIONAL TECHNOLOGIES - CAPITAL COSTS
    #Creating one capital cost file to ensure that costs are read by the model for subsector (power plants) and technologies (cooling technologies)
    #First, need to change the header name for 2 columns so that the files can be joined seamlessly
    L223.IndiaGlobalTechCapital_elec %>%
    rename(subsector = technology) -> L223.IndiaGlobalTechCapital_elec
    L2233.IndiaGlobalTechCapital_elec_cool %>%
    rename(subsector = subsector.name) -> L2233.IndiaGlobalTechCapital_elec_cool
    #Now join the 2 files by "subsector" and "year"
    L223.IndiaGlobalTechCapital_elec %>%
    inner_join(L2233.IndiaGlobalTechCapital_elec_cool, by = c("subsector", "year")) -> L2233.IndiaGlobalTechCapital_elec_cool
    L2233.IndiaGlobalTechCapital_elec_cool %>%
    select(-sector.name.x, -input.capital.x, -fixed.charge.rate.y) %>%
    rename(sector.name = sector.name.y, input.capital = input.capital.y, fixed.charge.rate = fixed.charge.rate.x) %>%
    mutate(capital.overnight = capital.overnight.x +capital.overnight.y) ->
      L2233.IndiaGlobalTechCapital_elec_cool
    L2233.IndiaGlobalTechCapital_elec_cool %>%
      select(-capital.overnight.x, -capital.overnight.y) -> L2233.IndiaGlobalTechCapital_elec_cool
    L2233.IndiaGlobalTechCapital_elec_cool %>%
      select(-subsector.name) %>%
      rename(subsector.name = subsector) -> L2233.IndiaGlobalTechCapital_elec_cool
    L2233.IndiaGlobalTechCapital_elec_cool <- L2233.IndiaGlobalTechCapital_elec_cool [c("sector.name", "subsector.name",
    "technology", "year", "input.capital", "capital.overnight", "fixed.charge.rate")]


    #CONVENTIONAL TECHNOLOGIES - OM_Var COSTS
    #Adding all the OM_var costs to the capital costs file created above.
    #First, we need to create common headers to merge the files
     L223.IndiaGlobalTechOMvar_elec %>%
      rename(subsector = technology) -> L223.IndiaGlobalTechOMvar_elec
    L2233.IndiaGlobalTechCapital_elec_cool %>%
    rename(subsector = subsector.name) -> L2233.IndiaGlobalTechCapital_elec_cool
    #Now join the 2 files by "subsector"
    L223.IndiaGlobalTechOMvar_elec %>%
      inner_join(L2233.IndiaGlobalTechCapital_elec_cool, by = c("subsector", "year")) -> L2233.IndiaGlobalTechCapital_elec_cool
    L2233.IndiaGlobalTechCapital_elec_cool %>%
      select(-sector.name.x) -> L2233.IndiaGlobalTechCapital_elec_cool
    L2233.IndiaGlobalTechCapital_elec_cool %>%
      mutate(capital.overnight = capital.overnight + OM.var) -> L2233.IndiaGlobalTechCapital_elec_cool
    L2233.IndiaGlobalTechCapital_elec_cool %>%
      select(-input.OM.var, -OM.var) -> L2233.IndiaGlobalTechCapital_elec_cool
    L2233.IndiaGlobalTechCapital_elec_cool %>%
      select(-subsector.name) -> L2233.IndiaGlobalTechCapital_elec_cool
    L2233.IndiaGlobalTechCapital_elec_cool %>%
      rename(subsector.name = subsector) -> L2233.IndiaGlobalTechCapital_elec_cool
    L2233.IndiaGlobalTechCapital_elec_cool %>%
      rename(sector.name = sector.name.y) -> L2233.IndiaGlobalTechCapital_elec_cool
    L2233.IndiaGlobalTechCapital_elec_cool <- L2233.IndiaGlobalTechCapital_elec_cool [c("sector.name", "subsector.name",
                                                                                        "technology", "year", "input.capital", "capital.overnight", "fixed.charge.rate")]



    # Produce outputs
    create_xml("electricity_water_india.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2233.DeleteSupplysector_Indiaelec, "DeleteSupplysector") %>%
      add_xml_data_generate_levels(L2233.IndiaStubTecheleccool,
                                   "StubTech","subsector","nesting-subsector",2,FALSE) %>%
      add_xml_data_generate_levels(L2233.IndiaTechShrwtFlltelec,
                                   "SubsectorShrwtFllt","subsector","nesting-subsector",2,FALSE) %>%
      add_xml_data_generate_levels(L2233.IndiaTechInterpToelec,
                                   "SubsectorInterpTo","subsector","nesting-subsector",2,FALSE) %>%
      add_xml_data_generate_levels(L2233.IndiaTechShrwtnucelec,
                                   "SubsectorInterpTo","subsector","nesting-subsector",2,FALSE) %>%
      add_xml_data_generate_levels(L2233.IndiaTechShrwtrenewelec,
                                   "SubsectorShrwt","subsector","nesting-subsector",2,FALSE) %>%
      add_xml_data_generate_levels(L2233.IndiaStubTechShrwteleccool,
                                   "StubTechShrwt","subsector","nesting-subsector",2,FALSE) %>%
      add_xml_data_generate_levels(L2233.IndiaStubTechCapfactorelecCSP,
                                   "StubTechCapFactor","subsector","nesting-subsector",2,FALSE) %>%
      add_xml_data_generate_levels(L2233.IndiaStubTechEffeleccool,
                                   "StubTechEff","subsector","nesting-subsector",2,FALSE) %>%
      add_xml_data(L2233.IndiaGlobalIntTechCapital_elec_cool,"GlobalIntTechCapital", "GlobalTechCapital") %>%
      add_xml_data(L2233.IndiaGlobalTechCapital_elec_cool,"GlobalTechCapital") %>%
      add_xml_data_generate_levels(L2233.IndiaSubsectorelec_shrwt,
                                   "SubsectorShrwtFllt","subsector","nesting-subsector",1,TRUE) %>%
      add_xml_data_generate_levels(L2233.IndiaSubsectorelec_interp,
                                   "SubsectorInterpTo","subsector","nesting-subsector",1,TRUE) %>%
      #add_xml_data(L223.IndiaGlobalTechCapital_elec,"GlobalTechCapital") %>%#
      add_xml_data(L223.IndiaGlobalIntTechCapital_elec,"GlobalIntTechCapital") %>%
      #add_xml_data(L223.IndiaGlobalTechOMfixed_elec,"GlobalTechOMfixed") %>%#
      add_xml_data(L223.IndiaGlobalIntTechOMfixed_elec,"GlobalIntTechOMfixed") %>%
      #add_xml_data(L223.IndiaGlobalTechOMvar_elec,"GlobalTechOMvar") %>%#
      add_xml_data(L223.IndiaGlobalIntTechOMvar_elec,"GlobalIntTechOMvar") %>%
      add_xml_data(L2233.IndiaAvgFossilEffKeyword_elec_cool, "AvgFossilEffKeyword") %>%
      add_xml_data(L2233.IndiaGlobalIntTechBackup_elec_cool, "GlobalIntTechBackup") %>%
      add_xml_data(L2233.GlobalIntTechEff_elec_cool, "GlobalIntTechEff") %>%
      add_xml_data(L2233.GlobalTechEff_elec_cool, "GlobalTechEff") %>%
      add_xml_data(L2233.IndiaGlobalIntTechCapFac_elec_cool, "GlobalIntTechCapFac") %>%
      add_xml_data(L2233.IndiaGlobalIntTechLifetime_elec_cool, "GlobalIntTechLifetime") %>%
      add_xml_data(L2233.IndiaGlobalIntTechShrwt_elec_cool, "GlobalIntTechShrwt") %>%
      add_xml_data(L2233.IndiaGlobalTechCapFac_elec_cool, "GlobalTechCapFac") %>%
      add_xml_data(L2233.IndiaGlobalTechCapture_elec_cool, "GlobalTechCapture") %>%
      add_xml_data(L2233.IndiaGlobalTechLifetime_elec_cool, "GlobalTechLifetime") %>%
      add_xml_data(L2233.IndiaGlobalTechProfitShutdown_elec_cool, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2233.IndiaGlobalTechSCurve_elec_cool, "GlobalTechSCurve") %>%
      add_xml_data(L2233.IndiaGlobalTechShrwt_elec_cool, "GlobalTechShrwt") %>%
      add_xml_data(L2233.IndiaPrimaryRenewKeyword_elec_cool, "PrimaryRenewKeyword") %>%
      add_xml_data(L2233.IndiaPrimaryRenewKeywordInt_elec_cool, "PrimaryRenewKeywordInt") %>%
      add_xml_data_generate_levels(L2233.IndiaBasinsStubTechTrackCapital_elec,
                                   "StubTechTrackCapital","subsector","nesting-subsector",2,FALSE) %>%
      add_xml_data_generate_levels(L2233.IndiaBasinsStubTechTrackCapital_elec,
                                   "StubTechCost","subsector","nesting-subsector",2,FALSE) %>%
      add_xml_data_generate_levels(L2233.IndiaBasinsStubTechFixOut_hydro,
                                   "StubTechFixOut","subsector","nesting-subsector",2,FALSE) %>%
      add_precursors("L2233.DeleteSupplysector_Indiaelec",
             "L2233.IndiaStubTecheleccool",
             "L2233.IndiaSubsectorelec_shrwt",
             "L2233.IndiaSubsectorelec_interp",
             "L2233.IndiaTechShrwtFlltelec",
             "L2233.IndiaTechInterpToelec",
             "L2233.IndiaTechShrwtnucelec",
             "L2233.IndiaTechShrwtrenewelec",
             "L2233.IndiaStubTechShrwteleccool",
             "L2233.IndiaStubTechCapfactorelecCSP",
             "L2233.IndiaStubTechEffeleccool",
             "L2233.IndiaGlobalIntTechCapital_elec_cool",
             "L2233.IndiaGlobalTechCapital_elec_cool",
             "L223.IndiaGlobalTechCapital_elec",
             "L223.IndiaGlobalIntTechCapital_elec",
             "L223.IndiaGlobalTechOMfixed_elec",
             "L223.IndiaGlobalIntTechOMfixed_elec",
             "L223.IndiaGlobalTechOMvar_elec",
             "L223.IndiaGlobalIntTechOMvar_elec",
             "L2233.IndiaAvgFossilEffKeyword_elec_cool",
             "L2233.IndiaGlobalIntTechBackup_elec_cool",
             "L2233.GlobalIntTechEff_elec_cool",
             "L2233.IndiaGlobalIntTechCapFac_elec_cool",
             "L2233.IndiaGlobalIntTechLifetime_elec_cool",
             "L2233.IndiaGlobalIntTechShrwt_elec_cool",
             "L2233.GlobalTechEff_elec_cool",
             "L2233.IndiaGlobalTechCapFac_elec_cool",
             "L2233.IndiaGlobalTechCapture_elec_cool",
             "L2233.IndiaGlobalTechLifetime_elec_cool",
             "L2233.IndiaGlobalTechProfitShutdown_elec_cool",
             "L2233.IndiaGlobalTechSCurve_elec_cool",
             "L2233.IndiaGlobalTechShrwt_elec_cool",
             "L2233.IndiaPrimaryRenewKeyword_elec_cool",
             "L2233.IndiaPrimaryRenewKeywordInt_elec_cool",
             "L2233.IndiaBasinsStubTechTrackCapital_elec",
             "L2233.IndiaBasinsStubTechFixOut_hydro") ->
      electricity_water_india.xml

    return_data(electricity_water_india.xml)
  } else {
    stop("Unknown command")
  }
}

