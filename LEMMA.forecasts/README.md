
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LEMMA.forecasts

This package simply collects the functions in **LEMMA-Forecasts** into a
package, for easier use in the Shiny application. Within the `R/`
directory the code is organized as:

1.  `GetCountyData_Scenario.R`
    1.  `GetCountyInputs_scen`
    2.  `Scenario`
    3.  `GetResults_scen`
2.  `GetCountyData.R`
    1.  `GetCountyData`
    2.  `GetStateData`
    3.  `ReadCsvAWS`
    4.  `ConvertNegative`
    5.  `GetOldAdmits`
    6.  `GetAdmits`
    7.  `GetAdmits_notused`
    8.  `GetSantaClaraData`
3.  `GetCountyInputs.R`
    1.  `Get1`
    2.  `GetCountySheets`
    3.  `GetCountyInputs`
4.  `GetDosesData.R`
    1.  `GetDosesData.old`
    2.  `GetDosesData`
    3.  `ReadSFDoses.old`
    4.  `SFDosesToAWS`
    5.  `ReadSFDoses`
5.  `RunCounty_Scenario.R`
    1.  `RunOneCounty_scen`
6.  `RunCounty.R`
    1.  `RunOneCounty`
