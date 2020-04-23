source("PlotData.R")
source("BaseModel.R")
source("BaselineScenario.R")
source("BondFinancedScenario.R")
source("HelicopterScenario.R")
source("PermanentExpansionScenario.R")
source("JapaneseParameters.R")
source("SwedishParameters.R")

simulation_parameters <- "Swedish"

if (simulation_parameters == "Swedish") {
     parameters <- SwedishParameters()
} else {
    ## Parameters to use in all simulations
    parameters <- JapaneseParameters()
}


result_baseline <- BaseModel(parameters, BaselineScenario)
result_bond <- BaseModel(parameters, BondFinancedScenario)
result_helicopter <- BaseModel(parameters, HelicopterScenario)
result_permanent <- BaseModel(parameters, PermanentExpansion)
print("Plots:")
PlotData(result_baseline[[1]], result_bond[[1]], result_helicopter[[1]], result_permanent[[1]], "Output/GDP", parameters$legend_pos)
PlotData(result_baseline[[2]], result_bond[[2]], result_helicopter[[2]], result_permanent[[2]], "Inflation")
PlotData(result_baseline[[3]], result_bond[[3]], result_helicopter[[3]], result_permanent[[3]], "Nominal interest rate")
PlotData(result_baseline[[4]], result_bond[[4]], result_helicopter[[4]], result_permanent[[4]], "OMO/GDP")
PlotData(result_baseline[[5]], result_bond[[5]], result_helicopter[[5]], result_permanent[[5]], "Monetary base/GDP")
PlotData(result_baseline[[6]], result_bond[[6]], result_helicopter[[6]], result_permanent[[6]], "Dept/GDP")
