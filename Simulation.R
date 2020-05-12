source("PlotData.R")
source("BaseModel.R")
source("BaselineScenario.R")
source("BondFinancedScenario.R")
source("HelicopterScenario.R")
source("PermanentExpansionScenario.R")
source("JapaneseParameters.R")
source("SwedishParameters.R")
source("RobustnessTest.R")

simulation_parameters <- "Japanese"#"Swedish","Japanese"

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
PlotData(result_baseline, result_bond, result_helicopter, result_permanent, parameters)

RobustnessTest(parameters)