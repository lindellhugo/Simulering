## File:Simulation.R
## Purpose: Start of the simulation. Decides which parameters that should be used.

source("PlotData.R")
source("BaseModel.R")
source("BaselineScenario.R")
source("BondFinancedScenario.R")
source("HelicopterScenario.R")
source("PermanentExpansionScenario.R")
source("JapaneseParameters.R")
source("SwedishParameters.R")
source("RobustnessTest.R")

# Decide which parameters to use
simulation_parameters <- "Swedish"# Use "Swedish" or "Japanese"

if (simulation_parameters == "Swedish") {
     parameters <- SwedishParameters()
} else {
    parameters <- JapaneseParameters()
}

## Calculate the result for the different scenarios
result_baseline <- BaseModel(parameters, BaselineScenario)
result_bond <- BaseModel(parameters, BondFinancedScenario)
result_helicopter <- BaseModel(parameters, HelicopterScenario)
result_permanent <- BaseModel(parameters, PermanentExpansion)

## Plot the result and save as images
PlotData(result_baseline, result_bond, result_helicopter, result_permanent, parameters)

## Perform robustness test
RobustnessTest(parameters)

## End File