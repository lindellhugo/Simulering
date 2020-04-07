source("SimulationBaseLinePolicy.R")
source("SimulationBondFinanced.R")
source("SimulationHelicopter.R")
source("JapaneseParameters.R")

simulation <- "all" ## "baseline", "bond", "helicopter"
simulation_parameters <- "Japanese"

if (simulation_parameters == "Japanese") {
    ## Parameters to use in all simulations
    parameters <- JapaneseParameters()
}


if ("bond" == simulation) {
    BondFinanced(parameters)
}else if ("baseline" == simulation) {
    BaseLinePolicy(parameters)
}else if ("helicopter" == simulation) {
    Helicopter(parameters)
} else {
    result_baseline <- BaseLinePolicy(parameters)
    result_bond <- BondFinanced(parameters)
    result_helicopter <- Helicopter(parameters)
    plot(result_baseline[[1]],type="l", col="red", main="Output/GDP")
    lines(result_bond[[1]])
    lines(result_helicopter[[1]], col = "green", lty = 2)
    plot(result_baseline[[2]], type = "l", col = "red", main = "Inflation")
    lines(result_bond[[2]])
    lines(result_helicopter[[2]], col = "green", lty = 2)
    plot(result_baseline[[3]], type = "l", col = "red", main = "Nominal interest rate")
    lines(result_bond[[3]])
    lines(result_helicopter[[3]], col = "green", lty = 2)
    plot(result_baseline[[4]], ylim = -c(0.16, -0.08), type = "l", col = "red", main = "OMO/GDP")
    lines(result_bond[[4]])
    lines(result_helicopter[[4]], col = "green", lty = 2)
    plot(result_baseline[[5]], ylim = c(0.06, 0.30), type = "l", col = "red", main = "Monetary base/GDP")
    lines(result_bond[[5]])
    lines(result_helicopter[[5]], col = "green", lty = 2)
    plot(result_baseline[[6]], ylim = c(0.6, 0.88), type = "l", col = "red", main = "Dept/GDP")
    lines(result_bond[[6]])
    lines(result_helicopter[[6]], col = "green", lty = 2)
}