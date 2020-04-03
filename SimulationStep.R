setClass("simulationStep",
         slots=list(
           step="numeric"
           ))

NewSimulationStep <- function(step)
{
    return(new("simulationStep", step=step))
}