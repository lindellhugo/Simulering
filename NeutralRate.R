NeutralRate <- function(time_period)
{
  
  if (time_period < 10)
    return(- 0.02 + (0.04 / 10 * time_period))
  else
    return(0.02)
}