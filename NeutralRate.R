NeutralRate <- function(time_period)
{
  
  if (time_period < 11)
    return(- 0.02 + (0.04 / 10 * (time_period-1)))
  else
    return(0.02)
}