## Monetary base per GDP

BasePerGDP <- function(P_t, i_t, gamma_param, k_param)
{
  base_per_GDP <- P_t * exp(k_param - gamma_param * i_t)  
  
  return(base_per_GDP)
}