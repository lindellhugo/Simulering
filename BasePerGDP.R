## Monetary base per GDP

BasePerGDP <- function(P_t, i_t, k_param, gamma_param)
{
  base_per_GDP <- P_t * exp(k_param) * exp( - gamma_param * i_t)  
  return(base_per_GDP)
}