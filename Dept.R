## Dept

Dept <- function(D_t_per_GDP, i_t, P_t, G_t_per_GDP, Z_t_per_GDP, theta, output_gap_Y_t)
{
  
  D_t = D_t_per_GDP * (1.0 + i_t) + P_t * G_t_per_GDP - Z_t_per_GDP - theta * P_t * output_gap_Y_t
  return(D_t)
  
}