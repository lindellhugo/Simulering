## Inflation

Inflation <- function(Pi_t_minus_1, alpha, output_gap_Y_t_minus_1)
{
  Pi_t_expt_minus_1 = max(Pi_t_minus_1, 0)
  
  Pi_t = Pi_t_expt_minus_1 + alpha * output_gap_Y_t_minus_1
  
  return(Pi_t)
}