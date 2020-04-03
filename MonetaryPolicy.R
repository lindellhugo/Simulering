## Monetary policy

MonetaryPolicy <- function(Pi_t, Pi_target, a, b, output_gap_Y_t, r_t_star)
{
  i_t_T = (r_t_star + Pi_t + a *output_gap_Y_t_minus_1 + b * (Pi_t - Pi_target))
  i_t = max(i_t_T, 0)
    
  return(i_t)
}