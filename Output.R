## Output

Output <- function(lambda_param, beta, output_gap_Y_t_minus_1, r_t_minus_1, r_t_star_minus_1,delta, G_t_minus_1_over_Y_star_t_minus_1)
{
  output_growth = (lambda_param * output_gap_Y_t_minus_1 
                  - beta * (r_t_minus_1 - r_t_star_minus_1) 
                  + delta * (G_t_minus_1_over_Y_star_t_minus_1)) 
  return(output_growth)
}

