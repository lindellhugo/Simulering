## Money

Money <- function(M_t_minus_1, Z_t)
{
  M_t  = M_t_minus_1 + Z_t
  
  return(M_t)
}