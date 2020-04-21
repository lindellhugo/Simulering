TaylorRule <- function(neutral_rate_t, inflation_t, a_param, b_param, output_gap_Y_t, inflation_target, min_rate) {
    ## The rate is set using a Taylor rule
    i_t_T <- (neutral_rate_t + inflation_t + a_param * output_gap_Y_t + b_param * (inflation_t - inflation_target))
    nominal_rate_t <- max(i_t_T, min_rate)
    real_rate_t <- nominal_rate_t - inflation_t

    rates <- list(
        i_t_T = i_t_T,
        nominal_rate_t = nominal_rate_t,
        real_rate_t = real_rate_t
        )
    return(rates)
}