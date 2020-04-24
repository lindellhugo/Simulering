PermanentExpansion <- function(economic_state, parameters) {

    rates <- TaylorRule(economic_state$neutral_rate_t, economic_state$inflation_t, parameters$a_param, parameters$b_param,
        economic_state$output_gap_Y_t, parameters$inflation_target, parameters$minRate)
    omo_t <- 0
    monetary_base_t <- economic_state$monetary_base_t_minus_1
    if (rates$i_t_T > parameters$minRate) {
        ## Money demand determines M
        monetary_base_t_prim <- (economic_state$output_Y_t * economic_state$P_t * exp(parameters$k_param - parameters$gamma_param * (rates$nominal_rate_t)))
        ## Lagged M determine open market purchases Z
        omo_t_prim <- (monetary_base_t_prim - economic_state$monetary_base_t_minus_1)
        rates$nominal_rate_t = economic_state$nominal_rate_t_minus_1
        rates$real_rate_t = economic_state$real_rate_t_minus_1
    } 

    ## Fiscal policy
    G_t_star <- (economic_state$output_Y_star / parameters$delta_param * ((parameters$beta_param * (rates$real_rate_t - economic_state$neutral_rate_t))
            - parameters$lambda_param * economic_state$output_gap_Y_t))

    G_t <- max(G_t_star, 0)

    if (G_t > 0) {
        omo_t <- omo_t + G_t * economic_state$P_t
        monetary_base_t <- monetary_base_t + omo_t
    } 


    policy <- list(
        monetary_base_t = monetary_base_t,
        omo_t = omo_t,
        G_t = G_t,
        i_t_T = rates$i_t_T,
        nominal_rate_t = rates$nominal_rate_t,
        real_rate_t = rates$real_rate_t)

    return(policy)
}