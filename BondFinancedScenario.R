BondFinancedScenario <- function(economic_state, parameters) {
    
    G_t_star <- (economic_state$output_Y_star / parameters$delta_param * ((parameters$beta_param * (economic_state$real_rate_t - economic_state$neutral_rate_t))
            - parameters$lambda_param * economic_state$output_gap_Y_t))

    G_t <- max(G_t_star, 0)

    if (economic_state$i_t_T > parameters$minRate) {
        ## End of page 94
        ## Money demand determines M
        monetary_base_t <- (economic_state$output_Y_t * economic_state$P_t * exp(parameters$k_param - parameters$gamma_param * (economic_state$nominal_rate_t)))
        ## Lagged M determine open market purchases Z
        omo_t <- (monetary_base_t - economic_state$monetary_base_t_minus_1)
    }
    else {
        ## Case of i_t == 0
        monetary_base_t <- economic_state$monetary_base_t_minus_1
        omo_t <- economic_state$omo_t_minus_1
    }
    policy <- list(
        monetary_base_t = monetary_base_t,
        omo_t = omo_t,
        G_t = G_t)

    return(policy)
}