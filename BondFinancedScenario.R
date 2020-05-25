## File:BondFinancedScenario.R
## Purpose: Depicts the evolution of the bond financed scenario


source("TaylorRule.R")
BondFinancedScenario <- function(economic_state, parameters) {
        
    rates <- TaylorRule(economic_state$neutral_rate_t, economic_state$inflation_t, parameters$a_param, parameters$b_param,
        economic_state$output_gap_Y_t, parameters$inflation_target, parameters$minRate)

    ## Fiscal policy
    G_t_star <- (economic_state$output_Y_star / parameters$delta_param * ((parameters$beta_param * (rates$real_rate_t - economic_state$neutral_rate_t))
            - parameters$lambda_param * economic_state$output_gap_Y_t))

    G_t <- max(parameters$goverment_stimuli_assumption * G_t_star, 0)

    if (rates$i_t_T > parameters$minRate || !parameters$fixed_monetary_demand_at_min_rate) {
        ## Money demand determines M
        monetary_base_t <- (economic_state$output_Y_t * economic_state$P_t * exp(parameters$k_param - parameters$gamma_param * (rates$nominal_rate_t)))
        ## Lagged M determine open market purchases Z
        omo_t <- (monetary_base_t - economic_state$monetary_base_t_minus_1)
    }
    else {
        monetary_base_t <- economic_state$monetary_base_t_minus_1
        omo_t <- economic_state$omo_t_minus_1
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

## End File