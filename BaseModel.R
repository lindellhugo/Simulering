## File:BaseModel.R
## Purpose: The base model dictating the evolution on the economy based on the parameters and used scenario

BaseModel <- function(parameters, scenario) {

    print("BaseModel")

    ## Steps in simulation
    number_of_steps <- parameters$number_of_steps

    ## Evolving data lists
    inflation_percent_list <- c(1:number_of_steps + 1)
    output_gap_percent_list <- c(1:number_of_steps + 1)
    output_Y_list <- c(1:number_of_steps + 1)
    output_Y_star_list <- c(1:number_of_steps + 1)
    nominal_interest_rate_list <- c(1:number_of_steps + 1)
    neutral_rate_list <- c(1:number_of_steps + 1)
    real_rate_list <- c(1:number_of_steps + 1)
    monetary_base_list <- c(1:number_of_steps + 1)
    monetary_base_per_gdp_list <- c(1:number_of_steps + 1)
    dept_list <- c(1:number_of_steps + 1)
    dept_per_gdp_list <- c(1:number_of_steps + 1)
    omo_list <- c(1:number_of_steps + 1)
    P_t_list <- c(1:number_of_steps + 1)
    step_list <- c(1:number_of_steps + 1)
    G_t_list <- c(1:number_of_steps + 1)
    G_t_per_gdp_list <- c(1:number_of_steps + 1)
    neutral_rate_list <- c(1:number_of_steps + 1)
    omo_list <- c(1:number_of_steps + 1)
    omo_per_gdp_list <- c(1:number_of_steps + 1)
    P_t_list <- c(1:number_of_steps + 1)
    step_list <- c(1:number_of_steps + 1)

    ## Initial values
     
    ## Constants
    alpha_param <- parameters$alpha_param
    beta_param <- parameters$beta_param
    delta_param <- parameters$delta_param
    gamma_param <- parameters$gamma_param
    lambda_param <- parameters$lambda_param
    theta_param <- parameters$theta_param
    k_param <- parameters$k_param
    g_param <- parameters$g_param
    inflation_target = parameters$inflation_target

    ## Evolves
    inflation_percent_list[1] <- parameters$inflation_percent_gap
    output_gap_percent_list[1] <- parameters$output_gap_percent_gap
    output_Y_list[1] <- 1.0 + output_gap_percent_list[1]
    nominal_interest_rate_list[1] <- parameters$nominal_interest_rate
    monetary_base_list[1] <- parameters$monetary_base_per_gdp * output_Y_list[1]
    dept_list[1] <- parameters$dept_per_gdp * output_Y_list[1]
    omo_list[1] <- output_Y_list[1] * parameters$omo_per_gdp
    omo_per_gdp_list[1] <- parameters$omo_per_gdp
    P_t_list[1] <- parameters$P_t
    dept_per_gdp_list[1] <- dept_list[1] / output_Y_list[1]
    monetary_base_per_gdp_list[1] <- monetary_base_list[1] / output_Y_list[1]
    G_t_list[1] <- 0.0
    G_t_per_gdp_list[1] <- 0.0
    real_rate_list[1] <- nominal_interest_rate_list[1] - inflation_percent_list[1]
    state <- list(
        time_period = 1,
        output_gap_Y_t = output_gap_percent_list[1]
        )
    neutral_rate_list[1] <- parameters$neutralRate(state)
    output_Y_star_list = (1 + g_param) ^ c(0:number_of_steps + 1)

    ## Policy values
    a_param = parameters$a_param
    b_param = parameters$b_param


    ## Below, the base scenario is coded:
    for (step_i in c(2:(number_of_steps + 1))) {

        ## Values from previous iteration
        output_Y_t_minus_1 <- output_Y_list[step_i - 1]
        output_Y_star_t_minus_1 <- output_Y_star_list[step_i - 1]
        output_gap_Y_t_minus_1 <- output_gap_percent_list[step_i - 1]
        inflation_t_minus_1 <- inflation_percent_list[step_i - 1]
        neutral_rate_t_minus_1 <- neutral_rate_list[step_i - 1]
        nominal_rate_t_minus_1 <- nominal_interest_rate_list[step_i - 1]
        real_rate_t_minus_1 <- real_rate_list[step_i - 1]
        omo_t_minus_1 <- omo_list[step_i - 1]
        monetary_base_t_minus_1 <- monetary_base_list[step_i - 1]
        dept_t_minus_1 <- dept_list[step_i - 1]
        P_t_minus_1 <- P_t_list[step_i - 1]
        G_t_minus_1 <- G_t_list[step_i - 1]

        ## Update values for current interation

        ## Variables for the economy

        ## Output gap
        output_gap_Y_t <- (lambda_param * output_gap_Y_t_minus_1
                     - beta_param * (real_rate_t_minus_1 - neutral_rate_t_minus_1)
                     + delta_param * (G_t_minus_1 / output_Y_star_t_minus_1))

        output_Y_star <- output_Y_star_list[step_i]

        output_Y_t <- output_Y_star * (output_gap_Y_t + 1.0)

        ## Inflation
        inflation_expt_t <- max(inflation_t_minus_1, 0)
        inflation_t <- inflation_expt_t + alpha_param * output_gap_Y_t_minus_1

        ## Price level
        P_t <- P_t_minus_1 * (1 + inflation_t_minus_1)

        ## Neutral rate
        state <- list(
            time_period = step_i,
            output_gap_Y_t = output_gap_Y_t
        )
        neutral_rate_t <- parameters$neutralRate(state)

        ## Save the current economic state
        economic_state <- list(
            time_period = step_i,
            neutral_rate_t = neutral_rate_t,
            neutral_rate_t_minus_1 = neutral_rate_t_minus_1,
            output_Y_t = output_Y_t,
            output_Y_t_minus_1 = output_Y_t_minus_1,
            output_gap_Y_t = output_gap_Y_t,
            output_gap_Y_t_minus_1 = output_gap_Y_t_minus_1,            
            output_Y_star = output_Y_star,
            output_Y_star_t_minus_1 = output_Y_star_t_minus_1,
            inflation_t = inflation_t,
            inflation_t_minus_1 = inflation_t_minus_1,
            P_t = P_t,
            P_t_minus_1 = P_t_minus_1,
            G_t_minus_1 = G_t_minus_1,
            monetary_base_t_minus_1 = monetary_base_t_minus_1,
            omo_t_minus_1 = omo_t_minus_1,
            nominal_rate_t_minus_1 = nominal_rate_t_minus_1,
            real_rate_t_minus_1 = real_rate_t_minus_1
        )

        scenarioResult <- scenario(economic_state, parameters)
        G_t <- scenarioResult$G_t
        monetary_base_t <- scenarioResult$monetary_base_t
        omo_t <- scenarioResult$omo_t
        nominal_rate_t <- scenarioResult$nominal_rate_t
        real_rate_t <- scenarioResult$real_rate_t

        ## The dept in the economy
        dept_t = (dept_t_minus_1 * (1.0 + nominal_rate_t_minus_1) +
              P_t * G_t - omo_t -
              theta_param * P_t * (output_Y_t - output_Y_star))


        ## Save results
        neutral_rate_list[step_i] <- neutral_rate_t
        output_gap_percent_list[step_i] <- output_gap_Y_t
        output_Y_list[step_i] <- output_Y_t
        inflation_percent_list[step_i] <- inflation_t
        nominal_interest_rate_list[step_i] <- nominal_rate_t
        real_rate_list[step_i] <- real_rate_t
        omo_list[step_i] <- omo_t
        omo_per_gdp_list[step_i] <- omo_t / P_t / output_Y_t
        monetary_base_list[step_i] <- monetary_base_t
        monetary_base_per_gdp_list[step_i] = monetary_base_t  / output_Y_t / P_t
        dept_list[step_i] <- dept_t
        dept_per_gdp_list[step_i] <- dept_t  / output_Y_t / P_t
        P_t_list[step_i] <- P_t
        G_t_list[step_i] <- G_t
        G_t_per_gdp_list[step_i] <- G_t / output_Y_t / P_t
        print("Iteration :")
        print(step_i)
    }

    print("End of simulation")

    result <- list(output_gap_percent_list, inflation_percent_list, nominal_interest_rate_list, omo_per_gdp_list, monetary_base_per_gdp_list, dept_per_gdp_list, G_t_per_gdp_list, real_rate_list)
    return(result)
}

## End File