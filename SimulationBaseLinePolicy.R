source("NeutralRate.R")

BaseLinePolicy <- function() {

    print("Baseline policy")

    ## Steps in simulation
    number_of_steps <- 25

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
    G_t_minus_1_over_Y_star_t_minus_1_list <- c(1:number_of_steps + 1)
    neutral_rate_list <- c(1:number_of_steps + 1)
    real_rate_list <- c(1:number_of_steps + 1)


    ## Initial values

    ## Evolves
    inflation_percent_list[1] <- -1.0 / 100
    output_gap_percent_list[1] <- -7.5 / 100
    output_Y_list[1] <- 1.0 + output_gap_percent_list[1]
    nominal_interest_rate_list[1] <- 0
    monetary_base_list[1] <- 0.2 * output_Y_list[1]
    omo_list <- c(1:number_of_steps + 1)
    omo_per_gdp_list <- c(1:number_of_steps + 1)
    P_t_list <- c(1:number_of_steps + 1)
    step_list <- c(1:number_of_steps + 1)
    dept_list[1] <- 0.79 * output_Y_list[1]
    omo_list[1] <- 0.0
    omo_per_gdp_list[1] <- 0.0
    P_t_list[1] <- 1.0
    dept_per_gdp_list[1] <- dept_list[1] / output_Y_list[1]
    monetary_base_per_gdp_list[1] <- monetary_base_list[1] / output_Y_list[1]

    ## Constants
    alpha_param <- 0.2
    beta_param <- 1.0
    delta_param <- 1.25 ## Kuttner and Posen 2001
    gamma_param <- 0.1
    lambda_param <- 0.6
    theta_param <- 0.25
    k_param <- log(0.1)
    g_param <- 0.02
    inflation_target = 0.02


    output_Y_star_list = (1 + g_param) ^ c(0:number_of_steps + 1)

    ## Policy values
    G_t <- 0 ## Always zero in base scenario
    a_param = 1.1
    b_param = 2.5


    ## Below, the base scenario is coded:
    for (step_i in c(2:(number_of_steps + 1))) {

        ## Values from previous iteration
        output_Y_t_minus_1 <- output_Y_list[step_i - 1]
        output_Y_star_t_minus_1 <- output_Y_star_list[step_i - 1]
        output_gap_Y_t_minus_1 <- output_gap_percent_list[step_i - 1]

        inflation_t_minus_1 <- inflation_percent_list[step_i - 1]

        neutral_rate_t_minus_1 <- NeutralRate(step_i - 1)
        neutral_rate_list[step_i - 1] <- neutral_rate_t_minus_1
        nominal_rate_t_minus_1 <- nominal_interest_rate_list[step_i - 1]
        real_rate_t_minus_1 <- nominal_rate_t_minus_1 - inflation_t_minus_1
        omo_t_minus_1 <- omo_list[step_i - 1]
        monetary_base_t_minus_1 <- monetary_base_list[step_i - 1]
        dept_t_minus_1 <- dept_list[step_i - 1]
        P_t_minus_1 <- P_t_list[step_i - 1]

        ## Update values for current interation

        ## Variables for the economy

        ## Output gap
        output_gap_Y_t <- (lambda_param * output_gap_Y_t_minus_1
                     - beta_param * (real_rate_t_minus_1 - neutral_rate_t_minus_1)
                     + delta_param * (G_t))

        output_Y_star <- output_Y_star_list[step_i]

        output_Y_t <- output_Y_star * (output_gap_Y_t + 1.0)

        ## Inflation
        inflation_expt_t <- max(inflation_t_minus_1, 0)
        inflation_t <- inflation_expt_t + alpha_param * output_gap_Y_t_minus_1

        ## Price level
        P_t <- P_t_minus_1 * (1 + inflation_t)


        ## Baseline policy
        ## A. Monetary policy
        neutral_rate_t <- NeutralRate(step_i)
        ## The rate is set as
        i_t_T <- (neutral_rate_t + inflation_t + a_param * output_gap_Y_t + b_param * (inflation_t - inflation_target))
        nominal_rate_t <- max(i_t_T, 0)
        ##browser()

        if (nominal_rate_t > 0) {
            ## End of page 94
            ## Money demand determines M
            monetary_base_t <- (output_Y_t * P_t * exp(k_param - gamma_param * nominal_rate_t))
            #monetary_base_t <- 
            ## Lagged M determine open market purchases Z
            omo_t <- (monetary_base_t - monetary_base_t_minus_1)
        }
        else {
            ## Case of i_t == 0
            monetary_base_t <- monetary_base_t_minus_1
            omo_t <- omo_t_minus_1
        }

        ## The dept in the economy
        dept_t = (dept_t_minus_1 * (1.0 + nominal_rate_t_minus_1) +
              P_t * G_t - omo_t -
              theta_param * P_t * (output_Y_t - output_Y_star))


        ## Save results
        output_gap_percent_list[step_i] <- output_gap_Y_t
        output_Y_list[step_i] <- output_Y_t
        inflation_percent_list[step_i] <- inflation_t
        nominal_interest_rate_list[step_i] <- nominal_rate_t
        omo_list[step_i] <- omo_t
        omo_per_gdp_list[step_i] <- omo_t / P_t / output_Y_t
        monetary_base_list[step_i] <- monetary_base_t
        monetary_base_per_gdp_list[step_i] = monetary_base_t / P_t / output_Y_t
        dept_list[step_i] <- dept_t
        dept_per_gdp_list[step_i] <- dept_t / P_t / output_Y_t
        P_t_list[step_i] <- P_t
        print("Iteration :")
        print(step_i)
    }

    print("End of simulation")

    par(mfrow = c(3, 2))
    plot(output_gap_percent_list, type = "l")
    plot(inflation_percent_list, type = "l")
    plot(nominal_interest_rate_list, type = "l")
    plot(omo_per_gdp_list, type = "l")
    plot(monetary_base_per_gdp_list, type = "l")
    plot(dept_per_gdp_list, type = "l")
    result <- list(output_gap_percent_list, inflation_percent_list, nominal_interest_rate_list, omo_per_gdp_list, monetary_base_per_gdp_list , dept_per_gdp_list)
    return(result)
}