source("NeutralRateSweden.R")
SwedishParameters <- function() {
    swedish_parameters <- list(
        number_of_steps = 15,
        alpha_param = 0.3, ## Philips kurvan
        beta_param = 1.0, ##
        delta_param = 1.3, ## Kuttner and Posen 2001
        gamma_param = 0.122,
        lambda_param = 0.9,
        theta_param = 0.45,
        k_param = log(0.66),
        g_param = 0.0126,
        inflation_target = 0.02, 
        inflation_percent_gap = 2.0 / 100,
        output_gap_percent_gap = -0.4 / 100,
        nominal_interest_rate = -0.5 / 100,
        monetary_base_per_gdp = 0.72,
        dept_per_gdp = 0.48,
        omo_per_gdp = 0.0,
        P_t = 1.0,
        a_param = 0.1, # Output parameter
        b_param = 1.5, # Inflation parameter
        minRate = -0.5 / 100,
        neutralRate = NeutralRateSweden,
        legend_pos = "topleft"
        )
    return(swedish_parameters)
}
