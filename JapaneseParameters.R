source("NeutralRateJapan.R")
JapaneseParameters <- function() {
    japanese_parameters <- list(
        number_of_steps = 25,
        alpha_param = 0.2,
        beta_param = 1.0,
        delta_param = 1.25, ## Kuttner and Posen 2001
        gamma_param = 0.1,
        lambda_param = 0.6,
        theta_param = 0.25,
        k_param = log(0.1),
        g_param = 0.02,
        inflation_target = 0.02,
        inflation_percent_gap = -1.0 / 100,
        output_gap_percent_gap = -7.5 / 100,
        nominal_interest_rate = 0,
        monetary_base_per_gdp = 0.2,
        dept_per_gdp = 0.79,
        omo_per_gdp = 0.0,
        P_t = 1.0,
        a_param = 1.1,
        b_param = 2.5,
        minRate = 0,
        neutralRate = NeutralRateJapan,
        legend_pos = "bottomright"
        )
    return(japanese_parameters)
}
