source("PlotData.R")
source("BaseModel.R")
source("BaselineScenario.R")
source("BondFinancedScenario.R")
source("HelicopterScenario.R")
source("PermanentExpansionScenario.R")
source("JapaneseParameters.R")
source("SwedishParameters.R")

# The parameters to investigate
toTest = c("beta_param", "gamma_param", "k_param")

Simulate <- function(parameters, new_value, p, comparaison_factor, function_name = "") {
    new_parameters <- parameters
    new_parameters[[p]] = new_value
    result_baseline <- BaseModel(new_parameters, BaselineScenario)
    result_bond <- BaseModel(new_parameters, BondFinancedScenario)
    result_helicopter <- BaseModel(new_parameters, HelicopterScenario)

    result <- list(
            result_baseline = result_baseline,
            result_bond = result_bond,
            result_helicopter = result_helicopter
        )
    if (is.function(new_value)) {
        row_label <- paste0(p, " = ", function_name)
    } else {
        row_label <- paste0(p, " = ", new_value)
    }
    row_data <- c(row_label,
                  tail(result_baseline[[comparaison_factor]], 1),
                  tail(result_bond[[comparaison_factor]], 1),
                  tail(result_helicopter[[comparaison_factor]], 1)
               )

    return(GetResultRow(row_data))
}

GetResultRow <- function(result) {
    return(GetDFRow(c(result[1]), c(result[2]), c(result[3]), c(result[4])))
}

GetDFRow <- function(value = c(), base = c(), bond = c(), helicopter = c()) {
    return(data.frame(Value = value, Baseline = base, Bond = bond, Helicopter = helicopter))
}


RobustnessTest <- function(parameters) {

    comparaison_factor = 6 # 6 implies Dept/GDP 

    main_table <- GetDFRow()

    for (p in toTest) {
        original_value <- parameters[[p]]
        double_value <- 2 * original_value
        half_value <- 0.5 * original_value
        half_result <- Simulate(parameters, half_value, p, comparaison_factor)
        main_table <- rbind(main_table, half_result)
        original_result <- Simulate(parameters, original_value, p, comparaison_factor)
        main_table <- rbind(main_table, original_result)
        double_result <- Simulate(parameters, double_value, p, comparaison_factor)
        main_table <- rbind(main_table, double_result)
    }

    special_k <- Simulate(parameters, log(0.85*0.66), c("k_param"), comparaison_factor)
    main_table <- rbind(main_table, special_k)
    special_k <- Simulate(parameters, log(1.15*0.66), c("k_param"), comparaison_factor)
    main_table <- rbind(main_table, special_k)
    special_k <- Simulate(parameters, -0.234, c("k_param"), comparaison_factor)
    main_table <- rbind(main_table, special_k)
    ## Special test case for the neutral rate
    # Original case:
    p_neutral_rate <- c("neutralRate")
    original_value <- parameters[[p_neutral_rate]]
    original_result <- Simulate(parameters, original_value, p_neutral_rate, comparaison_factor, "original (increasing)")
    main_table <- rbind(main_table, original_result)

    for (rate in c(0.0, 0.01, 0.02)) {
        # With fixed rate
        fixed_rate_value <- function(state) {
            return(rate) # Adjust here to adjust level
        }
        fixed_rate_result <- Simulate(parameters, fixed_rate_value, p_neutral_rate, comparaison_factor, paste0("fixed ", rate, "%"))
        main_table <- rbind(main_table, fixed_rate_result)
    }

    write.csv(main_table, "robustnessDeptGDP.csv", row.names = TRUE)

}
