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
SimulateBaselineData <- function(parameters, new_value, p, comparaison_factor, function_name = "") {
    new_parameters <- parameters
    new_parameters[[p]] = new_value
    result_baseline <- BaseModel(new_parameters, BaselineScenario)
    return(result_baseline)
}
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
        double_value <- 1.5 * original_value
        half_value <- 0.5 * original_value
        half_result <- Simulate(parameters, half_value, p, comparaison_factor)
        main_table <- rbind(main_table, half_result)
        original_result <- Simulate(parameters, original_value, p, comparaison_factor)
        main_table <- rbind(main_table, original_result)
        double_result <- Simulate(parameters, double_value, p, comparaison_factor)
        main_table <- rbind(main_table, double_result)
    }


    ## Special test case for the neutral rate
    p_neutral_rate <- c("neutralRate")
    for (rate in c(0.0, 0.01, 0.02, 0.03)) {
        # With fixed rate
        fixed_rate_value <- function(state) {
            return(rate) # Adjust here to adjust level
        }
        fixed_rate_result <- Simulate(parameters, fixed_rate_value, p_neutral_rate, comparaison_factor, paste0("fixed ", 100 * rate, "%"))
        main_table <- rbind(main_table, fixed_rate_result)
    }

    ## Special test case for the neutral rate
    p_min_rate <- c("minRate")
    zero_result <- Simulate(parameters, 0, p_min_rate, comparaison_factor)
    zero_result_baseline <- SimulateBaselineData(parameters, 0, p_min_rate, comparaison_factor)
    main_table <- rbind(main_table, zero_result)
    original_result <- Simulate(parameters, -0.5 / 100, p_min_rate, comparaison_factor)
    original_result_baseline <- SimulateBaselineData(parameters, -0.5 / 100, p_min_rate, comparaison_factor)
    main_table <- rbind(main_table, original_result)
    only_taylor_result <- Simulate(parameters, - Inf, p_min_rate, comparaison_factor)
    only_taylor_result_baseline <- SimulateBaselineData(parameters, - Inf, p_min_rate, comparaison_factor)
    main_table <- rbind(main_table, only_taylor_result)

    write.csv(main_table, "robustnessDeptGDP.csv", row.names = TRUE)

    # Plotting when using different min rates
    # Change to true to show the plot
    if (FALSE) {
        plot.new()
        titles = c("Output gap", "Inflation", "Nominal interest rate", "OMO/GDP", "Monetary base/GDP", "Dept/GDP", "Transfers/GDP")
        lengendpos = c("bottomright", "bottomleft", "topleft", "bottomright", "bottomleft", "bottomleft", "bottomright")
        usePercent = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
        texts <- c("0%", "-0.5%", "None") #, "Permanent")
        par(mfrow = c(2, 3))
        for (index in 1:7) {
            if (index == 4 || index == 4 || index == 4) {
                next
            }
            title <- titles[index]
            pos <- lengendpos[index]
            Plotting(zero_result_baseline[[index]], original_result_baseline[[index]], only_taylor_result_baseline[[index]], only_taylor_result[[index]], title, texts, "bottomleft", usePercent[index])
        }
    }

    # Plotting when using different assumptions for monetary demand
    # Change to true to show the plot
    if (FALSE) {
        # Plotting when using another monetary demand assumption
        plot.new()
        p_assumption <- c("fixed_monetary_demand_at_min_rate")
        original_assumption_result <- SimulateBaselineData(parameters, TRUE, p_assumption, comparaison_factor)
        without_assumption_result <- SimulateBaselineData(parameters, FALSE, p_assumption, comparaison_factor)

        titles = c("Output gap", "Inflation", "Nominal interest rate", "OMO/GDP", "Monetary base/GDP", "Debt/GDP", "Transfers/GDP")
        lengendpos = c("bottomright", "bottomleft", "topleft", "bottomright", "bottomleft", "bottomleft", "bottomright")
        usePercent = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
        texts <- c("With assumption", "Without assumption")
        par(mfrow = c(2, 3))
        for (index in 1:7) {
            if (index != 4 ) {
                title <- titles[index]
                pos <- lengendpos[index]
                Plotting(original_assumption_result[[index]], without_assumption_result[[index]], c(), NULL, title, texts, "bottomleft", usePercent[index])
            }            
        }
    }
}
