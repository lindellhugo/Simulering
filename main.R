## Source files to use
source("SimulationStep.R")
source("Inflation.R")
source("Output.R")
source("Dept.R")
source("MonetaryPolicy.R")
source("Money.R")
source("NeutralRate.R")
source("BasePerGDP.R")


print("Helicopter money simulation")

## Steps in simulation
number_of_steps <- 25

## Evolving data listsC
inflation_percent_list <- c(1: number_of_steps+1)
output_gap_percent_list <- c(1:number_of_steps+1)
nominal_interest_rate_list <- c(1:number_of_steps+1)
neutral_rate_list <- c(1: number_of_steps+1)
real_rate_list <- c(1:number_of_steps+1)
base_GDP_list <- c(1: number_of_steps+1)
dept_GDP_list <- c(1: number_of_steps+1)
omo_GDP_list <- c(1:number_of_steps+1)
P_t_list <- c(1: number_of_steps+1)
step_list <- c(1: number_of_steps+1)
G_t_minus_1_over_Y_star_t_minus_1_list <- c(1: number_of_steps +1)


## Initial values

## Evolves
output_gap_percent_list[1] <- -7.5 / 100
inflation_percent_list[1] <- -1.0 / 100
nominal_interest_rate_list[1] <- 0
base_GDP_list[1] <- 0.2
dept_GDP_list[1] <- 0.79
omo_GDP_list[1] <- 0.0
P_t_list[1] <- 1.0

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

## Policy values
G_t_minus_1_over_Y_star_t_minus_1 <- 0 ## Always zero in base scenario
a_param = 1.1
b_param = 2.5


## Below, the base scenario is coded:
for (step_i in c(2:(number_of_steps+1))) {
  
  ## Values from previous iteration
  output_gap_Y_t_minus_1 <-  output_gap_percent_list[step_i-1]
  inflation_t_minus_1 <- inflation_percent_list[step_i-1]
  neutral_rate_t_minus_1 <- NeutralRate(step_i-1)
  neutral_rate_list[step_i-1] <- neutral_rate_t_minus_1
  nominal_rate_t_minus_1 <- nominal_interest_rate_list[step_i-1]
  real_rate_t_minus_1 <- nominal_rate_t_minus_1 - inflation_t_minus_1 
  omo_GDP_t_minus_1 <- omo_GDP_list[step_i-1]
  base_GDP_t_minus_1 <- base_GDP_list[step_i-1] 
  dept_t_minus_1 <- dept_GDP_list[step_i - 1]
  P_t_minus_1 <- P_t_list[step_i - 1]
  
  
  ## Update values for current interation
  
  ## Variables for the economy
  
  ## Output gap
  output_gap_Y_t <- Output(lambda_param, beta_param, output_gap_Y_t_minus_1, real_rate_t_minus_1, neutral_rate_t_minus_1, delta_param, G_t_minus_1_over_Y_star_t_minus_1)
  
  ## Inflation
  inflation_t <- Inflation(inflation_t_minus_1, alpha_param, output_gap_Y_t_minus_1)
  
  ## Price level
  P_t <- P_t_minus_1 * (1+inflation_t)
  
  ## Baseline policy
  ## A. Monetary policy
  
  ## The rate is set as
  nominal_rate_t <- MonetaryPolicy(inflation_t, inflation_target, a_param, b_param, output_gap_Y_t, neutral_rate_t_minus_1)
  
  
  if(nominal_rate_t > 0) { ## End of page 94
    ## Money demand determines M
    base_GDP_t <- BasePerGDP(P_t, nominal_rate_t, k_param, gamma_param)
    ## Lagged M determine open market purchases Z
    omo_GDP_t <- base_GDP_t - base_GDP_t_minus_1
    
  }
  else{ ## Case of i_t == 0
    base_GDP_t <- base_GDP_t_minus_1 / ((1.0+g_param))
    omo_GDP_t <- omo_GDP_t_minus_1
  }
  
  dept_t = Dept(dept_t_minus_1, nominal_rate_t, P_t, G_t_minus_1_over_Y_star_t_minus_1, omo_GDP_t_minus_1, theta_param, output_gap_Y_t) 
  
  ## Save results
  output_gap_percent_list[step_i] <- output_gap_Y_t
  inflation_percent_list[step_i] <- inflation_t
  nominal_interest_rate_list[step_i] <- nominal_rate_t
  omo_GDP_list[step_i] <- omo_GDP_t 
  base_GDP_list[step_i] <- base_GDP_t
  dept_GDP_list[step_i] <- dept_t
  P_t_list[step_i] <- P_t
  print("Iteration :")
  print(step_i)
}

print("End of simulation")

par(mfrow=c(3,2))
plot(output_gap_percent_list,type="l")
plot(inflation_percent_list,type="l")
plot(nominal_interest_rate_list,type="l")
plot(omo_GDP_list,type="l")
plot(base_GDP_list,type="l")
plot(dept_GDP_list,type="l")
