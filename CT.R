rm(list=ls())
library(haven)
directory <- ("C:\\Users\\lopo\\OneDrive\\桌面\\course\\causal inference\\exercise")
data <- read_dta(paste0(directory,"\\carbontax_fullsample_data.dta"))

table(data$"country")

data <-data[!(data$"country"=="Poland"|data$"country"=="Norway"|data$"country"=="Sweden"|
                data$"country"=="Denmark"|data$"country"=="Slovenia"|data$"country"=="Estonia"|data$"country"=="Latvia"),]

data <- data[data$year >= 1970 & data$year <= 2005, ]

pacman::p_load(tidysynth)
library(tidysynth)

# Filter the data to include only the rows between 1970 and 2005

Finsynth <- 
  
  data %>%
  
  #intializing the synthetic control object
  synthetic_control(outcome = CO2_transport_capita, # outcome
                    unit = country, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "Finland", # unit where the intervention occurred
                    i_time = 1990, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  
  # average log income, retail price of cigarettes, and proportion of the
  # population between 15 and 24 years of age from 1980 - 1988
  generate_predictor(time_window = 1970:1989,
                     GDP_per_capita = mean(GDP_per_capita, na.rm = T),
                     Motor_vehicles = mean(vehicles_capita, na.rm = T),
                     Gasoline = mean(gas_cons_capita, na.rm = T),
                     Urbanpop = mean(urban_pop, na.rm=T)) %>%
  
  # Lagged cigarette sales 
  generate_predictor(time_window = 1989,
                     co2_1989 = CO2_transport_capita) %>%
  generate_predictor(time_window = 1980,
                     co2_1980 = CO2_transport_capita) %>%
  generate_predictor(time_window = 1975,
                     co2_1975 = CO2_transport_capita) %>%
  generate_predictor(time_window = 1970,
                     co2_1970 = CO2_transport_capita) %>%
  
  
  
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1970:1989 # time to use in the optimization task
  ) %>%
  
  # Generate the synthetic control
  generate_control()

plot_trends(Finsynth)

plot_differences(Finsynth)

grab_balance_table(Finsynth)

plot_weights(Finsynth)

plot_placebos(Finsynth, prune=FALSE)
# prunning
plot_placebos(Finsynth)

plot_mspe_ratio(Finsynth)

#-------------------------
# exclude the Ireland, Netherlands, Luxembourg, Austria, Germany, the United Kingdom, and Italy

data2 <-data[!(data$"country"=="Poland"|data$"country"=="Norway"|data$"country"=="Sweden"|
                data$"country"=="Ireland"|data$"country"=="Netherlands"|data$"country"=="Luxembourg"|
                data$"country"=="Austria"|data$"country"=="Germany"|data$"country"=="United Kingdom"|
                data$"country"=="Italy"|
                data$"country"=="Denmark"|data$"country"=="Slovenia"|data$"country"=="Estonia"|data$"country"=="Latvia"),]

table(data2$"country")

data2 <- data2[data2$year >= 1970 & data2$year <= 2005, ]


# Filter the data to include only the rows between 1970 and 2005

Finsynth <- 
  
  data2 %>%
  
  #intializing the synthetic control object
  synthetic_control(outcome = CO2_transport_capita, # outcome
                    unit = country, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "Finland", # unit where the intervention occurred
                    i_time = 1990, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  
  # average log income, retail price of cigarettes, and proportion of the
  # population between 15 and 24 years of age from 1980 - 1988
  generate_predictor(time_window = 1970:1989,
                     GDP_per_capita = mean(GDP_per_capita, na.rm = T),
                     Motor_vehicles = mean(vehicles_capita, na.rm = T),
                     Gasoline = mean(gas_cons_capita, na.rm = T),
                     Urbanpop = mean(urban_pop, na.rm=T)) %>%
  
  # Lagged cigarette sales 
  generate_predictor(time_window = 1989,
                     co2_1989 = CO2_transport_capita) %>%
  generate_predictor(time_window = 1980,
                     co2_1980 = CO2_transport_capita) %>%
  generate_predictor(time_window = 1975,
                     co2_1975 = CO2_transport_capita) %>%
  generate_predictor(time_window = 1970,
                     co2_1970 = CO2_transport_capita) %>%
  
  
  
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1970:1989 # time to use in the optimization task
  ) %>%
  
  # Generate the synthetic control
  generate_control()

plot_trends(Finsynth)

plot_differences(Finsynth)

grab_balance_table(Finsynth)

plot_weights(Finsynth)

plot_placebos(Finsynth, prune=FALSE)
# prunning
plot_placebos(Finsynth)

plot_mspe_ratio(Finsynth)
