setwd("/Users/julianashwin/Documents/GitHub/dalys-model/julian_stuff")
rm(list=ls())

library(ggplot2)
library(ggpubr)
library(readxl)
library(dplyr)
library(stringr)
library(reshape2)
library(tidyr)

source("functions.R")


# Import data 
all_fertility_df <- read.csv("clean_data/fertility_rates.csv",stringsAsFactors = F)
all_population_df <- read.csv("clean_data/population.csv",stringsAsFactors = F)
all_mortality_df <- read.csv("clean_data/mortality_medium.csv",stringsAsFactors = F)
all_disability_df <- read.csv("clean_data/disability_medium.csv",stringsAsFactors = F)

locations <- unique(all_mortality_df$location)

# Starting point
loc_cols <- c("Global" = "black","World Bank High Income" = "forestgreen", 
              "World Bank Upper Middle Income" = "green", "World Bank Lower Middle Income" = "orange",
              "World Bank Low Income" = "red")
fert_plt <- ggplot(filter(all_fertility_df, location %in% locations & year == 2021)) + theme_bw() +
  geom_line(aes(x = age, y = fertility, color = location)) + 
  scale_color_manual(values = loc_cols) + labs(x = "Age", y = "Fertility", color = "Region")
pop_plt <- ggplot(filter(all_population_df, location %in% locations & year == 2021)) + theme_bw() +
  geom_line(aes(x = age, y = population, color = location)) + 
  scale_color_manual(values = loc_cols) + labs(x = "Age", y = "Population", color = "Region")
mort_plt <- ggplot(filter(all_mortality_df, location %in% locations)) + theme_bw() +
  geom_line(aes(x = age, y = mortality/100000, color = location)) + 
  scale_color_manual(values = loc_cols) + labs(x = "Age", y = "Mortality", color = "Region")
disab_plt <- ggplot(filter(all_disability_df, location %in% locations)) + theme_bw() +
  geom_line(aes(x = age, y = disability/100000, color = location)) + 
  scale_color_manual(values = loc_cols) + labs(x = "Age", y = "Disability", color = "Region")
ggarrange(fert_plt, pop_plt, mort_plt, disab_plt, common.legend = T, nrow =1, legend = "right")
ggsave("figures/rates_byregion.pdf", width = 12, height = 3)
# Choose options
start_year <- 2021
end_year <- 2122
end_age <- 100
no_births <- FALSE


loc_name <- locations[3]
# Select which region to focus on and which years to use as a starting point
fertility_df <- filter(all_fertility_df, location == loc_name, year == start_year)
population_df <- filter(all_population_df, location == loc_name)
mortality_df <- filter(all_mortality_df, location == loc_name)
disability_df <- filter(all_disability_df, location == loc_name)


## Slow_Aging
mortality_df <- slow_mortality(mortality_df, slow_by = 0.1)
disability_df <- slow_disability(disability_df, slow_by = 0.1)
fertility_df <- slow_fertility(fertility_df, slow_by = 0.1)
ggplot(mortality_df) + theme_bw() +
  geom_line(data=mortality_df, aes(x = age, y = mortality/100000, color = "Mortality")) +
  geom_line(data=mortality_df, aes(x = age, y = mortality_new/100000, color = "Mortality"), linetype = "dashed") +
  geom_line(data=disability_df, aes(x = age, y = disability/100000, color = "Disability")) +
  geom_line(data=disability_df, aes(x = age, y = disability_new/100000, color = "Disability"), linetype = "dashed") +
  geom_line(data=fertility_df, aes(x = age, y = fertility, color = "Fertility")) +
  geom_line(data=fertility_df, aes(x = age, y = fertility_new, color = "Fertility"), linetype = "dashed") + 
  labs(x = "Age", y = "Rates", color = "Variable")
ggsave("figures/slow_rates_10pc_example.pdf", width = 4, height = 2)
  


"
Forecasts under the baseline scenario
"
forecasts_base <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                                 start_year = start_year, end_year = end_year, end_age = end_age, 
                                 no_births = no_births)
plot_df <- forecasts_base %>%
  full_join(select(population_df, c(year, age, population))) %>%
  arrange(year, age) %>%
  mutate(forecast = case_when(year > start_year  ~ "Forecast",
                              year <= start_year  ~ "Estimate"))

ggplot(filter(plot_df, year%%10 == 0), aes(x = age)) + theme_bw() +
  scale_linetype_manual("", values = c("Estimate" = "solid", "Forecast" = "dotted")) + 
  geom_line(aes(y = population/1e6, group = as.character(year), color = year, linetype = forecast)) + 
  labs(color = "Year") + xlab("Age") + ylab("Population (millions)") + ggtitle("Baseline")
#ggsave("figures/lowincome_base_forecasts.pdf", width = 6, height = 4)

base_plt <- ggplot(filter(forecasts_base, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) + #ylim(c(0,215)) +
  labs(color = "Year") + xlab("Age") + ylab("Population (millions)") + ggtitle("Baseline")


"
Remove infant diseases
"
# Define the change we want to assess
mortality_df <- mortality_df %>% mutate(mortality_new = mortality - mortality_infant) %>%
  relocate(mortality_new, .after = mortality)
disability_df <- disability_df %>% mutate(disability_new = disability - disability_infant) %>%
  relocate(disability_new, .after = disability)
# Forecast population
forecasts_infant <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                                 start_year = start_year, end_year = end_year, end_age = end_age, 
                                 no_births = no_births)
infant_plt <- ggplot(filter(forecasts_infant, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) + 
  labs(color = "Year") + xlab("Age") + ylab("Population (millions)") + ggtitle("Infant diseases")
  
# Compare to benchmark
dalys_infant <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                               start_year = start_year, end_year = end_year, no_births = no_births)

"
Remove adult diseases
"
# Define the change we want to assess
mortality_df <- mortality_df %>% mutate(mortality_new = mortality - mortality_adult) %>%
  relocate(mortality_new, .after = mortality)
disability_df <- disability_df %>% mutate(disability_new = disability - disability_adult) %>%
  relocate(disability_new, .after = disability)
# Forecast population
forecasts_adult <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                                   start_year = start_year, end_year = end_year, end_age = end_age, 
                                   no_births = no_births)
adult_plt <- ggplot(filter(forecasts_adult, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) +
  labs(color = "Year") + xlab("Age") + ylab("") + ggtitle("Adult diseases")
# Compare to benchmark
dalys_adult <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                               start_year = start_year, end_year = end_year, no_births = no_births)

"
Remove senescent diseases
"
# Define the change we want to assess
mortality_df <- mortality_df %>% mutate(mortality_new = mortality - 1.*mortality_senescent) %>%
  relocate(mortality_new, .after = mortality)
disability_df <- disability_df %>% mutate(disability_new = disability - 1.*disability_senescent) %>%
  relocate(disability_new, .after = disability)
# Forecast population
forecasts_senescent <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                                  start_year = start_year, end_year = end_year, end_age = end_age, 
                                  no_births = no_births)
senescent_plt <- ggplot(filter(forecasts_senescent, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) + 
  labs(color = "Year") + xlab("Age") + ylab("") + ggtitle("Senescent diseases")
# Compare to benchmark
dalys_senescent <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                 start_year = start_year, end_year = end_year, no_births = no_births)

"
Slow aging 
"
# Define the change we want to assess
mortality_df <- slow_mortality(mortality_df, slow_by = 0.1)
disability_df <- slow_disability(disability_df, slow_by = 0.1)
# Forecast population
forecasts_slow <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                                      start_year = start_year, end_year = end_year, end_age = end_age, 
                                      no_births = no_births)
slow_plt <- ggplot(filter(forecasts_slow, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) + 
  labs(color = "Year") + xlab("Age") + ylab("") + ggtitle("Slow aging")
# Compare to benchmark
dalys_slow <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                     start_year = start_year, end_year = end_year, no_births = no_births)



"
Compare removing the different clusters
"
ggarrange(base_plt, infant_plt, adult_plt, senescent_plt, ncol = 4, 
          common.legend = TRUE, legend = "right", align = "hv")
ggsave("figures/compare_pop_forecasts.pdf", width = 12, height = 4)

ggplot() + theme_bw() +
  scale_color_manual("Scenario", values = c("Baseline" = "grey", "Senescent" = "firebrick1", 
                                   "Adult" = "blue3", "Infant" = "forestgreen", 
                                   "Slow Aging" = "darkgoldenrod3")) +
  geom_line(data = dalys_infant, aes(x = year, y = daly_base/1e9, color = "Baseline")) +
  geom_line(data = dalys_infant, aes(x = year, y = daly_new/1e9, color = "Infant")) +
  geom_line(data = dalys_adult, aes(x = year, y = daly_new/1e9, color = "Adult")) +
  geom_line(data = dalys_senescent, aes(x = year, y = daly_new/1e9, color = "Senescent")) +
  geom_line(data = dalys_slow, aes(x = year, y = daly_new/1e9, color = "Slow Aging")) +
  xlab("Year") + ylab("Disability-adjusted person years (billions)") + expand_limits(y=0)
ggsave("figures/compare_daly_forecasts.pdf", width = 6, height = 4)







"
Loop through each region
"
results_df <- data.frame()
for (loc_name in locations){
  # Select which region to focus on and which years to use as a starting point
  fertility_df <- filter(all_fertility_df, location == loc_name, year == start_year)
  population_df <- filter(all_population_df, location == loc_name)
  mortality_df <- filter(all_mortality_df, location == loc_name)
  disability_df <- filter(all_disability_df, location == loc_name)
  # Remove infant diseases
  mortality_df <- mortality_df %>% mutate(mortality_new = mortality - mortality_infant) %>%
    relocate(mortality_new, .after = mortality)
  disability_df <- disability_df %>% mutate(disability_new = disability - disability_infant) %>%
    relocate(disability_new, .after = disability)
  dalys_infant <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                    start_year = start_year, end_year = end_year, no_births = FALSE)
  dalys_infant <- dalys_infant %>% mutate(location = loc_name, intervention = "Infant")
  # Remove adult diseases
  mortality_df <- mortality_df %>% mutate(mortality_new = mortality - mortality_adult) %>%
    relocate(mortality_new, .after = mortality)
  disability_df <- disability_df %>% mutate(disability_new = disability - disability_adult) %>%
    relocate(disability_new, .after = disability)
  dalys_adult <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                   start_year = start_year, end_year = end_year, no_births = FALSE)
  dalys_adult <- dalys_adult %>% mutate(location = loc_name, intervention = "Adult")
  # Remove senescent diseases
  mortality_df <- mortality_df %>% mutate(mortality_new = mortality - 1.*mortality_senescent) %>%
    relocate(mortality_new, .after = mortality)
  disability_df <- disability_df %>% mutate(disability_new = disability - 1.*disability_senescent) %>%
    relocate(disability_new, .after = disability)
  dalys_senescent <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                       start_year = start_year, end_year = end_year, no_births = FALSE)
  dalys_senescent <- dalys_senescent %>% mutate(location = loc_name, intervention = "Senescent")
  
  # Slow aging by 10%
  mortality_df <- slow_mortality(mortality_df, slow_by = 0.1)
  disability_df <- slow_disability(disability_df, slow_by = 0.1)
  dalys_slow_10pc <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                  start_year = start_year, end_year = end_year, no_births = FALSE)
  dalys_slow_10pc <- dalys_slow_10pc %>% mutate(location = loc_name, intervention = "Slow_10pc")
  
  # Slow aging by 25%
  mortality_df <- slow_mortality(mortality_df, slow_by = 0.25)
  disability_df <- slow_disability(disability_df, slow_by = 0.25)
  dalys_slow_25pc <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                  start_year = start_year, end_year = end_year, no_births = FALSE)
  dalys_slow_25pc <- dalys_slow_25pc %>% mutate(location = loc_name, intervention = "Slow_25pc")
  
  # Append all results
  results_df <- rbind(results_df, dalys_infant, dalys_adult, dalys_senescent, dalys_slow_10pc, dalys_slow_25pc)
  
}



ggplot(results_df, aes(x = year)) + theme_bw() + facet_wrap(vars(location), scales = "free") + 
  scale_color_manual("Scenario", values = c("Baseline" = "grey", "Senescent" = "firebrick1", 
                                            "Adult" = "blue3", "Infant" = "forestgreen", 
                                            "Slow_10pc" = "darkgoldenrod3",
                                            "Slow_25pc" = "orange")) +
  geom_line(aes(x = year, y = daly_base/1e9, color = "Baseline")) +
  geom_line(aes(x = year, y = daly_new/1e9, color = intervention)) +
  xlab("Year") + ylab("Disability-adjusted person years (billions)") + expand_limits(y=0)
ggsave("figures/compare_daly_forecasts.pdf", width = 10, height = 6)



