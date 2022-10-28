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
all_mortality_df <- read.csv("clean_data/mortality_medium.csv",stringsAsFactors = F, check.names=F)
all_disability_df <- read.csv("clean_data/disability_medium.csv",stringsAsFactors = F, check.names=F)
cause_tree_df <- read.csv("clean_data/cause_definitions.csv", stringsAsFactors = F)
income_transition_df <- read.csv("clean_data/income_transition_shares.csv", stringsAsFactors = F)

locations <- unique(all_mortality_df$location)

# Starting point
loc_cols <- c("Global" = "black","World Bank High Income" = "forestgreen", 
              "World Bank Upper Middle Income" = "green", "World Bank Lower Middle Income" = "orange",
              "World Bank Low Income" = "red")
fert_plt <- ggplot(filter(all_fertility_df, location %in% locations & year >= 2021)) + theme_bw() +
  geom_line(aes(x = age, y = fertility_est, color = location, group = interaction(year, location))) + 
  geom_line(aes(x = age, y = fertility_med, color = location, group = interaction(year, location)), 
            linetype = "dashed", alpha = 0.2) + 
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
fertility_type <- "fertility_med"


loc_name <- locations[3]
# Select which region to focus on and which years to use as a starting point
fertility_df <- filter(all_fertility_df, location == loc_name, year >= start_year)
population_df <- filter(all_population_df, location == loc_name)
mortality_df <- filter(all_mortality_df, location == loc_name)
disability_df <- filter(all_disability_df, location == loc_name)


## Slow_Aging
mortality_df <- slow_mortality(mortality_df, slow_by = 0.1)
disability_df <- slow_disability(disability_df, slow_by = 0.1)
fertility_df <- slow_fertility(fertility_df, slow_by = 0.1, fertility_type = fertility_type)
ggplot(mortality_df) + theme_bw() +
  geom_line(data=mortality_df, aes(x = age, y = mortality/100000, color = "Mortality")) +
  geom_line(data=mortality_df, aes(x = age, y = mortality_new/100000, color = "Mortality"), linetype = "dashed") +
  geom_line(data=disability_df, aes(x = age, y = disability/100000, color = "Disability")) +
  geom_line(data=disability_df, aes(x = age, y = disability_new/100000, color = "Disability"), linetype = "dashed") +
  geom_line(data=fertility_df, aes(x = age, y = fertility, group = year, color = "Fertility", alpha = year)) +
  geom_line(data=fertility_df, aes(x = age, y = fertility_new, group = year, color = "Fertility", alpha = year), linetype = "dashed") + 
  labs(x = "Age", y = "Rates", color = "Variable")
ggsave("figures/slow_rates_10pc_example.pdf", width = 4, height = 2)
  


"
Forecasts under the baseline scenario
"
forecasts_base <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                 start_year = start_year, end_year = end_year, end_age = end_age, 
                                 no_births = no_births, fertility_type = fertility_type)
plot_df <- forecasts_base %>%
  full_join(select(population_df, c(year, age, population))) %>% arrange(year, age) %>%
  mutate(forecast = case_when(year > start_year  ~ "Forecast", year <= start_year  ~ "Estimate"))

ggplot(filter(plot_df, year%%10 == 0), aes(x = age)) + theme_bw() +
  scale_linetype_manual("", values = c("Estimate" = "solid", "Forecast" = "dotted")) + 
  geom_line(aes(y = population/1e6, group = as.character(year), color = year, linetype = forecast)) + 
  labs(color = "Year") + xlab("Age") + ylab("Population (millions)") + ggtitle("Baseline")
#ggsave("figures/lowincome_base_forecasts.pdf", width = 6, height = 4)

base_plt <- ggplot(filter(forecasts_base, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) + #ylim(c(0,215)) +
  labs(color = "Year") + xlab("Age") + ylab("Population (millions)") + ggtitle("Baseline")


"
Remove infant diseases (cluster3)
"
infant3_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster3 == "Infant")] 
# Define the change we want to assess
mortality_df$mortality_infant3 <- rowSums(mortality_df[,which(names(mortality_df) %in% infant3_diseases)])
mortality_df$mortality_new <- mortality_df$mortality - mortality_df$mortality_infant3
disability_df$disability_infant3 <- rowSums(disability_df[,which(names(disability_df) %in% infant3_diseases)])
disability_df$disability_new <- disability_df$disability - disability_df$disability_infant3
# Forecast population
forecasts_infant <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "both",
                                 start_year = start_year, end_year = end_year, end_age = end_age, 
                                 no_births = no_births, fertility_type = fertility_type)
infant3_plt <- ggplot(filter(forecasts_infant, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) + 
  labs(color = "Year") + xlab("Age") + ylab("Population (millions)") + ggtitle("Infant diseases")
  
# Compare to benchmark
dalys_infant3 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                               start_year = start_year, end_year = end_year, no_births = no_births, 
                               fertility_type = fertility_type)
decomp_infant3 <- ggplot(dalys_infant3, aes(x = year)) + theme_bw() + 
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Infant", fill = "")


"
Remove infant diseases (cluster4)
"
infant4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Infant")] 
# Define the change we want to assess
mortality_df$mortality_infant4 <- rowSums(mortality_df[,which(names(mortality_df) %in% infant4_diseases)])
mortality_df$mortality_new <- mortality_df$mortality - mortality_df$mortality_infant3
disability_df$disability_infant4 <- rowSums(disability_df[,which(names(disability_df) %in% infant4_diseases)])
disability_df$disability_new <- disability_df$disability - disability_df$disability_infant3
# Forecast population
forecasts_infant <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "both",
                                   start_year = start_year, end_year = end_year, end_age = end_age, 
                                   no_births = no_births, fertility_type = fertility_type)
infant4_plt <- ggplot(filter(forecasts_infant, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) + 
  labs(color = "Year") + xlab("Age") + ylab("Population (millions)") + ggtitle("Infant diseases")

# Compare to benchmark
dalys_infant4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                  start_year = start_year, end_year = end_year, no_births = no_births, 
                                  fertility_type = fertility_type)
decomp_infant4 <- ggplot(dalys_infant4, aes(x = year)) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Infant", fill = "")



"
Remove adult diseases (cluster3)
"
adult3_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster3 == "Adult")] 
# Define the change we want to assess
mortality_df$mortality_adult3 <- rowSums(mortality_df[,which(names(mortality_df) %in% adult3_diseases)])
mortality_df$mortality_new <- mortality_df$mortality - mortality_df$mortality_adult3
disability_df$disability_adult3 <- rowSums(disability_df[,which(names(disability_df) %in% adult3_diseases)])
disability_df$disability_new <- disability_df$disability - disability_df$disability_adult3
# Forecast population
forecasts_adult <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "both",
                                   start_year = start_year, end_year = end_year, end_age = end_age, 
                                   no_births = no_births, fertility_type = fertility_type) 
adult3_plt <- ggplot(filter(forecasts_adult, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) +
  labs(color = "Year") + xlab("Age") + ylab("") + ggtitle("Adult diseases")
# Compare to benchmark
dalys_adult3 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                               start_year = start_year, end_year = end_year, no_births = no_births,
                               fertility_type = fertility_type)
decomp_adult3 <- ggplot(dalys_adult3, aes(x = year)) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Adult", fill = "")


"
Remove early adult diseases (cluster4)
"
adult_early4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_early")] 
# Define the change we want to assess
mortality_df$mortality_adult_early4 <- rowSums(mortality_df[,which(names(mortality_df) %in% adult_early4_diseases)])
mortality_df$mortality_new <- mortality_df$mortality - mortality_df$mortality_adult_early4
disability_df$disability_adult_early4 <- rowSums(disability_df[,which(names(disability_df) %in% adult_early4_diseases)])
disability_df$disability_new <- disability_df$disability - disability_df$disability_adult_early4
# Forecast population
forecasts_adult <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "both",
                                  start_year = start_year, end_year = end_year, end_age = end_age, 
                                  no_births = no_births, fertility_type = fertility_type) 
adult_early4_plt <- ggplot(filter(forecasts_adult, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) +
  labs(color = "Year") + xlab("Age") + ylab("") + ggtitle("Adult (early) diseases")
# Compare to benchmark
dalys_adult_early4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                  start_year = start_year, end_year = end_year, no_births = no_births,
                                  fertility_type = fertility_type)
decomp_adult_early4 <- ggplot(dalys_adult_early4, aes(x = year)) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Adult (early)", fill = "")



"
Remove late adult diseases (cluster4)
"
adult_late4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_late")] 
# Define the change we want to assess
mortality_df$mortality_adult_late4 <- rowSums(mortality_df[,which(names(mortality_df) %in% adult_late4_diseases)])
mortality_df$mortality_new <- mortality_df$mortality - mortality_df$mortality_adult_late4
disability_df$disability_adult_late4 <- rowSums(disability_df[,which(names(disability_df) %in% adult_late4_diseases)])
disability_df$disability_new <- disability_df$disability - disability_df$disability_adult_late4
# Forecast population
forecasts_adult <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "both",
                                  start_year = start_year, end_year = end_year, end_age = end_age, 
                                  no_births = no_births, fertility_type = fertility_type) 
adult_late4_plt <- ggplot(filter(forecasts_adult, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) +
  labs(color = "Year") + xlab("Age") + ylab("") + ggtitle("Adult (late) diseases")
# Compare to benchmark
dalys_adult_late4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                        start_year = start_year, end_year = end_year, no_births = no_births,
                                        fertility_type = fertility_type)
decomp_adult_late4 <- ggplot(dalys_adult_late4, aes(x = year)) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Adult (late)", fill = "")


"
Remove senescent diseases (cluster3)
"
senescent3_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster3 == "Senescent")] 
# Define the change we want to assess
mortality_df$mortality_senescent3 <- rowSums(mortality_df[,which(names(mortality_df) %in% senescent3_diseases)])
mortality_df$mortality_new <- mortality_df$mortality - mortality_df$mortality_senescent3
disability_df$disability_senescent3 <- rowSums(disability_df[,which(names(disability_df) %in% senescent3_diseases)])
disability_df$disability_new <- disability_df$disability - disability_df$disability_senescent3
# Forecast population
forecasts_senescent <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "both",
                                  start_year = start_year, end_year = end_year, end_age = end_age, 
                                  no_births = no_births, fertility_type = fertility_type)
senescent3_plt <- ggplot(filter(forecasts_senescent, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) + 
  labs(color = "Year") + xlab("Age") + ylab("") + ggtitle("Senescent diseases")
# Compare to benchmark
dalys_senescent3 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                 start_year = start_year, end_year = end_year, no_births = no_births,
                                 fertility_type = fertility_type)
decomp_senescent3 <- ggplot(dalys_senescent3, aes(x = year)) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Senescent", fill = "")


"
Remove senescent diseases (cluster4)
"
senescent4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Senescent")] 
# Define the change we want to assess
mortality_df$mortality_senescent4 <- rowSums(mortality_df[,which(names(mortality_df) %in% senescent4_diseases)])
mortality_df$mortality_new <- mortality_df$mortality - mortality_df$mortality_senescent4
disability_df$disability_senescent4 <- rowSums(disability_df[,which(names(disability_df) %in% senescent4_diseases)])
disability_df$disability_new <- disability_df$disability - disability_df$disability_senescent4
# Forecast population
forecasts_senescent <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "both",
                                      start_year = start_year, end_year = end_year, end_age = end_age, 
                                      no_births = no_births, fertility_type = fertility_type)
senescent4_plt <- ggplot(filter(forecasts_senescent, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) + 
  labs(color = "Year") + xlab("Age") + ylab("") + ggtitle("Senescent diseases")
# Compare to benchmark
dalys_senescent4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                      start_year = start_year, end_year = end_year, no_births = no_births,
                                      fertility_type = fertility_type)
decomp_senescent4 <- ggplot(dalys_senescent3, aes(x = year)) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Senescent", fill = "")


"
Slow aging 
"
# Define the change we want to assess
mortality_df <- slow_mortality(mortality_df, slow_by = 0.2)
disability_df <- slow_disability(disability_df, slow_by = 0.2)
# Forecast population
forecasts_slow <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "both",
                                      start_year = start_year, end_year = end_year, end_age = end_age, 
                                      no_births = no_births, fertility_type = fertility_type)
slow_plt <- ggplot(filter(forecasts_slow, year%%10 == 0), aes(x = age)) + theme_bw() +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) + 
  labs(color = "Year") + xlab("Age") + ylab("") + ggtitle("Slow aging")
# Compare to benchmark
dalys_slow <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                start_year = start_year, end_year = end_year, no_births = no_births,
                                fertility_type = fertility_type)
decomp_slow <- ggplot(dalys_slow, aes(x = year)) + 
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Slow aging", fill = "")



"
Compare removing the different clusters
"
# Population forecasts
ggarrange(base_plt, infant3_plt, adult3_plt, senescent3_plt, nrow = 1, 
          common.legend = TRUE, legend = "right", align = "hv")
ggsave("figures/compare_pop_forecasts_cluster3.pdf", width = 12, height = 4)
ggarrange(base_plt, infant4_plt, adult_early4_plt, adult_late4_plt, senescent4_plt, nrow = 1, 
          common.legend = TRUE, legend = "right", align = "hv")
ggsave("figures/compare_pop_forecasts_cluster4.pdf", width = 15, height = 4)
# Decomposition of gains
ggarrange(decomp_infant3, decomp_adult3+ylab(""), decomp_senescent3+ylab(""), decomp_slow+ylab(""), nrow = 1, 
          common.legend = TRUE, legend = "right", align = "hv")
ggsave("figures/decomp_daly_cluster3.pdf", width = 12, height = 4)
ggarrange(decomp_infant4, decomp_adult_early4+ylab(""), decomp_adult_late4+ylab(""), decomp_senescent4+ylab(""), nrow = 1, 
          common.legend = TRUE, legend = "right", align = "hv")
ggsave("figures/decomp_daly_cluster4.pdf", width = 12, height = 4)

ggplot() + theme_bw() +
  scale_color_manual("Scenario", values = c("Baseline" = "grey", "Senescent" = "firebrick1", 
                                   "Adult" = "blue3", "Infant" = "forestgreen", 
                                   "Slow Aging" = "darkgoldenrod3")) +
  geom_line(data = dalys_infant3, aes(x = year, y = daly_base/1e9, color = "Baseline")) +
  geom_line(data = dalys_infant3, aes(x = year, y = daly_new/1e9, color = "Infant")) +
  geom_line(data = dalys_adult3, aes(x = year, y = daly_new/1e9, color = "Adult")) +
  geom_line(data = dalys_senescent3, aes(x = year, y = daly_new/1e9, color = "Senescent")) +
  geom_line(data = dalys_slow, aes(x = year, y = daly_new/1e9, color = "Slow Aging")) +
  xlab("Year") + ylab("Disability-adjusted person years (billions)") + expand_limits(y=0)
ggsave("figures/compare_daly_forecasts_cluster3.pdf", width = 6, height = 4)

ggplot() + theme_bw() +
  scale_color_manual("Scenario", values =c("Baseline" = "grey", "Senescent" = "firebrick1", "Adult_late" = "blue3", 
                                           "Adult_early" = "cornflowerblue", "Infant" = "forestgreen",
                                           "Slow Aging" = "darkgoldenrod3")) +
  geom_line(data = dalys_infant4, aes(x = year, y = daly_base/1e9, color = "Baseline")) +
  geom_line(data = dalys_infant4, aes(x = year, y = daly_new/1e9, color = "Infant")) +
  geom_line(data = dalys_adult_early4, aes(x = year, y = daly_new/1e9, color = "Adult_early")) +
  geom_line(data = dalys_adult_late4, aes(x = year, y = daly_new/1e9, color = "Adult_late")) +
  geom_line(data = dalys_senescent4, aes(x = year, y = daly_new/1e9, color = "Senescent")) +
  geom_line(data = dalys_slow, aes(x = year, y = daly_new/1e9, color = "Slow Aging")) +
  xlab("Year") + ylab("Disability-adjusted person years (billions)") + expand_limits(y=0)
ggsave("figures/compare_daly_forecasts_cluster4.pdf", width = 6, height = 4)








"
Loop through each region
"
# Make sure the disease clusters are defined
infant3_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster3 == "Infant")] 
adult3_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster3 == "Adult")] 
senescent3_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster3 == "Senescent")] 
infant4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Infant")] 
adult_early4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_early")] 
adult_late4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_late")] 
senescent4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Senescent")] 
# Control parameters here
locations <- unique(all_mortality_df$location)
fertility_types <- c("fertility_est", "fertility_low", "fertility_med", "fertility_high")
start_year <- 2021
end_year <- 2122
end_age <- 100
no_births <- FALSE
fertility_type <- "fertility_med"

results_df <- data.frame()
for (loc_name in locations){
  print(paste0("Analysis for ", loc_name))
  for (fertility_type in fertility_types){
    print(paste0("Fertility assumption: ", fertility_type))
    # Select which region to focus on and which years to use as a starting point
    fertility_df <- filter(all_fertility_df, location == loc_name, year >= start_year)
    population_df <- filter(all_population_df, location == loc_name)
    mortality_df <- filter(all_mortality_df, location == loc_name)
    disability_df <- filter(all_disability_df, location == loc_name)
    # Remove infant diseases
    mortality_df <- def_mortality_new(mortality_df, infant3_diseases)
    disability_df <- def_disability_new(disability_df, infant3_diseases)
    dalys_infant3 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                       start_year = start_year, end_year = end_year, no_births = no_births,
                                       fertility_type = fertility_type) %>%
      mutate(location = loc_name, intervention = "Infant", fertility_type = fertility_type, clusters = 3)
    mortality_df <- def_mortality_new(mortality_df, infant4_diseases)
    disability_df <- def_disability_new(disability_df, infant4_diseases)
    dalys_infant4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                       start_year = start_year, end_year = end_year, no_births = no_births,
                                       fertility_type = fertility_type) %>%
      mutate(location = loc_name, intervention = "Infant", fertility_type = fertility_type, clusters = 4)
    # Remove adult diseases
    mortality_df <- def_mortality_new(mortality_df, adult3_diseases)
    disability_df <- def_disability_new(disability_df, adult3_diseases)
    dalys_adult3 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                       start_year = start_year, end_year = end_year, no_births = no_births,
                                       fertility_type = fertility_type) %>%
      mutate(location = loc_name, intervention = "Adult", fertility_type = fertility_type, clusters = 3)
    mortality_df <- def_mortality_new(mortality_df, adult_early4_diseases)
    disability_df <- def_disability_new(disability_df, adult_early4_diseases)
    dalys_adult_early4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                       start_year = start_year, end_year = end_year, no_births = no_births,
                                       fertility_type = fertility_type) %>%
      mutate(location = loc_name, intervention = "Adult_early", fertility_type = fertility_type, clusters = 4)
    mortality_df <- def_mortality_new(mortality_df, adult_late4_diseases)
    disability_df <- def_disability_new(disability_df, adult_late4_diseases)
    dalys_adult_late4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                            start_year = start_year, end_year = end_year, no_births = no_births,
                                            fertility_type = fertility_type) %>%
      mutate(location = loc_name, intervention = "Adult_late", fertility_type = fertility_type, clusters = 4)
    # Remove senescent diseases
    mortality_df <- def_mortality_new(mortality_df, senescent3_diseases)
    disability_df <- def_disability_new(disability_df, senescent3_diseases)
    dalys_senescent3 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                       start_year = start_year, end_year = end_year, no_births = no_births,
                                       fertility_type = fertility_type) %>%
      mutate(location = loc_name, intervention = "Senescent", fertility_type = fertility_type, clusters = 3)
    mortality_df <- def_mortality_new(mortality_df, senescent4_diseases)
    disability_df <- def_disability_new(disability_df, senescent4_diseases)
    dalys_senescent4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                       start_year = start_year, end_year = end_year, no_births = no_births,
                                       fertility_type = fertility_type) %>%
      mutate(location = loc_name, intervention = "Senescent", fertility_type = fertility_type, clusters = 4)
    # Slow aging by 10%
    mortality_df <- slow_mortality(mortality_df, slow_by = 0.1)
    disability_df <- slow_disability(disability_df, slow_by = 0.1)
    dalys_slow_10pc <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                         start_year = start_year, end_year = end_year, no_births = no_births,
                                         fertility_type = fertility_type) %>% 
      mutate(location = loc_name, intervention = "Slow_10pc", fertility_type = fertility_type, clusters = 3)
    # Slow aging by 25%
    mortality_df <- slow_mortality(mortality_df, slow_by = 0.25)
    disability_df <- slow_disability(disability_df, slow_by = 0.25)
    dalys_slow_25pc <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                         start_year = start_year, end_year = end_year, no_births = no_births,
                                         fertility_type = fertility_type) %>% 
      mutate(location = loc_name, intervention = "Slow_25pc", fertility_type = fertility_type, clusters = 3)
    
    # Append all results
    results_df <- rbind(results_df, dalys_infant3, dalys_adult3, dalys_senescent3, 
                        dalys_infant4, dalys_adult_early4, dalys_adult_late4, dalys_senescent4,
                        dalys_slow_10pc, dalys_slow_25pc)
  }
}
results_df <- relocate(results_df, location,intervention, fertility_type, clusters, .before = year)

baseline_results <- filter(results_df, clusters == 3 & fertility_type == "fertility_med" &
                             intervention != "Slow_10pc")

ggplot(baseline_results, aes(x = year)) + theme_bw() + facet_wrap(.~location) +
  #geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity", group = intervention), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Slow aging", fill = "")




ggplot(filter(results_df, location != "Global")) + theme_bw() +
  facet_wrap(.~fertility_type) +
  geom_bar(aes(x = intervention, y = daly_diff, fill = location), stat = "summary", fun = "sum") +
  geom_line(data = filter(results_df, location == "Global"),
            aes(x = intervention, y = daly_diff, color = location, group = location), stat = "summary", fun = "sum") +
  scale_fill_manual("Region", values = loc_cols) + 
  scale_color_manual("Region", values = loc_cols) + guides(color = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot(results_df) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_point(aes(x = intervention, y = daly_diff - daly_diff_dis -daly_diff_mort, color = clusters))
table(select(results_df, c(LE_new, intervention)))


ggplot(filter(results_df, fertility_type == "fertility_med" & clusters == 3), aes(x = year)) + theme_bw() + 
  facet_wrap(.~location, scales = "free") + 
  geom_line(aes(x = year, y = daly_base/1e9, color = "Baseline")) +
  geom_line(aes(x = year, y = daly_new/1e9, color = intervention)) +
  scale_color_manual("Scenario", values = c("Baseline" = "grey", "Senescent" = "firebrick1", 
                                            "Adult" = "blue3", "Adult_late" = "blue3", "Adult_early" = "cornflowerblue",
                                            "Infant" = "forestgreen", "Slow_10pc" = "darkgoldenrod3", "Slow_25pc" = "orange")) +
  xlab("Year") + ylab("Disability-adjusted person years (billions)") + expand_limits(y=0)
ggsave("figures/compare_daly_forecasts.pdf", width = 10, height = 6)



