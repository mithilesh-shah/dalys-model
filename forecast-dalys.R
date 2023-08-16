setwd("/Users/julianashwin/Documents/GitHub/dalys-model/")
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
all_fertility_df <- read_csv("clean_data/_archive/fertility_rates.csv")
all_population_df <- read_csv("clean_data/_archive/population.csv")
all_mortality_df <- read_csv("clean_data/_archive/mortality_medium.csv")
all_disability_df <- read_csv("clean_data/_archive/disability_medium.csv")
cause_tree_df <- read_csv("clean_data/_archive/cause_definitions.csv")
income_transition_df <- read_csv("clean_data/_archive/income_transition_shares.csv")

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
pop_plt <- ggplot(filter(all_population_df, location %in% locations & year <= 2021 & year >= 1990)) + theme_bw() +
  geom_line(aes(x = age, y = population, color = location, group = interaction(year, location))) + 
  scale_color_manual(values = loc_cols) + labs(x = "Age", y = "Population", color = "Region")
mort_plt <- all_mortality_df %>%
  filter(location %in% locations & year == 2019) %>%
  ggplot() + theme_bw() +
  geom_line(aes(x = age, y = mortality/100000, color = location)) + 
  scale_color_manual(values = loc_cols) + labs(x = "Age", y = "Mortality", color = "Region")
disab_plt <- all_disability_df %>%
  filter(location %in% locations & year == 2019) %>%
  ggplot() + theme_bw() +
  geom_line(aes(x = age, y = disability/100000, color = location)) + 
  scale_color_manual(values = loc_cols) + labs(x = "Age", y = "Disability", color = "Region")
ggarrange(fert_plt, pop_plt, mort_plt, disab_plt, common.legend = T, nrow =1, legend = "right")
#ggsave("figures/rates_byregion.pdf", width = 12, height = 3)
# Choose options
start_year <- 2021
end_year <- 2122
end_age <- 100
no_births <- FALSE
fertility_type <- "fertility_med"
growth_transitions <- TRUE
new_vars <- "none"

loc_name <- "Regions"
# Select which region to focus on and which years to use as a starting point
if (loc_name == "Regions"){
  population_df <- filter(all_population_df, location %in% c("World Bank High Income", "World Bank Low Income", 
                                                             "World Bank Lower Middle Income", "World Bank Upper Middle Income"))
  fertility_df <- filter(all_fertility_df, year >= start_year,
                         location %in% c("World Bank High Income", "World Bank Low Income", 
                                         "World Bank Lower Middle Income", "World Bank Upper Middle Income"))
} else {
  population_df <- filter(all_population_df, location == loc_name)
  fertility_df <- filter(all_fertility_df, location == loc_name, year >= start_year)
}

# Create mortality data, with growth transitions
mortality_df <- create_mortality_df(all_mortality_df, loc_name = loc_name, start_year = start_year, end_year = end_year, 
                                    end_age = end_age, growth_transitions = TRUE, 
                                    income_transition_df = income_transition_df)
mortality_df1 %>%
  ggplot() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = mortality, group = year, color = year))
# Create disability data, with growth transitions
disability_df <- create_disability_df(all_disability_df, loc_name = loc_name, start_year = start_year, end_year = end_year, 
                                    end_age = end_age, growth_transitions = TRUE, 
                                    income_transition_df = income_transition_df)
disability_df %>%
  ggplot() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = disability, group = year, color = year))



"
Growth vs no growth
"
forecasts_nogrowth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                     start_year = start_year, end_year = end_year, end_age = end_age, 
                                     no_births = no_births, fertility_type = "fertility_est", loc_name = loc_name,
                                     growth_transitions = FALSE) %>% mutate(growth = "No Growth", fertility = "2021")
forecasts_growth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                   start_year = start_year, end_year = end_year, end_age = end_age, 
                                   no_births = no_births, fertility_type = "fertility_est", loc_name = loc_name, 
                                   growth_transitions = TRUE) %>% mutate(growth = "Growth", fertility = "2021")
forecasts_both <- rbind(forecasts_nogrowth, forecasts_growth)
forecasts_nogrowth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                     start_year = start_year, end_year = end_year, end_age = end_age, 
                                     no_births = no_births, fertility_type = "fertility_med", loc_name = loc_name,
                                     growth_transitions = FALSE) %>% mutate(growth = "No Growth", fertility = "Medium")
forecasts_growth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                   start_year = start_year, end_year = end_year, end_age = end_age, 
                                   no_births = no_births, fertility_type = "fertility_med", loc_name = loc_name, 
                                   growth_transitions = TRUE) %>% mutate(growth = "Growth", fertility = "Medium")
forecasts_both <- rbind(forecasts_both, forecasts_nogrowth, forecasts_growth)

ggplot(forecasts_both) + theme_bw() + facet_wrap(vars(location, growth, fertility), scales = "free") + 
  geom_line(aes(x = age, y = population, color = year, group = year)) + 
  labs(x = "Age", y = "Population", color = "Year", title = loc_name)
#ggsave(paste0("figures/growth_nogrowth_pop_",loc_name,".pdf"), width = 6, height = 5)


"
Forecasts under the baseline scenario
"
forecasts_base <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                 start_year = start_year, end_year = end_year, end_age = end_age, 
                                 no_births = no_births, fertility_type = fertility_type, loc_name = loc_name, 
                                 growth_transitions = growth_transitions)
plot_df <- forecasts_base %>%
  full_join(select(population_df, c(location, year, age, population))) %>% arrange(year, age) %>%
  mutate(forecast = case_when(year > start_year  ~ "Forecast", year <= start_year  ~ "Estimate"))

ggplot(filter(plot_df, year%%10 == 0), aes(x = age)) + theme_bw() + facet_wrap(~location)+
  scale_linetype_manual("", values = c("Estimate" = "solid", "Forecast" = "dotted")) + 
  geom_line(aes(y = population/1e6, group = as.character(year), color = year, linetype = forecast)) + 
  labs(color = "Year") + xlab("Age") + ylab("Population (millions)") + ggtitle("Baseline")


"
Remove infant diseases (cluster4)
"
infant4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Infant")] 
## Define the change we want to assess
# Mortality 
mortality_df$mortality_infant4 <- rowSums(mortality_df[,which(names(mortality_df) %in% infant4_diseases)])
mortality_df$mortality_new <- mortality_df$mortality - mortality_df$mortality_infant4
mortality_df %>%
  ggplot() + theme_bw() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(mortality)/1e5, group = year, color = year, linetype = "Old")) + 
  geom_line(aes(x = age, y = log(mortality_new)/1e5, group = year, color = year, linetype = "New"))
# Disability
disability_df$disability_infant4 <- rowSums(disability_df[,which(names(disability_df) %in% infant4_diseases)])
disability_df$disability_new <- disability_df$disability - disability_df$disability_infant4
disability_df %>%
  ggplot() + theme_bw() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(disability)/1e5, group = year, color = year, linetype = "Old")) + 
  geom_line(aes(x = age, y = log(disability_new)/1e5, group = year, color = year, linetype = "New"))
# Forecast population
forecasts_infant <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "both",
                                   start_year = start_year, end_year = end_year, end_age = end_age, 
                                   no_births = no_births, fertility_type = fertility_type, loc_name = loc_name, 
                                   growth_transitions = growth_transitions)
forecasts_base %>%
  filter(year%%10 == 0) %>% 
  ggplot(aes(x = age)) + theme_bw() + facet_wrap(~location) +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) + 
  labs(color = "Year") + xlab("Age") + ylab("Population (millions)") + ggtitle("Infant")

# Compare to benchmark
dalys_infant4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, loc_name = loc_name, 
                                  start_year = start_year, end_year = end_year, no_births = no_births, 
                                  fertility_type = fertility_type, growth_transitions = growth_transitions)
dalys_infant4 %>%
  ggplot(aes(x = year)) + theme_bw() + facet_wrap(~location) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Infant", fill = "")




"
Remove early adult diseases (cluster4)
"
adult_early4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_early")] 
## Define the change we want to assess
# Mortality
mortality_df$mortality_adult_early4 <- rowSums(mortality_df[,which(names(mortality_df) %in% adult_early4_diseases)])
mortality_df$mortality_new <- mortality_df$mortality - mortality_df$mortality_adult_early4
mortality_df %>%
  ggplot() + theme_bw() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(mortality)/1e5, group = year, color = year, linetype = "Old")) + 
  geom_line(aes(x = age, y = log(mortality_new)/1e5, group = year, color = year, linetype = "New"))
# Disability
disability_df$disability_adult_early4 <- rowSums(disability_df[,which(names(disability_df) %in% adult_early4_diseases)])
disability_df$disability_new <- disability_df$disability - disability_df$disability_adult_early4
disability_df %>%
  ggplot() + theme_bw() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(disability)/1e5, group = year, color = year, linetype = "Old")) + 
  geom_line(aes(x = age, y = log(disability_new)/1e5, group = year, color = year, linetype = "New"))
# Forecast population
forecasts_adult_early <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "both",
                                  start_year = start_year, end_year = end_year, end_age = end_age, 
                                  no_births = no_births, fertility_type = fertility_type, loc_name = loc_name, 
                                  growth_transitions = growth_transitions) 
forecasts_adult_early %>%
  filter(year%%10 == 0) %>%
  ggplot(aes(x = age)) + theme_bw() + facet_wrap(~location) +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) +
  labs(color = "Year") + xlab("Age") + ylab("") + ggtitle("Adult (early)")

# Compare to benchmark
dalys_adult_early4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, loc_name = loc_name, 
                                  start_year = start_year, end_year = end_year, no_births = no_births,
                                  fertility_type = fertility_type, growth_transitions = growth_transitions)

dalys_adult_early4 %>%
  ggplot(aes(x = year)) + theme_bw() + facet_wrap(~location) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Adult (early)", fill = "")




"
Remove late adult diseases (cluster4)
"
adult_late4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_late")] 
## Define the change we want to assess
# Mortality
mortality_df$mortality_adult_late4 <- rowSums(mortality_df[,which(names(mortality_df) %in% adult_late4_diseases)])
mortality_df$mortality_new <- mortality_df$mortality - mortality_df$mortality_adult_late4
mortality_df %>%
  ggplot() + theme_bw() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(mortality)/1e5, group = year, color = year, linetype = "Old")) + 
  geom_line(aes(x = age, y = log(mortality_new)/1e5, group = year, color = year, linetype = "New"))
# Disability
disability_df$disability_adult_late4 <- rowSums(disability_df[,which(names(disability_df) %in% adult_late4_diseases)])
disability_df$disability_new <- disability_df$disability - disability_df$disability_adult_late4
disability_df %>%
  ggplot() + theme_bw() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(disability)/1e5, group = year, color = year, linetype = "Old")) + 
  geom_line(aes(x = age, y = log(disability_new)/1e5, group = year, color = year, linetype = "New"))

mortality_df$mortality_adult_late4 <- rowSums(mortality_df[,which(names(mortality_df) %in% adult_late4_diseases)])
mortality_df$mortality_new <- mortality_df$mortality - mortality_df$mortality_adult_late4
disability_df$disability_adult_late4 <- rowSums(disability_df[,which(names(disability_df) %in% adult_late4_diseases)])
disability_df$disability_new <- disability_df$disability - disability_df$disability_adult_late4
# Forecast population
forecasts_adult_late <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "both",
                                        start_year = start_year, end_year = end_year, end_age = end_age, 
                                        no_births = no_births, fertility_type = fertility_type, loc_name = loc_name, 
                                        growth_transitions = growth_transitions) 
forecasts_adult_late %>%
  filter(year%%10 == 0) %>%
  ggplot(aes(x = age)) + theme_bw() + facet_wrap(~location) +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) +
  labs(color = "Year") + xlab("Age") + ylab("") + ggtitle("Adult (late)")

# Compare to benchmark
dalys_adult_late4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, loc_name = loc_name, 
                                        start_year = start_year, end_year = end_year, no_births = no_births,
                                        fertility_type = fertility_type, growth_transitions = growth_transitions)
dalys_adult_late4 %>%
  ggplot(aes(x = year)) + theme_bw() + facet_wrap(~location) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Adult (late)", fill = "")


"
Remove senescent diseases (cluster4)
"
senescent4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Senescent")] 
## Define the change we want to assess
# Mortality
mortality_df$mortality_senescent4 <- rowSums(mortality_df[,which(names(mortality_df) %in% senescent4_diseases)])
mortality_df$mortality_new <- mortality_df$mortality - mortality_df$mortality_senescent4
mortality_df %>%
  ggplot() + theme_bw() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(mortality)/1e5, group = year, color = year, linetype = "Old")) + 
  geom_line(aes(x = age, y = log(mortality_new)/1e5, group = year, color = year, linetype = "New"))
# Disability
disability_df$disability_senescent4 <- rowSums(disability_df[,which(names(disability_df) %in% senescent4_diseases)])
disability_df$disability_new <- disability_df$disability - disability_df$disability_senescent4
disability_df %>%
  ggplot() + theme_bw() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(disability)/1e5, group = year, color = year, linetype = "Old")) + 
  geom_line(aes(x = age, y = log(disability_new)/1e5, group = year, color = year, linetype = "New"))
# Forecast population
forecasts_senescent <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "both",
                                      start_year = start_year, end_year = end_year, end_age = end_age, 
                                      no_births = no_births, fertility_type = fertility_type, loc_name = loc_name, 
                                      growth_transitions = growth_transitions)
forecasts_senescent %>%
  filter(year%%10 == 0) %>%
  ggplot(aes(x = age)) + theme_bw() + facet_wrap(~location) +
  geom_line(aes(y = population/1e6, group = as.character(year), color = year)) +
  labs(color = "Year") + xlab("Age") + ylab("") + ggtitle("Ageing-related")
# Compare to benchmark
dalys_senescent4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, loc_name = loc_name, 
                                      start_year = start_year, end_year = end_year, no_births = no_births,
                                      fertility_type = fertility_type, growth_transitions = growth_transitions)
dalys_senescent4 %>%
  ggplot(aes(x = year)) + theme_bw() + facet_wrap(~location) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Ageing-related", fill = "")



"
Combine and get global totals
"

dalys_global <- dalys_infant4 %>%
  mutate(experiment = "Infant") %>%
  rbind(mutate(dalys_adult_early4, experiment = "Adult (early)")) %>%
  rbind(mutate(dalys_adult_late4, experiment = "Adult (late)")) %>%
  rbind(mutate(dalys_senescent4, experiment = "Ageing-related")) %>%
  group_by(year, experiment) %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(location = "Global")
  
  

dalys_each <- dalys_infant4 %>%
  mutate(experiment = "Infant") %>%
  rbind(mutate(dalys_adult_early4, experiment = "Adult (early)")) %>%
  rbind(mutate(dalys_adult_late4, experiment = "Adult (late)")) %>%
  rbind(mutate(dalys_senescent4, experiment = "Ageing-related")) %>%
  rbind(dalys_global) %>%
  mutate(experiment = factor(experiment, ordered = T, levels = c("Infant", "Adult (early)", "Adult (late)", "Ageing-related"))) %>%
  mutate(location = factor(str_remove(location, "World Bank "), ordered = T, 
                           levels = c("Global", "High Income", "Upper Middle Income", "Lower Middle Income", "Low Income")))



dalys_each %>%
  ggplot(aes(x = year)) + theme_bw() + facet_wrap(~location+experiment, nrow = 5) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", fill = "")
ggsave("figures/forecasting/decomp_daly_cluster4_regions.pdf", width = 12, height = 10)


dalys_each %>%
  filter(location == "Global") %>%
  ggplot(aes(x = year)) + theme_bw() + facet_wrap(~experiment, nrow = 1) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", fill = "")
ggsave("figures/forecasting/decomp_daly_cluster4.pdf", width = 12, height = 4)


"
Compare removing the different clusters
"
# Population forecasts
ggarrange(base_plt, infant3_plt, adult3_plt, senescent3_plt, nrow = 1, 
          common.legend = TRUE, legend = "right", align = "hv")
ggsave("figures/compare_pop_forecasts_cluster3.pdf", width = 12, height = 4)
ggarrange(base_plt + ylim(c(0,270)), 
          infant4_plt + ylim(c(0,270)), 
          adult_early4_plt + ylim(c(0,270)), 
          adult_late4_plt + ylim(c(0,270)), 
          senescent4_plt + ylim(c(0,270)), 
          nrow = 1, common.legend = TRUE, legend = "right", align = "hv")
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
growth_transitions <- TRUE

results_df <- data.frame()
for (loc_name in locations[2:5]){
  print(paste0("Analysis for ", loc_name))
  # Select which region to focus on and which years to use as a starting point
  population_df <- filter(all_population_df, location == loc_name)
  fertility_df <- filter(all_fertility_df, location == loc_name, year >= start_year)
  mortality_df <- create_mortality_df(all_mortality_df, loc_name = loc_name, start_year = start_year, end_year = end_year, 
                                      end_age = end_age, growth_transitions = growth_transitions, 
                                      income_transition_df = income_transition_df)
  disability_df <- create_disability_df(all_disability_df, loc_name = loc_name, start_year = start_year, end_year = end_year, 
                                        end_age = end_age, growth_transitions = growth_transitions, 
                                        income_transition_df = income_transition_df)
  for (fertility_type in fertility_types){
    print(paste0("Fertility assumption: ", fertility_type))
    for (growth_transitions in c(FALSE, TRUE)){
      # Remove infant diseases
      mortality_df <- def_mortality_new(mortality_df, infant3_diseases)
      disability_df <- def_disability_new(disability_df, infant3_diseases)
      dalys_infant3 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                         start_year = start_year, end_year = end_year, no_births = no_births,
                                         fertility_type = fertility_type, growth_transitions = growth_transitions) %>%
        mutate(location = loc_name, intervention = "Infant", fertility_type = fertility_type, 
               clusters = "3", growth_trans = growth_transitions)
      mortality_df <- def_mortality_new(mortality_df, infant4_diseases)
      disability_df <- def_disability_new(disability_df, infant4_diseases)
      dalys_infant4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                         start_year = start_year, end_year = end_year, no_births = no_births,
                                         fertility_type = fertility_type, growth_transitions = growth_transitions) %>%
        mutate(location = loc_name, intervention = "Infant", fertility_type = fertility_type, 
               clusters = "4", growth_trans = growth_transitions)
      # Remove adult diseases
      mortality_df <- def_mortality_new(mortality_df, adult3_diseases)
      disability_df <- def_disability_new(disability_df, adult3_diseases)
      dalys_adult3 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                         start_year = start_year, end_year = end_year, no_births = no_births,
                                         fertility_type = fertility_type, growth_transitions = growth_transitions) %>%
        mutate(location = loc_name, intervention = "Adult", fertility_type = fertility_type, 
               clusters = "3", growth_trans = growth_transitions)
      mortality_df <- def_mortality_new(mortality_df, adult_early4_diseases)
      disability_df <- def_disability_new(disability_df, adult_early4_diseases)
      dalys_adult_early4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                         start_year = start_year, end_year = end_year, no_births = no_births,
                                         fertility_type = fertility_type, growth_transitions = growth_transitions) %>%
        mutate(location = loc_name, intervention = "Adult_early", fertility_type = fertility_type, 
               clusters = "4", growth_trans = growth_transitions)
      mortality_df <- def_mortality_new(mortality_df, adult_late4_diseases)
      disability_df <- def_disability_new(disability_df, adult_late4_diseases)
      dalys_adult_late4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                              start_year = start_year, end_year = end_year, no_births = no_births,
                                              fertility_type = fertility_type, growth_transitions = growth_transitions) %>%
        mutate(location = loc_name, intervention = "Adult_late", fertility_type = fertility_type, 
               clusters = "4", growth_trans = growth_transitions)
      # Remove senescent diseases
      mortality_df <- def_mortality_new(mortality_df, senescent3_diseases)
      disability_df <- def_disability_new(disability_df, senescent3_diseases)
      dalys_senescent3 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                         start_year = start_year, end_year = end_year, no_births = no_births,
                                         fertility_type = fertility_type, growth_transitions = growth_transitions) %>%
        mutate(location = loc_name, intervention = "Senescent", fertility_type = fertility_type, 
               clusters = "3", growth_trans = growth_transitions)
      mortality_df <- def_mortality_new(mortality_df, senescent4_diseases)
      disability_df <- def_disability_new(disability_df, senescent4_diseases)
      dalys_senescent4 <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                         start_year = start_year, end_year = end_year, no_births = no_births,
                                         fertility_type = fertility_type, growth_transitions = growth_transitions) %>%
        mutate(location = loc_name, intervention = "Senescent", fertility_type = fertility_type, 
               clusters = "4", growth_trans = growth_transitions)
      # Slow aging by 10%
      mortality_df <- slow_mortality(mortality_df, slow_by = 0.1)
      disability_df <- slow_disability(disability_df, slow_by = 0.1)
      dalys_slow_10pc <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                           start_year = start_year, end_year = end_year, no_births = no_births,
                                           fertility_type = fertility_type, growth_transitions = growth_transitions) %>% 
        mutate(location = loc_name, intervention = "Slow_10pc", fertility_type = fertility_type, 
               clusters = "NA", growth_trans = growth_transitions)
      # Slow aging by 25%
      mortality_df <- slow_mortality(mortality_df, slow_by = 0.25)
      disability_df <- slow_disability(disability_df, slow_by = 0.25)
      dalys_slow_25pc <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                           start_year = start_year, end_year = end_year, no_births = no_births,
                                           fertility_type = fertility_type, growth_transitions = growth_transitions) %>% 
        mutate(location = loc_name, intervention = "Slow_25pc", fertility_type = fertility_type, 
               clusters = "NA", growth_trans = growth_transitions)
      
      # Append all results
      results_df <- rbind(results_df, dalys_infant3, dalys_adult3, dalys_senescent3, 
                          dalys_infant4, dalys_adult_early4, dalys_adult_late4, dalys_senescent4,
                          dalys_slow_10pc, dalys_slow_25pc)
    }
  }
}
# Rearrange and make sure location is an ordered factor
results_df <- relocate(results_df, location,intervention, fertility_type, clusters, .before = year)
results_df <- results_df %>% 
  mutate(loc_order = case_when(str_detect(location, "High")  ~ 1,
                               str_detect(location, "Upper") ~ 2,
                               str_detect(location, "Lower") ~ 3,
                               str_detect(location, "Low ") ~ 4)) %>%
  arrange(loc_order, intervention, fertility_type, clusters, year)
results_df$location <- factor(results_df$location, levels = unique(results_df$location), ordered = T)



global_results_df <- results_df %>%
  filter(fertility_type == "fertility_med" & clusters != "3") %>% 
  group_by(intervention, year, growth_trans) %>%
  summarise(across(where(is.numeric), sum))

ggplot(global_results_df, aes(x = year)) + theme_bw() + 
  facet_wrap(vars(growth_trans)) +
  scale_color_manual("Scenario", values = c("Senescent" = "firebrick1", "Adult_late" = "blue3", 
                                            "Adult_early" = "cornflowerblue", "Infant" = "forestgreen", 
                                            "Slow_10pc" = "darkgoldenrod3", "Slow_25pc" = "orange")) +
  geom_line(aes(y = daly_diff/1e9, color = intervention)) + 
  labs(x = "Year", y = "DALYs gained (billions)")
ggsave("figures/global_dalys_GvNG.pdf", width = 7, height = 3)

# Plot dalys over time for baseline case
glob_plt <- ggplot(filter(global_results_df,growth_trans == TRUE), aes(x = year)) + theme_bw() + 
  scale_color_manual("Scenario", values = c("Senescent" = "firebrick1", "Adult_late" = "blue3", 
                                            "Adult_early" = "cornflowerblue", "Infant" = "forestgreen", 
                                            "Slow_10pc" = "darkgoldenrod3", "Slow_25pc" = "orange")) +
  geom_line(aes(y = daly_diff/1e9, color = intervention)) + 
  labs(x = "Year", y = "DALYs gained (billions)")

reg_plt <- ggplot(filter(results_df,growth_trans == TRUE & fertility_type == "fertility_med" &
                           clusters != "3"), aes(x = year)) + 
  theme_bw() + facet_wrap(.~location, nrow = 2, scales = "free") +
  scale_color_manual("Scenario", values = c("Senescent" = "firebrick1", "Adult_late" = "blue3", 
                                            "Adult_early" = "cornflowerblue", "Infant" = "forestgreen", 
                                            "Slow_10pc" = "darkgoldenrod3", "Slow_25pc" = "orange")) +
  geom_line(aes(y = daly_diff/1e9, color = intervention)) + 
  labs(x = "Year", y = "DALYs gained (billions)")

ggarrange(glob_plt, reg_plt + ylab(""), common.legend = TRUE, widths = c(0.35, 0.65), 
          legend = "right")
ggsave("figures/global_dalys_overtime.pdf", width = 10, height = 4)


baseline_results <- filter(results_df, clusters != "3" & fertility_type == "fertility_med" &
                             growth_trans == TRUE)

ggplot(filter(global_results_df,growth_trans == TRUE), aes(x = year)) + theme_bw() + 
  facet_wrap(.~ intervention, scales = "free") +
  #geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity", group = intervention), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "", fill = "")
ggsave("figures/global_dalys_compliment.pdf", width = 8, height = 4)


ggplot(baseline_results) + theme_bw() +
  #facet_wrap(.~fertility_type) +
  geom_bar(aes(x = intervention, y = daly_diff/1e9, fill = location), stat = "summary", fun = "sum") +
  scale_fill_manual("Region", values = loc_cols) + 
  scale_color_manual("Region", values = loc_cols) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(y = "Extra DALYs (billions)", x = "Secnario", fill = "")
ggsave("figures/global_dalys_byregion.pdf", width = 5, height = 3)






plot_df <- filter(results_df,growth_trans == TRUE & clusters != "3" &
                    fertility_type %in% c("fertility_med", "fertility_est")) %>%
  mutate(fertility_type = case_when(fertility_type == "fertility_med"  ~ "Medium variant", 
                                    fertility_type == "fertility_est"  ~ "2021 rates"))


ggplot(plot_df, aes(x = year)) + theme_bw() + 
  facet_wrap(.~fertility_type + location , nrow = 2, scales = "free") +
  scale_color_manual("Scenario", values = c("Senescent" = "firebrick1", "Adult_late" = "blue3", 
                                            "Adult_early" = "cornflowerblue", "Infant" = "forestgreen", 
                                            "Slow_10pc" = "darkgoldenrod3", "Slow_25pc" = "orange")) +
  geom_line(aes(y = daly_diff/1e9, color = intervention)) + 
  labs(x = "Year", y = "DALYs gained (billions)")
ggsave("figures/fertility_type_byregion.pdf", width = 12, height = 4.5)




