setwd("/Users/julianashwin/Documents/GitHub/dalys-model/")
rm(list=ls())

library(ggplot2)
library(ggpubr)
library(readxl)
library(tidyverse)

source("functions.R")


# Import data 
all_fertility_df <- read_csv("clean_data/_archive/fertility_rates.csv")
all_population_df <- read_csv("clean_data/_archive/population.csv")
all_mortality_df <- read_csv("clean_data/_archive/mortality_medium.csv")
all_disability_df <- read_csv("clean_data/_archive/disability_medium.csv")
cause_tree_df <- read_csv("clean_data/_archive/cause_definitions.csv")
income_transition_df <- read_csv("clean_data/_archive/income_transition_shares.csv")

locations <- unique(all_mortality_df$location)
infant4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Infant")] 
adult_early4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_early")] 
adult_late4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_late")] 
senescent4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Senescent")] 



# Starting point
loc_cols <- c("Global" = "black","World Bank High Income" = "forestgreen", 
              "World Bank Upper Middle Income" = "green", "World Bank Lower Middle Income" = "orange",
              "World Bank Low Income" = "red")
cluster_cols <- c("Ageing-related" = "firebrick1", "Adult (late)" = "blue3", 
                  "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")


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
  fertility_df <- filter(all_fertility_df, #year >= start_year,
                         location %in% c("World Bank High Income", "World Bank Low Income", 
                                         "World Bank Lower Middle Income", "World Bank Upper Middle Income"))
} else {
  population_df <- filter(all_population_df, location == loc_name)
  fertility_df <- filter(all_fertility_df, location == loc_name, year >= start_year)
}

# Create mortality data, with growth transitions
mortality_df <- create_mortality_df(all_mortality_df, loc_name = loc_name, start_year = 1990, end_year = end_year, 
                                    end_age = end_age, growth_transitions = TRUE, 
                                    income_transition_df = income_transition_df)
mortality_df %>%
  #filter(year >2017, year<2023) %>%
  ggplot() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(mortality/1e5), group = year, color = year))
# Create disability data, with growth transitions
disability_df <- create_disability_df(all_disability_df, loc_name = loc_name, start_year = 1990, end_year = end_year, 
                                    end_age = end_age, growth_transitions = TRUE, 
                                    income_transition_df = income_transition_df)
disability_df %>%
  ggplot() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(disability/1e5), group = year, color = year))



"
Growth vs no growth
"
forecasts_nogrowth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                     start_year = 2021, end_year = end_year, end_age = end_age, 
                                     no_births = no_births, fertility_type = "fertility_est", loc_name = loc_name,
                                     growth_transitions = FALSE) %>% mutate(growth = "No Growth", fertility = "2021")
forecasts_growth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                   start_year = 2021, end_year = end_year, end_age = end_age, 
                                   no_births = no_births, fertility_type = "fertility_est", loc_name = loc_name, 
                                   growth_transitions = TRUE) %>% mutate(growth = "Growth", fertility = "2021")
forecasts_both <- rbind(forecasts_nogrowth, forecasts_growth)
forecasts_nogrowth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                     start_year = 2021, end_year = end_year, end_age = end_age, 
                                     no_births = no_births, fertility_type = "fertility_med", loc_name = loc_name,
                                     growth_transitions = FALSE) %>% mutate(growth = "No Growth", fertility = "Medium")
forecasts_growth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                   start_year = 2021, end_year = end_year, end_age = end_age, 
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
Get mortality and disability for different scenarios
"
# Mortality due to each disease cluster
mortality_df$mortality_infant4 <- rowSums(mortality_df[,which(names(mortality_df) %in% infant4_diseases)])
mortality_df$mortality_adult_early4 <- rowSums(mortality_df[,which(names(mortality_df) %in% adult_early4_diseases)])
mortality_df$mortality_adult_late4 <- rowSums(mortality_df[,which(names(mortality_df) %in% adult_late4_diseases)])
mortality_df$mortality_senescent4 <- rowSums(mortality_df[,which(names(mortality_df) %in% senescent4_diseases)])

# Disability due to each disease cluster
disability_df$disability_infant4 <- rowSums(disability_df[,which(names(disability_df) %in% infant4_diseases)])
disability_df$disability_adult_early4 <- rowSums(disability_df[,which(names(disability_df) %in% adult_early4_diseases)])
disability_df$disability_adult_late4 <- rowSums(disability_df[,which(names(disability_df) %in% adult_late4_diseases)])
disability_df$disability_senescent4 <- rowSums(disability_df[,which(names(disability_df) %in% senescent4_diseases)])



"
What does reducing a disease look like?
"
mortality_df %>%
  filter(location == "World Bank Lower Middle Income", year == 2021) %>%
  mutate(Total = mortality) %>%
  select(age, Total, mortality_infant4, mortality_adult_early4, mortality_adult_late4, mortality_senescent4) %>%
  pivot_longer(cols = -c(age, Total)) %>%
  rbind(disability_df %>% 
          filter(location == "World Bank Lower Middle Income", year == 2021) %>%
          mutate(Total = disability) %>%
          select(age, Total, disability_infant4, disability_adult_early4, disability_adult_late4, disability_senescent4) %>%
          pivot_longer(cols = -c(age, Total))) %>%
  mutate(value = value/1e5, Total = Total/1e5,
         variable = case_when(str_detect(name, "disability") ~ "Disability", TRUE ~ "Mortality"),
         cluster = case_when(str_detect(name, "infant4") ~ "Infant",
                             str_detect(name, "adult_early4") ~ "Adult (early)",
                             str_detect(name, "adult_late4") ~ "Adult (late)",
                             str_detect(name, "senescent4") ~ "Ageing-related", TRUE ~ "All")) %>%
  ggplot(aes(x = age)) + theme_bw() + facet_wrap(~ variable) + 
  geom_line(aes(y = Total)) +
  geom_bar(aes(y = value, fill = cluster), stat = "identity", position = "stack") +
  scale_fill_manual("Region", values = cluster_cols) + 
  labs()
  

mortality_df %>%
  filter(location == "World Bank Lower Middle Income", year == 2021) %>%
  mutate(Total = mortality) %>%
  select(age, Total, mortality_infant4, mortality_adult_early4, mortality_adult_late4, mortality_senescent4) %>%
  pivot_longer(cols = -c(age, Total)) %>%
  rbind(disability_df %>% 
          filter(location == "World Bank Lower Middle Income", year == 2021) %>%
          mutate(Total = disability) %>%
          select(age, Total, disability_infant4, disability_adult_early4, disability_adult_late4, disability_senescent4) %>%
          pivot_longer(cols = -c(age, Total))) %>%
  mutate(value = value/1e5, Total = Total/1e5,
         variable = case_when(str_detect(name, "disability") ~ "Disability", TRUE ~ "Mortality"),
         cluster = case_when(str_detect(name, "infant4") ~ "Infant",
                             str_detect(name, "adult_early4") ~ "Adult (early)",
                             str_detect(name, "adult_late4") ~ "Adult (late)",
                             str_detect(name, "senescent4") ~ "Ageing-related", TRUE ~ "All")) %>%
  mutate(`10% reduction` = Total - 0.1*value, 
         `20% reduction` = Total - 0.2*value, 
         `50% reduction` = Total - 0.5*value, 
         `Eradication` = Total - value) %>%
  pivot_longer(cols = c(`10% reduction`, `20% reduction`, `50% reduction`, `Eradication`), 
               names_to = "scenario", values_to = "value_new") %>%
  ggplot(aes(x = age)) + theme_bw() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  facet_wrap(~variable+cluster, nrow = 2, scales = "free_y") + 
  geom_line(aes(y = log(Total)), color = "black") +
  geom_line(aes(y = log(value_new), color = scenario), linetype = "dashed") +
  scale_color_manual("Scenario", values = c("red4", "red3", "red2", "red1")) + 
  labs(x = "Age", y = "log scale")
ggsave("figures/scenarios/eradicating_disease.pdf", width = 10, height = 6)

"
An example of removing some disease
"
mortality_df$mortality_new <- mortality_df$mortality - 0*mortality_df$mortality_senescent4
disability_df$disability_new <- disability_df$disability - 0*disability_df$disability_senescent4

dalys_compare <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, loc_name = loc_name, 
                                   start_year = 2021, end_year = end_year, no_births = TRUE, 
                                   fertility_type = fertility_type, growth_transitions = growth_transitions)
dalys_compare %>%
  ggplot(aes(x = year)) + theme_bw() + facet_wrap(~location) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Infant", fill = "")





"
Systematically go through some options
"
# Options
start_years <- c(1990, 2005, 2021)
categories <- c("infant4", "adult_early4", "adult_late4", "senescent4")
erad_opts <- seq(0.1,1,0.1)
grwth_opts <- c(FALSE, TRUE)
birth_opts <- c(FALSE, TRUE)

# Convert to dataframes as tibbles don't cooperate...
mortality_data <- mortality_df %>%
  select(location, year, age_name, age, mortality, mortality_infant4, mortality_adult_early4,
         mortality_adult_late4, mortality_senescent4) %>%
  as.data.frame()
disability_data <- disability_df %>%
  select(location, year, age_name, age, disability, disability_infant4, disability_adult_early4,
         disability_adult_late4, disability_senescent4) %>%
  as.data.frame()



# Compare to benchmark
sy <- start_years[1]
cat <- categories[1]
erad <- erad_opts[1]
grwth <- grwth_opts[1]
brth <- birth_opts[1]
# loop over combos
dalys_scenarios <- tibble()
# Change the start year
for (sy in start_years){
  # Which disease category
  for (cat in categories){
    # How much to eradicate
    for (erad in erad_opts){
      # Include economic growth
      for (grwth in grwth_opts){
        # Include new births
        for (brth in birth_opts){
          # Progress update
          print(str_c("Start year ", sy, ", diseases ", cat, ", eradicating ", erad, ", growth ", grwth, 
                      ", no births ", brth))
          # Define new mortality and disabilty fns
          mortality_data$mortality_new <- mortality_data$mortality - erad*mortality_data[,str_c("mortality_",cat)]
          disability_data$disability_new <- disability_data$disability - erad*disability_data[,str_c("disability_",cat)]
          # Compare scenarios
          dalys_compare <- compare_forecasts(population_df, fertility_df, mortality_data, disability_data, 
                                             loc_name = "Regions", start_year = sy, end_year = sy+100, 
                                             no_births = brth,  fertility_type = "fertility_med", 
                                             growth_transitions = grwth)
          dalys_scenario <- dalys_compare %>% 
            mutate(start_year = sy, no_births = brth, growth_transitions = grwth, eradication = erad, diseases = cat) %>%
            select(start_year, no_births, growth_transitions, eradication, diseases, location, year, 
                   population_base, population_new, population_mort, population_dis, daly_base, daly_new, daly_mort, daly_dis)
          dalys_scenarios <- rbind(dalys_scenarios, dalys_scenario)
        }
      }
    }
    print("Saving file")
    dalys_scenarios %>%
      write_rds("temp.rds")
  }
}

dalys_scenarios <- dalys_scenarios %>%
  mutate(pop_diff = population_new - population_base, daly_diff = daly_new - daly_base,
         pop_diff_mort = population_mort - population_base, daly_diff_mort = daly_mort - daly_base,
         pop_diff_dis = population_dis - population_base, daly_diff_dis = daly_dis - daly_base) %>%
  mutate(no_births = case_when(no_births == TRUE ~ "current population only", TRUE ~ "with new births"),
         growth_transitions = case_when(growth_transitions == TRUE ~ "Economic Growth", TRUE ~ "No Growth"))

dalys_scenarios %>%
  saveRDS("clean_data/daly_scenarios.rds")



dalys_scenarios %>%
  filter(growth_transitions == "Economic Growth" & no_births == "current population only" & 
           start_year == 2021 & eradication == 0.5 & diseases == "senescent4") %>%
  ggplot(aes(x = year)) + theme_bw() + facet_wrap(~location+no_births, nrow = 1) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(y = "Extra DALYs (millions)", x = "Year",  fill = "", title = "50% eradication of ageing-related")
ggsave("figures/scenarios/example_complimentarity_senescent.pdf", width = 10, height = 3)

dalys_scenarios %>%
  filter(growth_transitions == "Economic Growth" & no_births == "current population only" & 
           start_year == 2021 & eradication == 0.5 & diseases == "infant4") %>%
  ggplot(aes(x = year)) + theme_bw() + facet_wrap(~location+no_births, nrow = 1) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(y = "Extra DALYs (millions)", x = "Year",  fill = "", title = "50% eradication of infant")
ggsave("figures/scenarios/example_complimentarity_infant.pdf", width = 10, height = 3)


daly_summary <- dalys_scenarios %>%
  group_by(start_year, no_births, growth_transitions, eradication, diseases, location) %>%
  summarise(daly_diff = sum(daly_diff), daly_diff_mort = sum(daly_diff_mort), daly_diff_dis = sum(daly_diff_dis)) %>%
  mutate(complimentarity = (daly_diff - daly_diff_mort - daly_diff_dis)/daly_diff)

daly_summary %>%
  mutate(location = str_remove(location, "World Bank ")) %>%
  filter(growth_transitions == "Economic Growth") %>%
  ggplot() + theme_bw() + 
  facet_wrap(~no_births +location, nrow = 2, scales = "free_y") +
  geom_line(aes(x = eradication, y = daly_diff, color = diseases, linetype = factor(start_year))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(y = "Extra DALYs (millions)", x = "Reduction", linetype = "Start year")
ggsave("figures/scenarios/daly_diff_reduction.pdf", width = 10, height = 6)


daly_summary %>%
  mutate(location = str_remove(location, "World Bank ")) %>%
  filter(growth_transitions == "Economic Growth") %>%
  ggplot() + theme_bw() + 
  facet_wrap(~no_births +location, nrow = 2, scales = "free_y") +
  geom_line(aes(x = eradication, y = complimentarity, color = diseases, linetype = factor(start_year))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(y = "Complimentarity", x = "Reduction", linetype = "Start year")
ggsave("figures/scenarios/comp_reduction.pdf", width = 10, height = 6)




dalys_scenarios %>%
  mutate(location = str_remove(location, "World Bank ")) %>%
  filter(growth_transitions == "Economic Growth" & 
           start_year == 2021 & eradication == 1 ) %>%
  ggplot(aes(x = year)) + theme_bw() + facet_wrap(~no_births+location, scales = "free_y", ncol = 4) +
  geom_line(aes(y = daly_diff/1e9, color = diseases)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(y = "Extra DALYs (billions)", x = "Year",  fill = "")
ggsave("figures/scenarios/daly_diff_overtime.pdf", width = 10, height = 6)

"
Analyse range of scenarios
"









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




