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
end_year <- start_year+125
end_age <- 150
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
                                    end_age = 100, growth_transitions = TRUE, 
                                    income_transition_df = income_transition_df)
mortality_df %>%
  #filter(year >2017, year<2023) %>%
  ggplot() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(mortality/1e5), group = year, color = year))
# Create disability data, with growth transitions
disability_df <- create_disability_df(all_disability_df, loc_name = loc_name, start_year = 1990, end_year = end_year, 
                                    end_age = 100, growth_transitions = TRUE, 
                                    income_transition_df = income_transition_df)
disability_df %>%
  ggplot() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(disability/1e5), group = year, color = year))





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

# Play forward mortality based on exponential starting at 75
mortality_df <- mortality_df %>% 
  full_join(crossing(location = unique(mortality_df$location), year = unique(mortality_df$year),
                     sex_name = unique(mortality_df$sex_name), age = 0:150)) %>%
  arrange(location, year, sex_name, age) %>%
  group_by(location, year, sex_name) %>%
  # Only use the trend from 75 onwards
  mutate(age_NAs = case_when(age >= 75 & age <= 100 ~ age)) %>%
  mutate(log_mortality_infant4 = case_when(age >= 75 ~ log(mortality_infant4))) %>%
  mutate(log_mortality_adult_early4 = case_when(age >= 75 ~ log(mortality_adult_early4))) %>%
  mutate(log_mortality_adult_late4 = case_when(age >= 75 ~ log(mortality_adult_late4))) %>%
  mutate(log_mortality_senescent4 = case_when(age >= 75 ~ log(mortality_senescent4))) %>%
  # Project forward infant
  mutate(beta1 = cov(log_mortality_infant4, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_mortality_infant4, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(mortality_infant4_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  # Project forward adult_early
  mutate(beta1 = cov(log_mortality_adult_early4, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_mortality_adult_early4, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(mortality_adult_early4_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  # Project forward adult_late
  mutate(beta1 = cov(log_mortality_adult_late4, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_mortality_adult_late4, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(mortality_adult_late4_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  # Project forward senescent
  mutate(beta1 = cov(log_mortality_senescent4, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_mortality_senescent4, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(mortality_senescent4_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  select(-c(log_mortality_infant4, log_mortality_adult_early4, log_mortality_adult_late4, 
            log_mortality_senescent4, beta1, beta0)) %>%
  # Add in the projections
  mutate(mortality_infant4_proj = case_when(age <= 100 ~ mortality_infant4, 
                                            age > 100 ~ mortality_infant4_fit)) %>%
  mutate(mortality_adult_early4_proj = case_when(age <= 100 ~ mortality_adult_early4, 
                                                 age > 100 ~ mortality_adult_early4_fit)) %>%
  mutate(mortality_adult_late4_proj = case_when(age <= 100 ~ mortality_adult_late4, 
                                                age > 100 ~ mortality_adult_late4_fit)) %>%
  mutate(mortality_senescent4_proj = case_when(age <= 100 ~ mortality_senescent4, 
                                               age > 100 ~ mortality_senescent4_fit)) %>%
  mutate(mortality_proj = mortality_infant4_proj+mortality_adult_early4_proj+mortality_adult_late4_proj+
           mortality_senescent4_proj) %>%
  relocate(year, sex_name, age, mortality, mortality_proj,
           mortality_infant4, mortality_infant4_fit, mortality_infant4_proj,
           mortality_adult_early4, mortality_adult_early4_fit, mortality_adult_early4_proj,
           mortality_adult_late4, mortality_adult_late4_fit, mortality_adult_late4_proj,
           mortality_senescent4, mortality_senescent4_fit, mortality_senescent4_proj,
           .after = location) %>%
  ungroup() 


# Play forward disability based on exponential starting at 75
disability_df <- disability_df %>% 
  full_join(crossing(location = unique(disability_df$location), year = unique(disability_df$year),
                     sex_name = unique(disability_df$sex_name), age = 0:150)) %>%
  arrange(location, year, sex_name, age) %>%
  group_by(location, year, sex_name) %>%
  # Only use the trend from 75 onwards
  mutate(age_NAs = case_when(age >= 75 & age <= 100 ~ age)) %>%
  mutate(log_disability_infant4 = case_when(age >= 75 ~ log(disability_infant4))) %>%
  mutate(log_disability_adult_early4 = case_when(age >= 75 ~ log(disability_adult_early4))) %>%
  mutate(log_disability_adult_late4 = case_when(age >= 75 ~ log(disability_adult_late4))) %>%
  mutate(log_disability_senescent4 = case_when(age >= 75 ~ log(disability_senescent4))) %>%
  # Project forward infant
  mutate(beta1 = cov(log_disability_infant4, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_disability_infant4, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(disability_infant4_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  # Project forward adult_early
  mutate(beta1 = cov(log_disability_adult_early4, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_disability_adult_early4, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(disability_adult_early4_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  # Project forward adult_late
  mutate(beta1 = cov(log_disability_adult_late4, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_disability_adult_late4, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(disability_adult_late4_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  # Project forward senescent
  mutate(beta1 = cov(log_disability_senescent4, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_disability_senescent4, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(disability_senescent4_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  select(-c(log_disability_infant4, log_disability_adult_early4, log_disability_adult_late4, 
            log_disability_senescent4, beta1, beta0)) %>%
  # Add in the projections
  mutate(disability_infant4_proj = case_when(age <= 100 ~ disability_infant4, 
                                             age > 100 ~ disability_infant4_fit)) %>%
  mutate(disability_adult_early4_proj = case_when(age <= 100 ~ disability_adult_early4, 
                                                  age > 100 ~ disability_adult_early4_fit)) %>%
  mutate(disability_adult_late4_proj = case_when(age <= 100 ~ disability_adult_late4, 
                                                 age > 100 ~ disability_adult_late4_fit)) %>%
  mutate(disability_senescent4_proj = case_when(age <= 100 ~ disability_senescent4, 
                                                age > 100 ~ disability_senescent4_fit)) %>%
  mutate(disability_proj = disability_infant4_proj+disability_adult_early4_proj+disability_adult_late4_proj+
           disability_senescent4_proj) %>%
  relocate(year, sex_name, age, disability, disability_proj,
           disability_infant4, disability_infant4_fit, disability_infant4_proj,
           disability_adult_early4, disability_adult_early4_fit, disability_adult_early4_proj,
           disability_adult_late4, disability_adult_late4_fit, disability_adult_late4_proj,
           disability_senescent4, disability_senescent4_fit, disability_senescent4_proj,
           .after = location)  %>%
  ungroup() 


# Plot the mortality projections
p1 <- mortality_df %>%
  filter(year == 2021 & age < 150) %>%
  ggplot() + theme_bw() + facet_wrap(~location) +
  geom_hline(aes(yintercept = log(1e5)), linetype = "dashed") +
  geom_line(aes(x = age, log(mortality_senescent4_fit), color = "Ageing-related"), linetype = "dashed") + 
  geom_line(aes(x = age, log(mortality_senescent4), color = "Ageing-related")) + 
  geom_line(aes(x = age, log(mortality_infant4_fit), color = "Infant"), linetype = "dashed") + 
  geom_line(aes(x = age, log(mortality_infant4), color = "Infant")) + 
  geom_line(aes(x = age, log(mortality_adult_early4_fit), color = "Adult (early)"), linetype = "dashed") + 
  geom_line(aes(x = age, log(mortality_adult_early4), color = "Adult (early)")) + 
  geom_line(aes(x = age, log(mortality_adult_late4_fit), color = "Adult (late)"), linetype = "dashed") + 
  geom_line(aes(x = age, log(mortality_adult_late4), color = "Adult (late)")) +
  scale_color_manual(values = cluster_cols) + 
  labs(x = "Age", y = "log mortality", color = "Category", title = "Mortality")
p2 <- disability_df %>%
  filter(year == 2021 & age < 150) %>%
  ggplot() + theme_bw() + facet_wrap(~location) +
  geom_hline(aes(yintercept = log(1e5)), linetype = "dashed") +
  geom_line(aes(x = age, log(disability_senescent4_fit), color = "Ageing-related"), linetype = "dashed") + 
  geom_line(aes(x = age, log(disability_senescent4), color = "Ageing-related")) + 
  geom_line(aes(x = age, log(disability_infant4_fit), color = "Infant"), linetype = "dashed") + 
  geom_line(aes(x = age, log(disability_infant4), color = "Infant")) + 
  geom_line(aes(x = age, log(disability_adult_early4_fit), color = "Adult (early)"), linetype = "dashed") + 
  geom_line(aes(x = age, log(disability_adult_early4), color = "Adult (early)")) + 
  geom_line(aes(x = age, log(disability_adult_late4_fit), color = "Adult (late)"), linetype = "dashed") + 
  geom_line(aes(x = age, log(disability_adult_late4), color = "Adult (late)")) +
  scale_color_manual(values = cluster_cols) + 
  labs(x = "Age", y = "log disability", color = "Category", title = "Disability")
ggarrange(p1,p2, common.legend = T)
ggsave("figures/clustering/log_rates_proj.pdf", width = 10, height = 6)



mortality_df %>% ungroup() %>%
  filter(location == "World Bank Lower Middle Income", year == 2021) %>%
  mutate(Total = mortality_proj) %>%
  select(age, Total, mortality_infant4_proj, mortality_adult_early4_proj, 
         mortality_adult_late4_proj, mortality_senescent4_proj) %>%
  pivot_longer(cols = -c(age, Total)) %>%
  rbind(disability_df %>% ungroup() %>%
          filter(location == "World Bank Lower Middle Income", year == 2021) %>%
          mutate(Total = disability_proj) %>%
          select(age, Total, disability_infant4_proj, disability_adult_early4_proj,
                 disability_adult_late4_proj, disability_senescent4_proj) %>%
          pivot_longer(cols = -c(age, Total))) %>%
  mutate(proj = case_when(age > 100 ~ 0.5, TRUE ~ 1)) %>%
  mutate(value = case_when(Total > 1e5 ~ NA_real_, TRUE ~ value)) %>%
  group_by(name) %>% fill(value, .direction = "down") %>%
  mutate(value = value/1e5, Total = Total/1e5,
         variable = case_when(str_detect(name, "disability") ~ "Disability", TRUE ~ "Mortality"),
         cluster = case_when(str_detect(name, "infant4") ~ "Infant",
                             str_detect(name, "adult_early4") ~ "Adult (early)",
                             str_detect(name, "adult_late4") ~ "Adult (late)",
                             str_detect(name, "senescent4") ~ "Ageing-related", TRUE ~ "All")) %>%
  ggplot(aes(x = age)) + theme_bw() + facet_wrap(~ variable) + 
  geom_line(aes(y = Total)) +
  geom_bar(aes(y = value, fill = cluster, alpha = proj), stat = "identity", position = "stack") +
  scale_fill_manual("Region", values = cluster_cols) + 
  scale_alpha(guide="none", range = c(0.5, 1)) +
  lims(y = c(0,1))
ggsave("figures/clustering/projected_decomp.pdf", width = 8, height = 3.5)



"
Growth vs no growth
"
forecasts_nogrowth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                     start_year = 2021, end_year = end_year, end_age = end_age, 
                                     no_births = no_births, fertility_type = "fertility_est", loc_name = loc_name,
                                     growth_transitions = FALSE, project_100plus = TRUE) %>% 
  mutate(growth = "No Growth", fertility = "2021")
forecasts_growth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                   start_year = 2021, end_year = end_year, end_age = end_age, 
                                   no_births = no_births, fertility_type = "fertility_est", loc_name = loc_name, 
                                   growth_transitions = TRUE, project_100plus = TRUE) %>% 
  mutate(growth = "Growth", fertility = "2021")
forecasts_both <- rbind(forecasts_nogrowth, forecasts_growth)
forecasts_nogrowth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                     start_year = 2021, end_year = end_year, end_age = end_age, 
                                     no_births = no_births, fertility_type = "fertility_med", loc_name = loc_name,
                                     growth_transitions = FALSE, project_100plus = TRUE) %>% 
  mutate(growth = "No Growth", fertility = "Medium")
forecasts_growth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                   start_year = 2021, end_year = end_year, end_age = end_age, 
                                   no_births = no_births, fertility_type = "fertility_med", loc_name = loc_name, 
                                   growth_transitions = TRUE, project_100plus = TRUE) %>% 
  mutate(growth = "Growth", fertility = "Medium")
forecasts_both <- rbind(forecasts_both, forecasts_nogrowth, forecasts_growth)

ggplot(forecasts_both) + theme_bw() + facet_wrap(vars(location, growth, fertility), scales = "free") + 
  geom_line(aes(x = age, y = population, color = year, group = year)) + 
  labs(x = "Age", y = "Population", color = "Year", title = loc_name)
#ggsave(paste0("figures/growth_nogrowth_pop_",loc_name,".pdf"), width = 6, height = 5)


"
Forecasts under the baseline scenario
"
mortality_df <- mortality_df %>%
  mutate(mortality_new = mortality - 0.5*mortality_senescent4,
         mortality_proj_new = mortality_proj - 0.5*mortality_senescent4_proj)
disability_df <- disability_df %>%
  mutate(disability_new = disability - 0.5*disability_senescent4,
         disability_proj_new = disability_proj - 0.5*disability_senescent4_proj)

forecasts_base <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                 start_year = 2021, end_year = end_year, end_age = end_age, 
                                 no_births = no_births, fertility_type = fertility_type, loc_name = loc_name, 
                                 growth_transitions = growth_transitions, project_100plus = TRUE)

forecasts_base %>%
  group_by(location) %>%
  mutate(newborn = case_when(year - start_year - age == 0 ~ 1, TRUE ~ 0)) %>%
  summarise(W = sum(daly), Wnewborn = sum(daly*newborn))
  


"
What does reducing a disease look like?
"
mortality_df %>%
  filter(location == "World Bank Lower Middle Income", year == 2021) %>%
  mutate(Total = mortality_proj) %>%
  select(age, Total, mortality_infant4_proj, mortality_adult_early4_proj, 
         mortality_adult_late4_proj, mortality_senescent4_proj) %>%
  pivot_longer(cols = -c(age, Total)) %>%
  rbind(disability_df %>% 
          filter(location == "World Bank Lower Middle Income", year == 2021) %>%
          mutate(Total = disability_proj) %>%
          select(age, Total, disability_infant4_proj, disability_adult_early4_proj, 
                 disability_adult_late4_proj, disability_senescent4_proj) %>%
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
  geom_hline(aes(yintercept = log(1)), linetype = "dashed") +
  geom_line(aes(y = log(pmin(Total, 1))), color = "black") +
  geom_line(aes(y = log(pmin(value_new,1)), color = scenario), linetype = "dashed") +
  scale_color_manual("Scenario", values = c("red4", "red3", "red2", "red1")) + 
  labs(x = "Age", y = "log scale")
ggsave("figures/scenarios/eradicating_disease.pdf", width = 10, height = 6)


## These eradications pin down dmu/derr and dh/derr. 
mortality_df %>%
  filter(year == 2021) %>%
  mutate(Total = mortality_proj) %>%
  select(location, age, Total, mortality_infant4_proj, mortality_adult_early4_proj, 
         mortality_adult_late4_proj, mortality_senescent4_proj) %>%
  pivot_longer(cols = -c(location, age, Total)) %>%
  rbind(disability_df %>% 
          filter(year == 2021) %>%
          mutate(Total = disability_proj) %>%
          select(location, age, Total, disability_infant4_proj, disability_adult_early4_proj, 
                 disability_adult_late4_proj, disability_senescent4_proj) %>%
          pivot_longer(cols = -c(location, age, Total))) %>%
  mutate(value = value/1e5, Total = Total/1e5,
         variable = case_when(str_detect(name, "disability") ~ "Health", TRUE ~ "Mortality"),
         cluster = case_when(str_detect(name, "infant4") ~ "Infant",
                             str_detect(name, "adult_early4") ~ "Adult (early)",
                             str_detect(name, "adult_late4") ~ "Adult (late)",
                             str_detect(name, "senescent4") ~ "Ageing-related", TRUE ~ "All"),
         cluster = factor(cluster, ordered = T, levels = c("Infant", "Adult (early)",  "Adult (late)", "Ageing-related"))) %>%
  mutate(value = case_when(Total > 1.01 ~ 0, TRUE ~ value)) %>%
  ggplot(aes(x = age)) + theme_bw() + 
  facet_wrap(~variable+cluster, nrow = 2, scales = "free_y") + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_line(aes(y = value, color = location)) +
  labs(x = "Age", y = "Derivative wrt eradication", color = "Location")
ggsave("figures/derivatives/dmu_der.pdf", width = 10, height = 6)


"
An example of removing some disease
"
mortality_df$mortality_new <- mortality_df$mortality - 0.5*mortality_df$mortality_senescent4
disability_df$disability_new <- disability_df$disability - 0.5*disability_df$disability_senescent4

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

dalys_compare %>%
  #filter(year %in% c(start_year, end_year)) %>%
  select(location, year, LE_base, LE_new, LE_mort, LE_dis) %>%
  ggplot() + theme_bw() + facet_wrap(~location) +
  geom_line(aes(x = year, y = LE_base, color = "Base"))+
  geom_line(aes(x = year, y = LE_new, color = "New"))+
  geom_line(aes(x = year, y = LE_mort, color = "Mort"))+
  geom_line(aes(x = year, y = LE_dis, color = "Dis"))
  


"
dN_dmu
"
# Convert to dataframes as tibbles don't cooperate...
mortality_data <- mortality_df %>%
  select(location, year, age_name, age, mortality, mortality_proj, 
         mortality_infant4, mortality_infant4_proj, mortality_adult_early4, mortality_adult_early4_proj,
         mortality_adult_late4_proj, mortality_adult_late4_proj, mortality_senescent4, mortality_senescent4_proj) %>%
  as.data.frame()
disability_data <- disability_df %>%
  select(location, year, age_name, age, disability, disability_proj,
         disability_infant4, disability_infant4_proj, disability_adult_early4, disability_adult_early4_proj,
         disability_adult_late4, disability_adult_late4_proj, disability_senescent4, disability_senescent4_proj) %>%
  as.data.frame()
disability_data$disability_proj_new <- disability_data$disability_proj

# Define new mortality and disabilty fns
forecasts_base <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "none",
                                 start_year = 2021, end_year = 2146, end_age = end_age, 
                                 no_births = FALSE, fertility_type = "fertility_med", loc_name = "Regions", 
                                 growth_transitions = TRUE, project_100plus = TRUE)
pops_base <- forecasts_base %>%
  group_by(location, year) %>%
  summarise(pop = sum(population))
dN_dmu_results <- tibble()
for (aa in 0:150){
  print(str_c("Computing dN_Dmu for age ", aa))
  mortality_data$mortality_proj_new <- mortality_data$mortality_proj
  mortality_data$mortality_proj_new[which(mortality_data$age == aa)] <- 
    mortality_data$mortality_proj[which(mortality_data$age == aa)] - 0.0001*mortality_data$mortality_proj[which(mortality_data$age == aa)]
  
  forecasts_both <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "mortality",
                                   start_year = 2021, end_year = 2021+125, end_age = end_age, 
                                   no_births = FALSE, fertility_type = "fertility_med", loc_name = "Regions", 
                                   growth_transitions = TRUE, project_100plus = TRUE) 
  dN_dmu_results <- rbind(dN_dmu_results, 
                          forecasts_both %>% mutate(age = aa) %>%
                            group_by(location, year, age) %>%
                            summarise(pop_new = sum(population)) %>%
                            right_join(pops_base))
}

dN_dmu_results %>%
  rbind(mutate(dN_dmu_results, location = "Global")) %>%
  group_by(location, year, age) %>% 
  summarise(pop_new = sum(pop_new), pop = sum(pop)) %>%
  filter(year %in% c(2022, 2050, 2100), location %in%  c("Global", "World Bank High Income", "World Bank Low Income")) %>%
  mutate(dN_dmu = pop_new - pop) %>%
  ggplot() + theme_bw() + 
  facet_wrap(~location, scales = "free_y") +
  geom_line(aes(x = age, y = dN_dmu, color = factor(year)))
ggsave("figures/derivatives/dN_dmu.pdf", width = 10, height = 4)




forecasts_both <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "both",
                                 start_year = 2021, end_year = 2021+125, end_age = end_age, 
                                 no_births = FALSE, fertility_type = "fertility_med", loc_name = "Regions", 
                                 growth_transitions = TRUE, project_100plus = TRUE) %>% 
  mutate(start_year = sy, no_births = brth, growth_transitions = grwth, eradication = erad, diseases = cat, type = "both") %>%
  select(start_year, no_births, growth_transitions, eradication, diseases, type, location, year, age, population, daly)



dalys_compare <- compare_forecasts(population_df, fertility_df, mortality_data, disability_data, loc_name = loc_name, 
                                   start_year = 2021, end_year = end_year, no_births = TRUE,  project_100plus = TRUE,
                                   fertility_type = fertility_type, growth_transitions = growth_transitions)
dalys_compare %>%
  ggplot(aes(x = year)) + theme_bw() + facet_wrap(~location) +
  geom_line(aes(y = daly_diff/1e6), color = "black") +
  geom_bar(aes(y = daly_diff/1e6, fill = "Complimentarity"), stat = "identity") +
  geom_bar(aes(y = daly_diff_mort/1e6 + daly_diff_dis/1e6, fill = "Mortality"), stat = "identity") +
  geom_bar(aes(y = daly_diff_dis/1e6, fill = "Disability"), stat = "identity") +
  labs(y = "Extra DALYs (millions)", x = "Year", title = "Infant", fill = "")

dalys_compare %>%
  #filter(year %in% c(start_year, end_year)) %>%
  select(location, year, LE_base, LE_new, LE_mort, LE_dis) %>%
  ggplot() + theme_bw() + facet_wrap(~location) +
  geom_line(aes(x = year, y = LE_base, color = "Base"))+
  geom_line(aes(x = year, y = LE_new, color = "New"))+
  geom_line(aes(x = year, y = LE_mort, color = "Mort"))+
  geom_line(aes(x = year, y = LE_dis, color = "Dis"))





"
Systematically go through some options
"
# Options
start_years <- c(2021)
categories <- c("infant4", "adult_early4", "adult_late4", "senescent4")
erad_opts <- seq(-0.05,1,0.01)
grwth_opts <- TRUE # c(FALSE, TRUE)
birth_opts <- FALSE # c(FALSE, TRUE)

# Convert to dataframes as tibbles don't cooperate...
mortality_data <- mortality_df %>%
  select(location, year, age_name, age, mortality, mortality_proj, 
         mortality_infant4, mortality_infant4_proj, mortality_adult_early4, mortality_adult_early4_proj,
         mortality_adult_late4_proj, mortality_adult_late4_proj, mortality_senescent4, mortality_senescent4_proj) %>%
  as.data.frame()
disability_data <- disability_df %>%
  select(location, year, age_name, age, disability, disability_proj,
         disability_infant4, disability_infant4_proj, disability_adult_early4, disability_adult_early4_proj,
         disability_adult_late4, disability_adult_late4_proj, disability_senescent4, disability_senescent4_proj) %>%
  as.data.frame()



# Compare to benchmark
sy <- start_years[1]
cat <- categories[4]
erad <- erad_opts[1]
grwth <- grwth_opts[1]
brth <- birth_opts[1]
# loop over combos
full_scenarios <- tibble()
W_scenarios <- tibble()
# Change the start year
ii<- 0
for (sy in start_years){
  # Include economic growth
  for (grwth in grwth_opts){
    # Include new births
    for (brth in birth_opts){
      # Which disease category
      for (cat in categories[1:3]){
        # How much to eradicate of that disease
        for (erad in erad_opts){
          # How much to eradicate of senescent disease (for the cross derivs)
          for (erad_s in erad_opts){
            # Only go ahead if erad*erad_s is less that 0.5
            if (abs(erad*erad_s) < 0.03 | erad %in% c(0.2, 0.4, 0.6, 0.8, 1)){
              #ii <- ii+1}}}}}}}
            # Progress update
            print(str_c("Start year ", sy, ", diseases ", cat, " eradicating ", erad, " and ageing-related eradicating ", erad_s, 
                        ", growth ", grwth, ", no births ", brth))
            # Define new mortality and disabilty fns
            mortality_data$mortality_proj_new <- mortality_data$mortality_proj - erad*mortality_data[,str_c("mortality_",cat, "_proj")]
            mortality_data$mortality_proj_new <- mortality_data$mortality_proj_new - erad_s*mortality_data[,str_c("mortality_senescent4_proj")]
            disability_data$disability_proj_new <- disability_data$disability_proj - erad*disability_data[,str_c("disability_",cat, "_proj")]
            disability_data$disability_proj_new <- disability_data$disability_proj_new - erad*disability_data[,str_c("disability_senescent4_proj")]
            
            # Calculate Life Expectancy 
            LE_birth_results <- mortality_data %>%
              group_by(location, year) %>%
              mutate(survival = cumprod(1 - pmin(mortality_proj_new/1e5, 1))) %>%
              summarise(LE_birth = sum(survival))
            
            # Forecast eradicating on both mortality and disability
            forecasts_both <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "both",
                                             start_year = sy, end_year = sy+125, end_age = end_age, 
                                             no_births = brth, fertility_type = "fertility_med", loc_name = "Regions", 
                                             growth_transitions = grwth, project_100plus = TRUE) %>% 
              mutate(start_year = sy, no_births = brth, growth_transitions = grwth, eradication = erad, diseases = cat, erad_senes = erad_s, type = "both") %>%
              select(start_year, no_births, growth_transitions, eradication, diseases, erad_senes, type, location, year, age, mortality, population, daly)
            # Forecast eradicating on just mortality
            forecasts_mort <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "mortality",
                                             start_year = sy, end_year = sy+125, end_age = end_age, 
                                             no_births = brth, fertility_type = "fertility_med", loc_name = "Regions", 
                                             growth_transitions = grwth, project_100plus = TRUE) %>% 
              mutate(start_year = sy, no_births = brth, growth_transitions = grwth, eradication = erad, diseases = cat, erad_senes = erad_s, type = "mortality") %>%
              select(start_year, no_births, growth_transitions, eradication, diseases, erad_senes, type, location, year, age, mortality, population, daly)
            # Forecast eradicating on just disability
            forecasts_disab <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "disability",
                                             start_year = sy, end_year = sy+125, end_age = end_age, 
                                             no_births = brth, fertility_type = "fertility_med", loc_name = "Regions", 
                                             growth_transitions = grwth, project_100plus = TRUE) %>% 
              mutate(start_year = sy, no_births = brth, growth_transitions = grwth, eradication = erad, diseases = cat, erad_senes = erad_s, type = "disability") %>%
              select(start_year, no_births, growth_transitions, eradication, diseases, erad_senes, type, location, year, age, mortality, population, daly)
            
            full_scenarios <- rbind(forecasts_both, forecasts_mort, forecasts_disab) %>%
              mutate(newborn = case_when(year - start_year - age == 0 ~ 1, TRUE ~ 0)) %>%
              group_by(start_year, no_births, growth_transitions, eradication, diseases, type, erad_senes, location, year) %>%
              summarise(W = sum(daly), Wnewborn = sum(daly*newborn), average_age = sum(age*population)/sum(population), pop_newborns = sum(newborn*population),
                        population = sum(population)) %>%
              left_join(LE_birth_results)

            
            # Compare scenarios
            W_scenarios <- W_scenarios %>%
              rbind(full_scenarios)
            }
          }
        }
        print("Saving file")
        W_scenarios %>%
          saveRDS("temp.rds")
      }
    }
  }
}

W_scenarios %>%
  saveRDS("figures/scenarios/W_scenarios.rds")





