setwd("/Users/julianashwin/Documents/GitHub/dalys-model")
rm(list=ls())


library(tidyverse)
library(readxl)
library(janitor)
library(stargazer)
library(ggpubr)

"
Define age factor
"
age_groups <- c("0-1 years", "1-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",  
                "90-94 years", "95-99 years")
income_groups <- c("Global", "High Income", "Upper Middle Income", "Lower Middle Income", "Low Income")

"
Import data
"
cluster_df <- read_csv("clean_data/cluster_membership_data.csv") %>%
  mutate(cluster_4 = factor(cluster_4, ordered = T, levels = c("Infant", "Adult (early)", "Adult (late)", "Age-related")))
macro_df <- read_csv("clean_data/country_wdi_data.csv") %>%
  rbind(tibble(location_name = "Global", year = 2019, GNI_pc = 0, WDI_population = 0, income_group = "Global")) %>%
  mutate(income_group = factor(str_remove(income_group, "World Bank "), ordered = T, levels = income_groups))
lifetab_df <- readRDS("clean_data/country_lifetab_data.rds")
gbd_deaths_df <- readRDS("raw_data/GBD/gbd_data_deaths.rds")
gbd_ylds_df <- readRDS("raw_data/GBD/gbd_data_ylds.rds")
gbd_dalys_df <- readRDS("raw_data/GBD/gbd_data_dalys.rds")
gbd_prevalence_df <- readRDS("raw_data/GBD/gbd_data_prevalence.rds")
gbd_ylls_df <- readRDS("raw_data/GBD/gbd_data_ylls.rds")

## Aggregate up lifetable to group level
lifetab_grp_df <- lifetab_df %>%
  arrange(location_name, year, age) %>%
  group_by(location_name, year, age_name) %>%
  summarise(age_mid = sum(population*age)/sum(population), 
            mortality = sum(population*mortality)/sum(population),
            survival = sum(population*survival)/sum(population),
            remaining_le = sum(population*remaining_le)/sum(population),
            population = sum(population)) %>%
  arrange(location_name, year, age_mid)

## Latest available year from macro data
macro_latest_df <- macro_df %>% na.omit() %>%
  arrange(location_name, year) %>%
  group_by(location_name) %>%
  summarise_all(last)


"
What actually is a YLD?
"
# Prevalance rate sums to more than one as an individual can have multiple disease
gbd_prevalence_df %>%
  filter(metric_name == "Rate") %>%
  group_by(location_name, age_name) %>%
  summarise(lower = sum(lower/1e5), val = sum(val/1e5), upper = sum(upper/1e5)) %>%
  mutate(age = factor(str_remove(age_name, " years"), ordered = T, levels = str_remove(age_groups, " years"))) %>%
  ggplot() + theme_bw() + 
  geom_violin(aes(x = age, y = val))
# The YLD rate metric is contemporaneous, and is bounded by zero and one
gbd_ylds_df %>%
  filter(metric_name == "Rate") %>%
  group_by(location_name, age_name) %>%
  summarise(lower = sum(lower/1e5), val = sum(val/1e5), upper = sum(upper/1e5)) %>%
  mutate(age = factor(str_remove(age_name, " years"), ordered = T, levels = str_remove(age_groups, " years"))) %>%
  ggplot() + theme_bw() + 
  geom_violin(aes(x = age, y = val))
# But the YLD number is actually forward looking
gbd_ylds_df %>%
  pivot_wider(id_cols = c(location_name, cause_name, age_name), names_from = metric_name, values_from = val) %>%
  mutate(population = Number*(1e5/Rate)) %>%
  group_by(location_name) %>%
  summarise(Number = sum(Number, na.rm = T), population = sum(population, na.rm = T))
# So if we divide yld rate by prevalence rate, we should back out a disability rate
weights_df <- gbd_prevalence_df %>%
  filter(metric_name == "Rate") %>%
  pivot_wider(id_cols = c(location_name, cause_name, age_name), names_from = measure_name, values_from = val) %>%
  full_join(filter(gbd_ylds_df, metric_name == "Rate")) %>%
  pivot_wider(id_cols = c(location_name, cause_name, age_name, Prevalence), names_from = measure_name, values_from = val) %>%
  mutate(Prevalence = replace_na(Prevalence, 0)/1e5, YLDs = replace_na(YLDs, 0)/1e5)

# But how can we have zero prevalence but non zero YLDs?
weights_df$condition <- weights_df$Prevalence - weights_df$YLDs

  
"
Decompose mortality and disability by cluster
"  
gbd_deaths_df %>%
  filter(metric_name == "Rate") %>%
  left_join(filter(lifetab_grp_df, year == 2019)) %>%
  group_by(age_name, cause_name) %>%
  summarise(val = sum(population*val)/sum(population)/1e5) %>%
  left_join(distinct(cluster_df, cause_name, cluster_4)) %>%
  group_by(age_name, cluster_4) %>%
  summarise(val = sum(val)) %>%
  mutate(age = factor(str_remove(age_name, " years"), ordered = T, levels = str_remove(age_groups, " years"))) %>%
  ggplot(aes(x = age, y = val)) + theme_bw() + 
  geom_bar(aes(fill = fct_rev(cluster_4)), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("Age-related" = "firebrick1", "Adult (late)" = "blue3", 
                               "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(y = "Mortality rate", x = "Age", fill = "Disease cluster") 
ggsave("figures/clustering/global_mortality_cluster4.pdf", width = 6, height = 5)

gbd_ylds_df %>%
  filter(metric_name == "Rate") %>%
  left_join(filter(lifetab_grp_df, year == 2019)) %>%
  group_by(age_name, cause_name) %>%
  summarise(val = sum(population*val)/sum(population)/1e5) %>%
  right_join(distinct(cluster_df, cause_name, cluster_4)) %>%
  group_by(age_name, cluster_4) %>%
  summarise(val = sum(val)) %>%
  mutate(age = factor(str_remove(age_name, " years"), ordered = T, levels = str_remove(age_groups, " years"))) %>%
  ggplot(aes(x = age, y = val)) + theme_bw() + 
  geom_bar(aes(fill = fct_rev(cluster_4)), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("Age-related" = "firebrick1", "Adult (late)" = "blue3", 
                               "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(y = "Disability rate", x = "Age", fill = "Disease cluster") 
ggsave("figures/clustering/global_disability_cluster4.pdf", width = 6, height = 5)


"
Current Health Burden (i.e. deaths and disability in 2019)
"
gbd_yll_new_df <- gbd_deaths_df %>%
  filter(metric_name == "Number") %>%
  mutate(measure_name = "YLLs (new)") %>%
  left_join(filter(lifetab_grp_df, year == 2019)) %>%
  mutate(val = val*remaining_le, upper = upper*remaining_le, lower = val*lower) %>%
  select(location_name, cause_name, age_name, sex_name, measure_name, metric_name, val, upper, lower)

current_gbd_tab <- gbd_ylls_df %>%
  rbind(gbd_yll_new_df, gbd_ylds_df, gbd_dalys_df) %>%
  filter(metric_name == "Number") %>%
  mutate(location_name = "Global") %>%
  rbind(gbd_ylls_df, gbd_yll_new_df, gbd_ylds_df, gbd_dalys_df) %>%
  filter(metric_name == "Number") %>%
  inner_join(distinct(cluster_df, cause_name, cluster_4)) %>%
  inner_join(select(macro_latest_df, location_name, income_group)) %>%
  group_by(income_group, cluster_4, measure_name) %>%
  summarise(val = sum(val, na.rm = T), upper = sum(upper, na.rm = T), lower = sum(lower, na.rm = T)) %>%
  pivot_wider(id_cols = c(income_group, cluster_4), 
              names_from = measure_name, values_from = c(val))

# Burden in millions of years
divide.by.1e6 <- function(x, na.rm=FALSE) (round(x/1e6))
current_gbd_tab %>%
  mutate(`DALYs (new)` = YLDs + `YLLs (new)`) %>%
  mutate_if(is.numeric, divide.by.1e6, na.rm = TRUE) %>%
  as.matrix() %>%
  stargazer(table.placement = "H", title = "Current Global Disease Burden (in millions)", 
            label = "tab:current_gbd_millions")
# Burden as percentages
current_gbd_tab %>%
  mutate(`DALYs (new)` = YLDs + `YLLs (new)`) %>%
  group_by(income_group) %>%
  mutate(YLDs = round(100*(YLDs/sum(YLDs)),1), 
         YLLs = round(100*(YLLs/sum(YLLs)),1), 
         DALYs = round(100*(DALYs/sum(DALYs)),1), 
         `YLLs (new)` = round(100*(`YLLs (new)`/sum(`YLLs (new)`)),1), 
         `DALYs (new)` = round(100*(`DALYs (new)`/sum(`DALYs (new)`)),1)) %>%
  as.matrix() %>%
  stargazer(table.placement = "H", title = "Current Global Disease Burden (as a percentage)", 
            label = "tab:current_gbd_percent")
# Get disease burden by category for each country
current_gbd_countries_df <- gbd_ylls_df %>% 
  rbind(gbd_yll_new_df, gbd_ylds_df, gbd_dalys_df) %>%
  filter(metric_name == "Number") %>%
  inner_join(distinct(cluster_df, cause_name, cluster_4)) %>%
  inner_join(select(macro_latest_df, location_name, income_group, WDI_population)) %>%
  group_by(location_name, income_group, WDI_population, cluster_4, measure_name) %>%
  summarise(val = sum(val, na.rm = T), upper = sum(upper, na.rm = T), lower = sum(lower, na.rm = T)) %>%
  pivot_wider(id_cols = c(location_name, income_group, WDI_population, cluster_4), 
              names_from = measure_name, values_from = c(val))

current_gbd_countries_df %>%  
  arrange(location_name, -YLDs) %>%
  group_by(location_name, WDI_population) %>%
  summarise(cluster_4 = first(cluster_4)) %>%
  tabyl(cluster_4)

current_gbd_countries_df %>%  
  arrange(location_name, -YLLs) %>%
  group_by(location_name, WDI_population) %>%
  summarise(cluster_4 = first(cluster_4)) %>%
  tabyl(cluster_4)

current_gbd_countries_df %>%  
  arrange(location_name, -`YLLs (new)`) %>%
  group_by(location_name, WDI_population) %>%
  summarise(cluster_4 = first(cluster_4)) %>%
  tabyl(cluster_4)

current_gbd_countries_df %>%  
  arrange(location_name, -DALYs) %>%
  group_by(location_name, WDI_population) %>%
  summarise(cluster_4 = first(cluster_4)) %>%
  tabyl(cluster_4)

current_gbd_countries_df %>%  
  arrange(location_name, -DALYs) %>%
  group_by(location_name, WDI_population) %>%
  summarise(cluster_4 = first(cluster_4)) %>%
  group_by(cluster_4) %>%
  summarise(pop = sum(WDI_population)) %>% ungroup() %>%
  mutate(pop = pop/sum(pop))


"
Lifetime Health Burdens
"
lifetab_df %>%
  group_by(year, age) %>%
  summarise(`Survival Rate` = sum(survival*population)/sum(population), 
            Population = sum(population)) %>%
  group_by(year) %>%
  mutate(`Population (standardised)` = Population/max(Population)) %>%
  pivot_longer(cols = c(`Survival Rate`, `Population (standardised)`)) %>%
  mutate(name = factor(name, ordered = T, levels = c("Survival Rate", "Population (standardised)"))) %>%
  filter(year %% 10 == 0) %>%
  ggplot() + theme_bw() + facet_wrap(~name) +
  geom_line(aes(x = age, y = value, color = year, group = year)) + 
  labs(x = "Age", y = "", color = "Year")
ggsave("figures/lifetime_burden/global_pop_survival.pdf", width = 8, height = 4)

eg_def <- lifetab_df %>%
  filter(year == 1950, location_name == "Afghanistan")

plot(cumprod(c(1, 1 - eg_def$mortality[1:100])))
lines(eg_def$survival)

# WHy doesn't this sum to one?
lifetab_df %>%
  group_by(location_name, year) %>%
  summarise(val = sum(survival*mortality))

gbd_deaths_df %>%
  # Get mortality probability of each 
  filter(metric_name == "Rate") %>%
  mutate(val = val/1e5) %>%
  # Multiply by survival probability
  left_join(filter(lifetab_df, year == 2019)) %>%
  group_by(location_name, cause_name) %>%
  summarise(prob_death = sum(val*survival)) %>%
  # Adjust so that probabilities sum to one
  group_by(location_name) %>%
  mutate(prob_death = prob_death/sum(prob_death)) %>%
  # Aggregate up to cause_group
  inner_join(distinct(cluster_df, cause_name, cluster_4)) %>%
  group_by(location_name, cluster_4) %>%
  summarise(prob_death = sum(prob_death)) %>%
  # Merge in population of new-borns
  left_join(select(filter(lifetab_df, age == 0 & year == 2019), location_name, population)) %>%
  # Group into income categories, weighted by population
  inner_join(select(macro_latest_df, location_name, income_group)) %>%
  group_by(income_group, cluster_4) %>%
  summarise(prob_death = sum(prob_death*population)/sum(population), population = sum(population)) %>%
  ggplot() + theme_bw() + 
  geom_bar(aes(x = income_group, y = prob_death, fill = cluster_4), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("Age-related" = "firebrick1", "Adult (late)" = "blue3", 
  "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(y = "Probability of death for new-born", x = "Income Group", fill = "Disease cluster")
ggsave("figures/lifetime_burden/prob_death_newborn.pdf", width = 4, height = 4)
  

# Multiply each death prob by remaining LE at that age
p1 <- gbd_deaths_df %>%
  # Get mortality probability of each 
  filter(metric_name == "Rate") %>%
  mutate(val = val/1e5) %>%
  # Multiply by survival probability
  left_join(filter(lifetab_df, year == 2019)) %>%
  group_by(location_name, cause_name) %>%
  summarise(exp_yll = sum(val*survival*remaining_le)) %>%
  # Aggregate up to cause_group
  inner_join(distinct(cluster_df, cause_name, cluster_4)) %>%
  group_by(location_name, cluster_4) %>%
  summarise(exp_yll = sum(exp_yll)) %>%
  # Merge in population of new-borns
  left_join(select(filter(lifetab_df, age == 0 & year == 2019), location_name, population)) %>%
  # Group into income categories, weighted by population
  inner_join(select(macro_latest_df, location_name, income_group)) %>%
  group_by(income_group, cluster_4) %>%
  summarise(exp_yll = sum(exp_yll*population)/sum(population), population = sum(population)) %>%
  ggplot() + theme_bw() + 
  geom_bar(aes(x = income_group, y = exp_yll, fill = cluster_4), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("Age-related" = "firebrick1", "Adult (late)" = "blue3", 
                               "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(y = "Expected YLL for new-born", x = "Income Group", fill = "Disease cluster")
p1
ggsave("figures/lifetime_burden/exp_yll_newborn.pdf", width = 4, height = 4)


p2 <- gbd_ylds_df %>%
  # Get disability probability of each 
  filter(metric_name == "Rate") %>%
  mutate(val = val/1e5) %>%
  # Multiply by survival probability
  left_join(filter(lifetab_df, year == 2019)) %>%
  group_by(location_name, cause_name) %>%
  summarise(exp_yld = sum(val*survival)) %>%
  # Aggregate up to cause_group
  inner_join(distinct(cluster_df, cause_name, cluster_4)) %>%
  group_by(location_name, cluster_4) %>%
  summarise(exp_yld = sum(exp_yld)) %>%
  # Merge in population of new-borns
  left_join(select(filter(lifetab_df, age == 0 & year == 2019), location_name, population)) %>%
  # Group into income categories, weighted by population
  inner_join(select(macro_latest_df, location_name, income_group)) %>%
  group_by(income_group, cluster_4) %>%
  summarise(exp_yld = sum(exp_yld*population)/sum(population), population = sum(population)) %>%
  ggplot() + theme_bw() + 
  geom_bar(aes(x = income_group, y = exp_yld, fill = cluster_4), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("Age-related" = "firebrick1", "Adult (late)" = "blue3", 
                               "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(y = "Expected YLD for new-born", x = "Income Group", fill = "Disease cluster")
p2
ggsave("figures/lifetime_burden/exp_yld_newborn.pdf", width = 4, height = 4)

ggarrange(p1 + ggtitle("Expected YLL"), p2 + ggtitle("Expected YLD"), nrow = 1, common.legend = T, legend = "right")
ggsave("figures/lifetime_burden/exp_burden_newborn.pdf", width = 6.5, height = 4)



exp_yll_tab <- gbd_deaths_df %>%
  # Get mortality probability of each 
  filter(metric_name == "Rate") %>%
  mutate(val = val/1e5) %>%
  # Multiply by survival probability
  left_join(filter(lifetab_df, year == 2019)) %>%
  group_by(location_name, cause_name) %>%
  summarise(exp_yll = sum(val*survival*remaining_le)) %>%
  # Aggregate up to cause_group
  inner_join(distinct(cluster_df, cause_name, cluster_4)) %>%
  group_by(location_name, cluster_4) %>%
  summarise(exp_yll = sum(exp_yll)) %>%
  # Merge in population of new-borns
  left_join(select(filter(lifetab_df, age == 0 & year == 2019), location_name, population)) %>%
  # Group into income categories, weighted by population
  inner_join(select(macro_latest_df, location_name, income_group)) %>%
  group_by(income_group, cluster_4) %>%
  summarise(exp_yll = sum(exp_yll*population)/sum(population), population = sum(population))
exp_yld_tab <- gbd_ylds_df %>%
  # Get disability probability of each 
  filter(metric_name == "Rate") %>%
  mutate(val = val/1e5) %>%
  # Multiply by survival probability
  left_join(filter(lifetab_df, year == 2019)) %>%
  group_by(location_name, cause_name) %>%
  summarise(exp_yld = sum(val*survival)) %>%
  # Aggregate up to cause_group
  inner_join(distinct(cluster_df, cause_name, cluster_4)) %>%
  group_by(location_name, cluster_4) %>%
  summarise(exp_yld = sum(exp_yld)) %>%
  # Merge in population of new-borns
  left_join(select(filter(lifetab_df, age == 0 & year == 2019), location_name, population)) %>%
  # Group into income categories, weighted by population
  inner_join(select(macro_latest_df, location_name, income_group)) %>%
  group_by(income_group, cluster_4) %>%
  summarise(exp_yld = sum(exp_yld*population)/sum(population), population = sum(population))


exp_yll_tab %>%
  left_join(exp_yld_tab) %>%
  select(income_group, cluster_4, population, exp_yll, exp_yld) %>%
  write_csv("results/newborn_burden.csv")

