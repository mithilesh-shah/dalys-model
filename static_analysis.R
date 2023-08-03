setwd("/Users/julianashwin/Documents/GitHub/dalys-model")
rm(list=ls())


library(tidyverse)
library(readxl)
library(janitor)


"
Define age factor
"
age_groups <- c("0-1 years", "1-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years ", "85-89 year",  
                "90-94 years", "95-99 years")

"
Import data
"
cluster_df <- read_csv("clean_data/cluster_membership_data.csv")
macro_df <- read_csv("clean_data/country_wdi_data.csv")
lifetab_df <- read_csv("clean_data/country_lifetab_data.csv")
gbd_deaths_df <- readRDS("raw_data/GBD/gbd_data_deaths.rds")
gbd_ylds_df <- readRDS("raw_data/GBD/gbd_data_ylds.rds")
gbd_dalys_df <- readRDS("raw_data/GBD/gbd_data_dalys.rds")
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
gbd_ylds_df %>%
  filter(metric_name == "Rate") %>%
  group_by(location_name, age_name) %>%
  summarise(lower = sum(lower/1e5), val = sum(val/1e5), upper = sum(upper/1e5)) %>%
  tabyl(age_name)
  mutate(age = factor(str_remove(age_name, " years"), ordered = T, levels = str_remove(age_groups, " years"))) %>%
  ggplot() + theme_bw() + 
  geom_violin(aes(x = age, y = val))
  
  
"
Decompose mortality and disability by cluster
"  
gbd_deaths_df %>%
  left_join(lifetab_grp_df)
filter(measure_name == "Deaths" & metric_name == "Rate") %>%
  select(-measure_name, -upper, -lower) %>%
  pivot_wider(id_cols = c(year, location_name, sex_name, age_name, age_low), names_from = cause_name, 
              values_from = val, values_fill = 0) %>%
  arrange(year, location_name, age_low) %>% rowwise() %>%
  mutate(mortality = sum(c_across(`HIV/AIDS resulting in other diseases`:`Alcoholic cardiomyopathy`))) %>%
  relocate(mortality, .after = age_low) %>%
  full_join(fill_df, by = c("year", "location_name","age_low")) %>% ungroup() %>%
  arrange(year, location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")

  

max(total_yld_df$val)

df_old <- gbd_ylls_df %>%
  filter(metric_name == "Number") %>%
  inner_join(select(cluster_df, cause_name, cluster_4)) %>%
  inner_join(select(macro_latest_df, location_name, WDI_population, income_group)) %>%
  group_by(location_name, income_group, WDI_population, cluster_4, measure_name) %>%
  summarise(val = sum(val), upper = sum(upper), lower = sum(lower)) %>%
  pivot_wider(id_cols = c(location_name, WDI_population, income_group, cluster_4), 
              names_from = measure_name, values_from = c(val)) 

df_old <- gbd_deaths_df %>%
  filter(metric_name == "Number") %>%
  left_join(lifetab_grp_df)
  

gbd_deaths_df %>%



df_out <- gbd_ylls_df %>%
  inner_join(cluster_df) %>%
  inner_join(classification_data) %>%
  inner_join(pop_df) %>%
  group_by(code, location_name, population, income_group, cluster_no, cluster, measure_name, year) %>%
  summarise(val = sum(val), upper = sum(upper), lower = sum(lower)) %>%
  mutate(measure = str_sub(measure_name, 1, 4)) %>%
  pivot_wider(id_cols = c(code, location_name, population, income_group, cluster), 
              names_from = measure, values_from = c(val)) 
df_out %>%
  write_csv("andrew_YLL_YLD_countries.csv")

df_out %>%
  mutate(Both = YLDs + YLLs) %>%
  group_by(code) %>%
  mutate(YLDs = YLDs/sum(YLDs), YLLs = YLLs/sum(YLLs), Both = Both/sum(Both)) %>% 
  ungroup() %>%
  filter(cluster == "Senescent") %>%
  write_csv("andrew_YLL_YLD_countries_senescent_only.csv")


# How many countries have senescent disease as the biggest burden in terms of a) YLD b) YLL c) YLL+YLD
df_out %>%
  mutate(Both = YLDs + YLLs) %>%
  arrange(code, -Both) %>%
  group_by(code) %>%
  summarise(cluster = first(cluster)) %>%
  tabyl(cluster)

df_out %>%
  mutate(ratio = YLDs/YLLs) %>%
  ggplot() + geom_histogram(aes(x = ratio))

df %>%
  right_join(cluster4_df) %>%
  group_by(cluster_no, cluster, location_name, measure_name, metric_name, year) %>%
  summarise(val = sum(val), upper = sum(upper), lower = sum(lower)) %>%
  mutate(measure = str_sub(measure_name, 1, 4),
         location_name = str_remove_all(location_name, "World Bank ")) %>%
  ggplot(aes(x = location_name, y = val)) + facet_wrap(~measure) + 
  geom_bar(aes(fill = cluster), stat = "identity", position = "stack")
  