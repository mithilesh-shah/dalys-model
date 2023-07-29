setwd("/Users/julianashwin/Documents/GitHub/dalys-model/julian_stuff")
rm(list=ls())


library(tidyverse)
library(readxl)
library(janitor)

cluster4_df <- as.data.frame(read_xlsx("raw_data/kmeans_clusters_4.xlsx")) %>%
  select(-`...1`) %>%
  filter(!str_detect(cause_name, "Total")) %>%
  pivot_longer(cols = c(-cause_name, -cluster_no), names_to = "age_name", values_to = "incidence") %>%
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age = as.numeric(str_sub(age_name, 1, str_locate(age_name, "-")[,"start"]-1))) %>%
  mutate(cluster = case_when(cluster_no == 0  ~ "Adult_late",
                             cluster_no == 1  ~ "Adult_early",
                             cluster_no == 2  ~ "Infant",
                             cluster_no == 3  ~ "Senescent")) %>%
  select(-age, -age_name, -incidence) %>%
  distinct()

df <- read_csv("raw_data/YLD_YLL_2019_countries.csv") %>%
  filter(metric_name == "Number") %>%
  filter(location_name != "Taiwan (Province of China)", location_name != "Venezuela (Bolivarian Republic of)",
         location_name != "Palestine", location_name != "Cook Islands", location_name != "Niue", 
         location_name != "Tokelau") %>%
  filter(cause_name != "Total cancers")


classification_data <- read_xlsx("raw_data/WDIEXCEL.xlsx", sheet = "Country") %>%
  filter(!is.na(`Income Group`)) %>%
  select(`Country Code`, `Table Name`, `Income Group`) %>%
  rename(name = `Table Name`, code = `Country Code`, income_group = `Income Group`) %>%
  mutate(income_group = case_when(income_group == "High income" ~ "World Bank High Income",
                                  income_group == "Upper middle income" ~ "World Bank Upper Middle Income",
                                  income_group == "Lower middle income" ~ "World Bank Lower Middle Income",
                                  income_group == "Low income" ~ "World Bank Low Income")) %>%
  mutate(location_name = case_when(name == "Korea, Dem. People's Rep." ~ "Democratic People's Republic of Korea",
                                   name == "Micronesia, Fed. Sts." ~ "Micronesia (Federated States of)",
                                   name == "Lao PDR" ~ "Lao People's Democratic Republic",
                                   name == "Vietnam" ~ "Viet Nam",
                                   name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                   name == "Slovak Republic" ~ "Slovakia",
                                   name == "Moldova" ~ "Republic of Moldova",
                                   name == "Korea, Rep." ~ "Republic of Korea",
                                   name == "United States" ~ "United States of America",
                                   name == "Bahamas, The" ~ "Bahamas",
                                   name == "St. Lucia" ~ "Saint Lucia", 
                                   name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines", 
                                   name == "Bolivia" ~ "Bolivia (Plurinational State of)", 
                                   name == "Egypt, Arab Rep." ~ "Egypt", 
                                   name == "Iran, Islamic Rep." ~ "Iran (Islamic Republic of)", 
                                   name == "Türkiye" ~ "Turkey", 
                                   name == "Yemen, Rep." ~ "Yemen", 
                                   name == "Congo, Rep." ~ "Congo", 
                                   name == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo", 
                                   name == "Tanzania" ~ "United Republic of Tanzania", 
                                   name == "Gambia, The" ~ "Gambia", 
                                   name == "São Tomé and Principe" ~ "Sao Tome and Principe", 
                                   name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis", 
                                   name == "Virgin Islands (U.S.)" ~ "United States Virgin Islands",
                                   TRUE ~ name))
country_codes <- unique(classification_data$code)

pop_df_in <- read_xlsx("raw_data/WDIEXCEL.xlsx", sheet = "Data")

pop_df <- pop_df_in %>%
  filter(`Indicator Code` %in% c("SP.POP.TOTL")) %>%
  select(-`Indicator Name`) %>%
  rename(name = `Country Name`, code = `Country Code`, ind_code = `Indicator Code`) %>%
  pivot_longer(cols = -c(name, code, ind_code), names_to = "year", values_to = "val") %>% 
  mutate(series = case_when(ind_code == "SP.POP.TOTL"  ~ "population")) %>%
  pivot_wider(id_cols = c(name, code, year), names_from = series, values_from = val) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(location = case_when(name == "World"  ~ "Global",
                              name == "High income" ~ "World Bank High Income",
                              name == "Upper middle income" ~ "World Bank Upper Middle Income",
                              name == "Lower middle income" ~ "World Bank Lower Middle Income",
                              name == "Low income" ~ "World Bank Low Income",
                              code %in% country_codes ~ "Country",
                              TRUE ~ "Drop")) %>%
  filter(location == "Country" & year == 2019) %>% 
  select(code, year, population)

unique(df$location_name)[!(unique(df$location_name) %in% classification_data$location_name)]



df_out <- df %>%
  inner_join(cluster4_df) %>%
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
  