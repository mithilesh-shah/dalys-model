setwd("/Users/julianashwin/Documents/GitHub/dalys-model/")
rm(list=ls())


library(tidyverse)
library(readxl)
library(janitor)

"
Clean GBD data
"
GBD_files <- dir("/Users/julianashwin/Documents/Research/DALYs/data/GBD_raw")
gbd_df_all <- tibble()
file <- GBD_files[85]
for (file in GBD_files){
  print(file)
  df_in <- read_csv(str_c("/Users/julianashwin/Documents/Research/DALYs/data/GBD_raw/", file, "/", file, ".csv")) %>%
    dplyr::select(location_name, cause_name, age_name, sex_name, measure_name, metric_name, val, upper, lower) %>%
    filter(!str_detect(cause_name, "Total")) %>%
    mutate(measure_name = str_replace(measure_name, "YLDs \\(Years Lived with Disability\\)", "YLDs")) %>%
    mutate(measure_name = str_replace(measure_name, "YLLs \\(Years of Life Lost\\)", "YLLs")) %>%
    mutate(measure_name = str_replace(measure_name, "DALYs \\(Disability-Adjusted Life Years\\)", "DALYs")) %>%
    mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
    mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years"))
  # Append
  gbd_df_all <- rbind(gbd_df_all, df_in)
}
gbd_df_all %>%
  arrange(location_name, cause_name, measure_name, metric_name, age_name, sex_name) %>%
  saveRDS("/Users/julianashwin/Documents/Research/DALYs/data/gbd_data_all.rds")
# Save names of all countries in GBD data
gbd_df_all %>%
  distinct(location_name) %>%
  saveRDS("raw_data/GBD/gbd_data_countries.rds")

# Save deaths data
gbd_df_all %>%
  filter(measure_name == "Deaths") %>%
  filter(sex_name == "Both") %>%
  saveRDS("raw_data/GBD/gbd_data_deaths.rds")
# Save YLDs data
gbd_df_all %>%
  filter(measure_name == "YLDs") %>%
  filter(sex_name == "Both") %>%
  saveRDS("raw_data/GBD/gbd_data_ylds.rds")
# Save YLLs data
gbd_df_all %>%
  filter(measure_name == "YLLs") %>%
  filter(sex_name == "Both") %>%
  saveRDS("raw_data/GBD/gbd_data_ylls.rds")
# Save DALYs data
gbd_df_all %>%
  filter(measure_name == "DALYS") %>%
  filter(sex_name == "Both") %>%
  saveRDS("raw_data/GBD/gbd_data_dalys.rds")
# Save Incidence data
gbd_df_all %>%
  filter(measure_name == "Incidence") %>%
  filter(sex_name == "Both") %>%
  saveRDS("raw_data/GBD/gbd_data_incidence.rds")

# Import the gbd_countries from saved file
gbd_countries <- readRDS("raw_data/GBD/gbd_data_countries.rds")


"
Clean cluster membership data
"
cluster3_df <- read_xlsx("raw_data/clusters/kmeans_clusters_3.xlsx") %>%
  select(-`...1`) %>%
  filter(!str_detect(cause_name, "Total")) %>%
  pivot_longer(cols = c(-cause_name, -cluster_no), names_to = "age_name", values_to = "incidence") %>%
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age = as.numeric(str_sub(age_name, 1, str_locate(age_name, "-")[,"start"]-1))) %>%
  mutate(cluster_3 = case_when(cluster_no == 0  ~ "Age-related",
                             cluster_no == 1  ~ "Adult",
                             cluster_no == 2  ~ "Infant")) %>%
  dplyr::select(cause_name, age, age_name, incidence, cluster_3)

cluster4_df <- read_xlsx("raw_data/clusters/kmeans_clusters_4.xlsx") %>%
  select(-`...1`) %>%
  filter(!str_detect(cause_name, "Total")) %>%
  pivot_longer(cols = c(-cause_name, -cluster_no), names_to = "age_name", values_to = "incidence") %>%
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age = as.numeric(str_sub(age_name, 1, str_locate(age_name, "-")[,"start"]-1))) %>%
  mutate(cluster_4 = case_when(cluster_no == 0  ~ "Adult (late)",
                             cluster_no == 1  ~ "Adult (early)",
                             cluster_no == 2  ~ "Infant",
                             cluster_no == 3  ~ "Age-related")) %>%
  dplyr::select(cause_name, age, cluster_4)

clusters_df <- cluster3_df %>%
  left_join(cluster4_df)

clusters_df %>%
  write_csv("clean_data/cluster_membership_data.csv")

"
WDI data on income classification and other indicators
"
classification_data <- read_xlsx("raw_data/econ/WDIEXCEL.xlsx", sheet = "Country") %>%
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
                                   TRUE ~ name)) %>%
  filter(location_name %in% as.vector(gbd_countries$location_name)) %>%
  dplyr::select(location_name, income_group)


gbd_countries$location_name[gbd_countries$location_name %in% unique(classification_data$location_name)]
gbd_countries$location_name[!(gbd_countries$location_name %in% unique(classification_data$location_name))]

pop_df_in <- read_xlsx("raw_data/econ/WDIEXCEL.xlsx", sheet = "Data")

macro_df <- pop_df_in %>%
  filter(`Indicator Code` %in% c("NY.GNP.PCAP.CD", "SP.POP.TOTL")) %>%
  select(-`Indicator Name`) %>%
  rename(name = `Country Name`, code = `Country Code`, ind_code = `Indicator Code`) %>%
  pivot_longer(cols = -c(name, code, ind_code), names_to = "year", values_to = "val") %>% 
  mutate(series = case_when(ind_code == "SP.POP.TOTL"  ~ "WDI_population",
                            ind_code == "NY.GNP.PCAP.CD" ~ "GNI_pc")) %>%
  pivot_wider(id_cols = c(name, code, year), names_from = series, values_from = val) %>%
  mutate(time = as.numeric(as.factor(year))) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(location = case_when(name == "World"  ~ "Global",
                              name == "High income" ~ "World Bank High Income",
                              name == "Upper middle income" ~ "World Bank Upper Middle Income",
                              name == "Lower middle income" ~ "World Bank Lower Middle Income",
                              name == "Low income" ~ "World Bank Low Income",
                              code %in% country_codes ~ "Country",
                              TRUE ~ "Drop")) %>%
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
                                   TRUE ~ name)) %>%
  filter(location == "Country" & year <= 2022) %>% 
  select(location_name, year, GNI_pc, WDI_population)  %>%
  left_join(classification_data, by = c("location_name")) %>%
  filter(!is.na(income_group)) %>%
  filter(location_name %in% gbd_countries$location_name)


macro_df %>%
  write_csv("clean_data/country_wdi_data.csv")

"
WPP life tables and population
"
## Import and clean population data 
pop_df <- read_xlsx("raw_data/WPP/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES_2019.xlsx", skip = 16) %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>%
  filter(!is.na(year), !is.na(location)) %>%
  filter(type == "Country/Area") %>%
  dplyr::select(c(location, year, `0`:`100+`)) %>% 
  pivot_longer(cols = -c(location, year), names_to = "age", values_to = "population") %>%
  mutate(population = population*1e3) %>%
  mutate(location_name = case_when(location == "State of Palestine" ~ "Palestine", 
                              location == "Türkiye" ~ "Turkey", 
                              location == "China, Taiwan Province of China" ~ "Taiwan (Province of China)", 
                              location == "Dem. People's Republic of Korea" ~ "Democratic People's Republic of Korea",
                              location == "Micronesia (Fed. States of)" ~ "Micronesia (Federated States of)",
                              TRUE ~ location)) %>%
  filter(location_name %in% gbd_countries$location_name) %>%
  mutate(age = as.numeric(str_replace_all(age, "100\\+", "100"))) %>%
  dplyr::select(location_name, year, age, population)

gbd_countries$location_name[gbd_countries$location_name %in% unique(pop_df$location_name)]
gbd_countries$location_name[!(gbd_countries$location_name %in% unique(pop_df$location_name))]
unique(pop_df$location_name)[!(unique(pop_df$location_name) %in% gbd_countries$location_name)]
## Import and clean life tables
lifetab_df <- read_xlsx("raw_data/WPP/WPP2022_MORT_F06_1_SINGLE_AGE_LIFE_TABLE_ESTIMATES_BOTH_SEXES_2019.xlsx", skip = 16) %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year, age = `Age (x)`, 
         mortality = `Central death rate m(x,n)`,
         survival = `Number of survivors l(x)`,
         remaining_le = `Expectation of life e(x)`) %>%
  filter(!is.na(year), !is.na(location)) %>%
  filter(type == "Country/Area") %>%
  dplyr::select(location, year, age, mortality, survival, remaining_le) %>%
  mutate(survival = survival/1e5) %>%
  mutate(location_name = case_when(location == "State of Palestine" ~ "Palestine", 
                              location == "Türkiye" ~ "Turkey", 
                              location == "China, Taiwan Province of China" ~ "Taiwan (Province of China)", 
                              location == "Dem. People's Republic of Korea" ~ "Democratic People's Republic of Korea",
                              location == "Micronesia (Fed. States of)" ~ "Micronesia (Federated States of)",
                              TRUE ~ location)) %>%
  filter(location_name %in% gbd_countries$location_name) %>%
  dplyr::select(location_name, year, age, mortality, survival, remaining_le)


pop_df %>%
  left_join(lifetab_df) %>%
  ggplot() + theme_bw() + facet_wrap(~year) +
  geom_line(aes(x = age, y = survival, group = location_name, alpha = population))
  
pop_df %>%
  left_join(lifetab_df) %>%
  write_csv("clean_data/country_lifetab_data.csv")


"
WPP Fertility rates
"
## Fertility estimates
fertility_est_df <- read_xlsx("raw_data/WPP/WPP2022_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx",
                              sheet = "Estimates", skip = 16) %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>%
  filter(year >= 2019, type == "Country/Area") %>%
  dplyr::select(c(location, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location, -type), as.numeric) %>%
  pivot_longer(cols = -c(location, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>% filter(!is.na(fertility)) %>%
  rename(fertility_est = fertility) %>%
  mutate(location = case_when(location == "State of Palestine" ~ "Palestine", 
                              location == "Türkiye" ~ "Turkey", 
                              location == "China, Taiwan Province of China" ~ "Taiwan (Province of China)", 
                              location == "Dem. People's Republic of Korea" ~ "Democratic People's Republic of Korea",
                              location == "Micronesia (Fed. States of)" ~ "Micronesia (Federated States of)",
                              TRUE ~ location)) %>%
  filter(location %in% gbd_countries$location_name) %>%
  full_join(crossing(year = 2021:2100, age = 15:49, location = gbd_countries$location_name),
            by = c("location", "year", "age")) %>%
  select(-type) %>% arrange(location, age, year) %>%
  fill(fertility_est, .direction = "down") %>%
  arrange(location, year, age)
## Low fertility projections
fertility_low_df <- as.data.frame(read_xlsx("raw_data/WPP/WPP2022_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx",
                                            sheet = "Low variant", skip = 16)) %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>% 
  filter(year >= 2019, type == "Country/Area") %>%
  select(c(location, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location, -type), as.numeric) %>%
  pivot_longer(cols = -c(location, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>% filter(!is.na(fertility)) %>%
  rename(fertility_low = fertility) %>%
  mutate(location = case_when(location == "State of Palestine" ~ "Palestine", 
                              location == "Türkiye" ~ "Turkey", 
                              location == "China, Taiwan Province of China" ~ "Taiwan (Province of China)", 
                              location == "Dem. People's Republic of Korea" ~ "Democratic People's Republic of Korea",
                              location == "Micronesia (Fed. States of)" ~ "Micronesia (Federated States of)",
                              TRUE ~ location)) %>%
  filter(location %in% gbd_countries$location_name) %>%
  select(-type) %>% arrange(location, year, age)

## Medium fertility projections
fertility_med_df <- as.data.frame(read_xlsx("raw_data/WPP/WPP2022_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx",
                                            sheet = "Medium variant", skip = 16)) %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>% 
  filter(year >= 2019, type == "Country/Area") %>%
  select(c(location, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location, -type), as.numeric) %>%
  pivot_longer(cols = -c(location, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>% filter(!is.na(fertility)) %>%
  rename(fertility_med = fertility) %>%
  mutate(location = case_when(location == "State of Palestine" ~ "Palestine", 
                              location == "Türkiye" ~ "Turkey", 
                              location == "China, Taiwan Province of China" ~ "Taiwan (Province of China)", 
                              location == "Dem. People's Republic of Korea" ~ "Democratic People's Republic of Korea",
                              location == "Micronesia (Fed. States of)" ~ "Micronesia (Federated States of)",
                              TRUE ~ location)) %>%
  filter(location %in% gbd_countries$location_name) %>%
  select(-type) %>% arrange(location, year, age)
## High fertility projections
fertility_high_df <- as.data.frame(read_xlsx("raw_data/WPP/WPP2022_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx",
                                             sheet = "High variant", skip = 16)) %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>% 
  filter(year >= 2019, type == "Country/Area") %>%
  select(c(location, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location, -type), as.numeric) %>%
  pivot_longer(cols = -c(location, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>% filter(!is.na(fertility)) %>%
  rename(fertility_high = fertility) %>%
  mutate(location = case_when(location == "State of Palestine" ~ "Palestine", 
                              location == "Türkiye" ~ "Turkey", 
                              location == "China, Taiwan Province of China" ~ "Taiwan (Province of China)", 
                              location == "Dem. People's Republic of Korea" ~ "Democratic People's Republic of Korea",
                              location == "Micronesia (Fed. States of)" ~ "Micronesia (Federated States of)",
                              TRUE ~ location)) %>%
  filter(location %in% gbd_countries$location_name) %>%
  select(-type) %>% arrange(location, year, age)
## Merge fertility together
fertility_df <- fertility_est_df %>%
  full_join(fertility_low_df) %>%
  full_join(fertility_med_df) %>%
  full_join(fertility_high_df) %>%
  arrange(location, year, age) %>%
  mutate(fertility_low = case_when(year <= 2021 ~ fertility_est, year > 2021 ~ fertility_low)) %>%
  mutate(fertility_med = case_when(year <= 2021 ~ fertility_est, year > 2021 ~ fertility_med)) %>%
  mutate(fertility_high = case_when(year <= 2021 ~ fertility_est, year > 2021 ~ fertility_high))
fertility_df %>%
  write_csv("clean_data/fertility_data.csv")






"
End of script
"