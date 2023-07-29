setwd("/Users/julianashwin/Documents/GitHub/dalys-model/julian_stuff")
rm(list=ls())


library(tidyverse)
library(readxl)
library(janitor)


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