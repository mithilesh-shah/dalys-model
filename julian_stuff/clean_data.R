setwd("/Users/julianashwin/Documents/GitHub/dalys-model/julian_stuff")
rm(list=ls())

require(ggplot2)
require(ggpubr)
require(readxl)
require(dplyr)
require(stringr)
require(reshape2)

"
Clean fertility data
"
fertility_df <- as.data.frame(read_xlsx("raw_data/WPP2022_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx",
                                        sheet = "Estimates", skip = 16))
fertility_df <- fertility_df %>%
  rename(region = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>%
  select(c(region, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-region, -type), as.numeric) %>%
  pivot_longer(cols = -c(region, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age))
# Note that we divide by 2000 here to get the rate per person (rather than per female)

ggplot(filter(fertility_df, region == "WORLD")) + theme_bw() + xlab("Age") + ylab("Fertility rate") +
  geom_line(aes(x = age, y = fertility, group = as.factor(year), color = year))

write.csv(fertility_df, "clean_data/fertility_rates.csv", row.names = F)

"
Clean population data
"
population_df <- as.data.frame(read_xlsx("raw_data/WPP2022_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx",
                                        sheet = "Estimates", skip = 16))

population_df <- population_df %>%
  rename(region = `Region, subregion, country or area *`, 
         type = Type, year = Year, `100` = `100+`) %>%
  select(c(region, type, year, 
           `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, 
           `10`, `11`, `12`, `13`, `14`, `15`, `16`, `17`, `18`, `19`, 
           `20`, `21`, `22`, `23`, `24`, `25`, `26`, `27`, `28`, `29`, 
           `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`,
           `50`, `51`, `52`, `53`, `54`, `55`, `56`, `57`, `58`, `59`,
           `60`, `61`, `62`, `63`, `64`, `65`, `66`, `67`, `68`, `69`,
           `70`, `71`, `72`, `73`, `74`, `75`, `76`, `77`, `78`, `79`,
           `80`, `81`, `82`, `83`, `84`, `85`, `86`, `87`, `88`, `89`,
           `90`, `91`, `92`, `93`, `94`, `95`, `96`, `97`, `98`, `99`, `100`)) %>%
  mutate_at(vars(-region, -type), as.numeric) %>%
  pivot_longer(cols = -c(region, type, year), names_to = "age", values_to = "population") %>%
  mutate(age = as.numeric(age)) %>% 
  mutate(population = population*1000)

ggplot(filter(population_df, region == "WORLD")) + theme_bw() + xlab("Age") + ylab("Population") +
  geom_line(aes(x = age, y = population, group = as.factor(year), color = year))

write.csv(population_df, "clean_data/population.csv", row.names = F)


"
K-means cluster membership
"
cluster3_df <- as.data.frame(read_xlsx("raw_data/kmeans_clusters_3.xlsx")) %>%
  select(-`...1`) %>%
  pivot_longer(cols = c(-cause_name, -cluster_no), names_to = "age_name", values_to = "incidence") %>%
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age = as.numeric(str_sub(age_name, 1, str_locate(age_name, "-")[,"start"]-1))) %>%
  mutate(cluster = case_when(cluster_no == 0  ~ "Senescent",
                             cluster_no == 1  ~ "Adult",
                             cluster_no == 2  ~ "Infant"))

cluster_key <- unique(select(cluster3_df, c(cause_name, cluster)))
infant_names <- filter(cluster_key, cluster == "Infant") 
adult_names <- filter(cluster_key, cluster == "Adult") 
senescent_names <- filter(cluster_key, cluster == "Senescent")
cluster_colors <- c("Senescent" = "firebrick1", "Adult" = "blue3", "Infant" = "forestgreen")

ggplot(cluster3_df, aes(x = age, y = incidence)) + theme_bw() + 
  geom_line(aes(group = cause_name, color = cluster), alpha = 0.2, size = 0.2) +
  geom_line(aes(color = cluster), stat = "summary", fun = "mean", size = 0.5) + 
  scale_color_manual("Cluster", values = cluster_colors) + 
  xlab("Age") + ylab("Incidence (standardised)") + 
  scale_y_continuous(limits = c(-2.5,4.5), expand = c(0, 0)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
ggsave("figures/clusters3.pdf", width = 6, height = 3)



            
    


"
Clean GBD data
"
gbd_df <- as.data.frame(read_xlsx("raw_data/gbd_global.xlsx"))

gbd_df <- filter(gbd_df, measure_id %in% c(1,3)) %>%
  mutate(measure_name = str_replace(measure_name, "YLDs \\(Years Lived with Disability\\)", "YLDs")) %>%
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age_low = as.numeric(str_sub(age_name, 1, str_locate(age_name, "-")[,"start"]-1))) %>%
  select(measure_name, location_name, sex_name, age_name, age_low, cause_name, val, upper, lower) %>%
  inner_join(cluster_key, by = "cause_name")

"
GBD disease hierarchy
"
gbd_hierarchy_df <- as.data.frame(read_xlsx("raw_data/IHME_GBD_2019_HIERARCHIES.xlsx", 
                                  sheet = "Cause Hierarchy"))

"
Mortality for medium, lower and upper variants
"
locations = unique(gbd_df$location_name)
ages = 0:100
fill_df <- data.frame(location_name = rep(locations, length(ages)), 
                      age_low = rep(ages, length(locations)))
## Medium
mortality_med_df <- filter(gbd_df, measure_name == "Deaths") %>%
  select(-measure_name, -upper, -lower) %>%
  pivot_wider(id_cols = c(location_name, sex_name, age_name, age_low), names_from = cause_name, 
              values_from = val, values_fill = 0) %>%
  arrange(location_name, age_low) %>% rowwise() %>%
  mutate(mortality = sum(c_across(`Larynx cancer`:`Other malignant neoplasms`))) %>%
  relocate(mortality, .after = age_low) %>%
  full_join(fill_df, by = c("location_name","age_low")) %>% ungroup() %>%
  arrange(location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")
# Sum by cluster 
mortality_med_df$mortality_infant <- rowSums(
  mortality_med_df[,which(names(mortality_med_df) %in% infant_names$cause_name)])
mortality_med_df$mortality_adult <- rowSums(
  mortality_med_df[,which(names(mortality_med_df) %in% adult_names$cause_name)])
mortality_med_df$mortality_senescent <- rowSums(
  mortality_med_df[,which(names(mortality_med_df) %in% senescent_names$cause_name)])
mortality_med_df <- mortality_med_df %>%
  relocate(c(mortality_infant, mortality_adult, mortality_senescent), .after = mortality)
# Plot mortality due to each cluster
mortality_plt <- ggplot(mortality_med_df, aes(x = age)) + theme_bw() + labs(fill="Cluster") +
  scale_fill_manual("Cluster", values = cluster_colors) +
  geom_bar(aes(y = (mortality_infant+mortality_adult+mortality_senescent)/100000, 
               fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (mortality_infant+mortality_adult)/100000, fill = "Adult"), stat = "identity") +
  geom_bar(aes(y = mortality_infant/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = mortality/100000)) +
  xlab("Age") + ylab("Mortality")
mortality_plt
# Save 
write.csv(mortality_med_df, "clean_data/mortality_medium.csv", row.names = F)

## Lower
mortality_low_df <- filter(gbd_df, measure_name == "Deaths") %>%
  select(-measure_name, -upper, -val) %>%
  pivot_wider(id_cols = c(location_name, sex_name, age_name, age_low), names_from = cause_name, 
              values_from = lower, values_fill = 0) %>%
  arrange(location_name, age_low) %>% rowwise() %>%
  mutate(mortality = sum(c_across(`Larynx cancer`:`Other malignant neoplasms`))) %>%
  relocate(mortality, .after = age_low)  %>%
  full_join(fill_df, by = c("location_name","age_low")) %>% ungroup() %>%
  arrange(location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")
# Sum by cluster 
mortality_low_df$mortality_infant <- rowSums(
  mortality_low_df[,which(names(mortality_low_df) %in% infant_names$cause_name)])
mortality_low_df$mortality_adult <- rowSums(
  mortality_low_df[,which(names(mortality_low_df) %in% adult_names$cause_name)])
mortality_low_df$mortality_senescent <- rowSums(
  mortality_low_df[,which(names(mortality_low_df) %in% senescent_names$cause_name)])
mortality_low_df <- mortality_low_df %>%
  relocate(c(mortality_infant, mortality_adult, mortality_senescent), .after = mortality)
# Plot mortality due to each cluster
ggplot(mortality_low_df, aes(x = age)) + theme_bw() + labs(fill="Cluster") +
  scale_fill_manual("Cluster", values = cluster_colors) +
  geom_bar(aes(y = (mortality_infant+mortality_adult+mortality_senescent)/100000, 
               fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (mortality_infant+mortality_adult)/100000, fill = "Adult"), stat = "identity") +
  geom_bar(aes(y = mortality_infant/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = mortality/100000)) +
  xlab("Age") + ylab("Mortality")
# Save
write.csv(mortality_low_df, "clean_data/mortality_low.csv", row.names = F)

## Upper
mortality_up_df <- filter(gbd_df, measure_name == "Deaths") %>%
  select(-measure_name, -lower, -val) %>%
  pivot_wider(id_cols = c(location_name, sex_name, age_name, age_low), names_from = cause_name, 
              values_from = upper, values_fill = 0) %>%
  arrange(location_name, age_low) %>% rowwise() %>%
  mutate(mortality = sum(c_across(`Larynx cancer`:`Other malignant neoplasms`))) %>%
  relocate(mortality, .after = age_low) %>%
  full_join(fill_df, by = c("location_name","age_low")) %>% ungroup() %>%
  arrange(location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")
# Sum by cluster 
mortality_up_df$mortality_infant <- rowSums(
  mortality_up_df[,which(names(mortality_up_df) %in% infant_names$cause_name)])
mortality_up_df$mortality_adult <- rowSums(
  mortality_up_df[,which(names(mortality_up_df) %in% adult_names$cause_name)])
mortality_up_df$mortality_senescent <- rowSums(
  mortality_up_df[,which(names(mortality_up_df) %in% senescent_names$cause_name)])
mortality_up_df <- mortality_up_df %>%
  relocate(c(mortality_infant, mortality_adult, mortality_senescent), .after = mortality)
# Plot mortality due to each cluster
ggplot(mortality_up_df, aes(x = age)) + theme_bw() + labs(fill="Cluster") +
  scale_fill_manual("Cluster", values = cluster_colors) +
  geom_bar(aes(y = (mortality_infant+mortality_adult+mortality_senescent)/100000, 
               fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (mortality_infant+mortality_adult)/100000, fill = "Adult"), stat = "identity") +
  geom_bar(aes(y = mortality_infant/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = mortality/100000)) +
  xlab("Age") + ylab("Mortality")
# Save
write.csv(mortality_up_df, "clean_data/mortality_high.csv", row.names = F)


"
Disability for medium, lower and upper variants
"
## Medium
health_med_df <- filter(gbd_df, measure_name == "YLDs") %>%
  select(-measure_name, -upper, -lower) %>%
  pivot_wider(id_cols = c(location_name, sex_name, age_name, age_low), names_from = cause_name, 
              values_from = val, values_fill = 0) %>%
  arrange(location_name, age_low) %>% rowwise() %>%
  mutate(disability = sum(c_across(`Psoriasis`:`Other drug use disorders`))) %>%
  relocate(disability, .after = age_low) %>%
  full_join(fill_df, by = c("location_name","age_low")) %>% ungroup() %>%
  arrange(location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")
# Sum by cluster 
health_med_df$disability_infant <- rowSums(
  health_med_df[,which(names(health_med_df) %in% infant_names$cause_name)])
health_med_df$disability_adult <- rowSums(
  health_med_df[,which(names(health_med_df) %in% adult_names$cause_name)])
health_med_df$disability_senescent <- rowSums(
  health_med_df[,which(names(health_med_df) %in% senescent_names$cause_name)])
health_med_df <- health_med_df %>%
  relocate(c(disability_infant, disability_adult, disability_senescent), .after = disability)
# Plot mortality due to each cluster
health_plt <- ggplot(health_med_df, aes(x = age)) + theme_bw() + labs(fill="Cluster") +
  scale_fill_manual("Cluster", values = cluster_colors) +
  geom_bar(aes(y = (disability_infant+disability_adult+disability_senescent)/100000, 
               fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (disability_infant+disability_adult)/100000, fill = "Adult"), stat = "identity") +
  geom_bar(aes(y = disability_infant/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = disability/100000)) +
  xlab("Age") + ylab("Disability")
health_plt
# Save
write.csv(health_med_df, "clean_data/disability_medium.csv", row.names = F)

# Lower
health_low_df <- filter(gbd_df, measure_name == "YLDs") %>%
  select(-measure_name, -val, -upper) %>%
  pivot_wider(id_cols = c(location_name, sex_name, age_name, age_low), names_from = cause_name, 
              values_from = lower, values_fill = 0) %>%
  arrange(location_name, age_low) %>% rowwise() %>%
  mutate(disability = sum(c_across(`Psoriasis`:`Other drug use disorders`))) %>%
  relocate(disability, .after = age_low) %>%
  full_join(fill_df, by = c("location_name","age_low")) %>% ungroup() %>%
  arrange(location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")
# Sum by cluster 
health_low_df$disability_infant <- rowSums(
  health_low_df[,which(names(health_low_df) %in% infant_names$cause_name)])
health_low_df$disability_adult <- rowSums(
  health_low_df[,which(names(health_low_df) %in% adult_names$cause_name)])
health_low_df$disability_senescent <- rowSums(
  health_low_df[,which(names(health_low_df) %in% senescent_names$cause_name)])
health_low_df <- health_low_df %>%
  relocate(c(disability_infant, disability_adult, disability_senescent), .after = disability)
# Plot mortality due to each cluster
ggplot(health_low_df, aes(x = age)) + theme_bw() + labs(fill="Cluster") +
  scale_fill_manual("Cluster", values = cluster_colors) +
  geom_bar(aes(y = (disability_infant+disability_adult+disability_senescent)/100000, 
               fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (disability_infant+disability_adult)/100000, fill = "Adult"), stat = "identity") +
  geom_bar(aes(y = disability_infant/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = disability/100000)) +
  xlab("Age") + ylab("Disability")
# Save
write.csv(health_low_df, "clean_data/disability_low.csv", row.names = F)

# Upper
health_up_df <- filter(gbd_df, measure_name == "YLDs") %>%
  select(-measure_name, -lower, -val) %>%
  pivot_wider(id_cols = c(location_name, sex_name, age_name, age_low), names_from = cause_name, 
              values_from = upper, values_fill = 0) %>%
  arrange(location_name, age_low) %>% rowwise() %>%
  mutate(disability = sum(c_across(`Psoriasis`:`Other drug use disorders`))) %>%
  relocate(disability, .after = age_low) %>%
  full_join(fill_df, by = c("location_name","age_low")) %>% ungroup() %>%
  arrange(location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")
# Sum by cluster 
health_up_df$disability_infant <- rowSums(
  health_up_df[,which(names(health_up_df) %in% infant_names$cause_name)])
health_up_df$disability_adult <- rowSums(
  health_up_df[,which(names(health_up_df) %in% adult_names$cause_name)])
health_up_df$disability_senescent <- rowSums(
  health_up_df[,which(names(health_up_df) %in% senescent_names$cause_name)])
health_up_df <- health_up_df %>%
  relocate(c(disability_infant, disability_adult, disability_senescent), .after = disability)
# Plot mortality due to each cluster
ggplot(health_up_df, aes(x = age)) + theme_bw() + labs(fill="Cluster") +
  scale_fill_manual("Cluster", values = cluster_colors) +
  geom_bar(aes(y = (disability_infant+disability_adult+disability_senescent)/100000, 
               fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (disability_infant+disability_adult)/100000, fill = "Adult"), stat = "identity") +
  geom_bar(aes(y = disability_infant/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = disability/100000)) +
  xlab("Age") + ylab("Disability")
# Save
write.csv(health_up_df, "clean_data/disability_high.csv", row.names = F)


ggarrange(mortality_plt, health_plt, ncol = 2, common.legend = T, legend = "right")
ggsave("figures/mortality_disability_by_cluster.pdf", width = 8, height = 3)



