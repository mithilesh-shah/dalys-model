setwd("/Users/julianashwin/Documents/GitHub/dalys-model/julian_stuff")
rm(list=ls())

library(ggplot2)
library(ggpubr)
library(readxl)
library(dplyr)
library(stringr)
library(reshape2)
library(stargazer)

"
Clean fertility data
"
fertility_est_df <- read_xlsx("raw_data/WPP2022_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx",
                              sheet = "Estimates", skip = 16) %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>%
  filter(!is.na(year)) %>%
  select(c(location, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location, -type), as.numeric) %>%
  pivot_longer(cols = -c(location, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>% filter(!is.na(fertility)) %>%
  rename(fertility_est = fertility) %>%
  mutate(location = case_when(location == "WORLD"  ~ "Global",
                              location == "High-income countries" ~ "World Bank High Income",
                              location == "Upper-middle-income countries" ~ "World Bank Upper Middle Income",
                              location == "Lower-middle-income countries" ~ "World Bank Lower Middle Income",
                              location == "Low-income countries" ~ "World Bank Low Income",
                              TRUE ~ location))

locs_2021 <- unique(fertility_est_df$location[which(fertility_est_df$year == 2021)])

fertility_est_df <- fertility_est_df %>%
  full_join(crossing(year = 2021:2100, age = 15:49, location = locs_2021),
            by = c("location", "year", "age")) %>%
  select(-type) %>% arrange(location, age, year) %>%
  fill(fertility_est, .direction = "down") %>%
  arrange(location, year, age)


## Fertility projections
# Low
fertility_low_df <- as.data.frame(read_xlsx("raw_data/WPP2022_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx",
                                        sheet = "Low variant", skip = 16)) %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>% filter(!is.na(year)) %>%
  select(c(location, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location, -type), as.numeric) %>%
  pivot_longer(cols = -c(location, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>% filter(!is.na(fertility)) %>%
  rename(fertility_low = fertility) %>%
  mutate(location = case_when(location == "WORLD"  ~ "Global",
                              location == "High-income countries" ~ "World Bank High Income",
                              location == "Upper-middle-income countries" ~ "World Bank Upper Middle Income",
                              location == "Lower-middle-income countries" ~ "World Bank Lower Middle Income",
                              location == "Low-income countries" ~ "World Bank Low Income", TRUE ~ location)) %>%
  select(-type) %>% arrange(location, year, age)

# Medium
fertility_med_df <- as.data.frame(read_xlsx("raw_data/WPP2022_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx",
                                            sheet = "Medium variant", skip = 16)) %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>% filter(!is.na(year)) %>%
  select(c(location, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location, -type), as.numeric) %>%
  pivot_longer(cols = -c(location, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>% filter(!is.na(fertility)) %>%
  rename(fertility_med = fertility) %>%
  mutate(location = case_when(location == "WORLD"  ~ "Global",
                              location == "High-income countries" ~ "World Bank High Income",
                              location == "Upper-middle-income countries" ~ "World Bank Upper Middle Income",
                              location == "Lower-middle-income countries" ~ "World Bank Lower Middle Income",
                              location == "Low-income countries" ~ "World Bank Low Income", TRUE ~ location))  %>%
  select(-type) %>% arrange(location, year, age)
# High
fertility_high_df <- as.data.frame(read_xlsx("raw_data/WPP2022_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx",
                                            sheet = "High variant", skip = 16)) %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>% filter(!is.na(year)) %>%
  select(c(location, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location, -type), as.numeric) %>%
  pivot_longer(cols = -c(location, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>% filter(!is.na(fertility)) %>%
  rename(fertility_high = fertility) %>%
  mutate(location = case_when(location == "WORLD"  ~ "Global",
                              location == "High-income countries" ~ "World Bank High Income",
                              location == "Upper-middle-income countries" ~ "World Bank Upper Middle Income",
                              location == "Lower-middle-income countries" ~ "World Bank Lower Middle Income",
                              location == "Low-income countries" ~ "World Bank Low Income", TRUE ~ location))  %>%
  select(-type) %>% arrange(location, year, age)

fertility_df <- fertility_est_df %>%
  full_join(fertility_low_df) %>%
  full_join(fertility_med_df) %>%
  full_join(fertility_high_df) %>%
  arrange(location, year, age) %>%
  mutate(fertility_low = case_when(year <= 2021 ~ fertility_est, year > 2021 ~ fertility_low)) %>%
  mutate(fertility_med = case_when(year <= 2021 ~ fertility_est, year > 2021 ~ fertility_med)) %>%
  mutate(fertility_high = case_when(year <= 2021 ~ fertility_est, year > 2021 ~ fertility_high))

write.csv(fertility_df, "clean_data/fertility_rates.csv", row.names = F)


ggplot(filter(fertility_df, location == "Global"), aes(x= age, y = fertility_est)) +
  geom_line(aes(color = year, group = year))
# Note that we divide by 2000 here to get the rate per person (rather than per female)
ggplot(filter(fertility_df, location == "Global")) + theme_bw() + xlab("Age") + ylab("Fertility rate") +
  geom_line(aes(x = age, y = fertility_est, group = as.factor(year), color = year)) + 
  geom_line(aes(x = age, y = fertility_med, group = as.factor(year), color = year), linetype = "dashed")
ggplot(filter(fertility_df, location == "World Bank Low Income")) + theme_bw() + xlab("Age") + ylab("Fertility rate") +
  geom_line(aes(x = age, y = fertility_est, group = as.factor(year), color = year)) + 
  geom_line(aes(x = age, y = fertility_med, group = as.factor(year), color = year), linetype = "dashed")




"
Clean population data
"
population_df <- as.data.frame(read_xlsx("raw_data/WPP2022_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx",
                                        sheet = "Estimates", skip = 16))

population_df <- population_df %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year, `100` = `100+`) %>%
  filter(!is.na(year)) %>%
  select(c(location, type, year, 
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
  mutate_at(vars(-location, -type), as.numeric) %>%
  pivot_longer(cols = -c(location, type, year), names_to = "age", values_to = "population") %>%
  mutate(age = as.numeric(age)) %>% 
  mutate(population = population*1000) %>%
  filter(!is.na(population)) %>%
  mutate(location = case_when(location == "WORLD"  ~ "Global",
                              location == "High-income countries" ~ "World Bank High Income",
                              location == "Upper-middle-income countries" ~ "World Bank Upper Middle Income",
                              location == "Lower-middle-income countries" ~ "World Bank Lower Middle Income",
                              location == "Low-income countries" ~ "World Bank Low Income",
                              TRUE ~ location))

ggplot(filter(population_df, location == "Global")) + theme_bw() + xlab("Age") + ylab("Population") +
  geom_line(aes(x = age, y = population, group = as.factor(year), color = year))

write.csv(population_df, "clean_data/population.csv", row.names = F)


"
K-means cluster membership
"
cluster3_df <- as.data.frame(read_xlsx("raw_data/kmeans_clusters_3.xlsx")) %>%
  select(-`...1`) %>%
  filter(!str_detect(cause_name, "Total")) %>%
  pivot_longer(cols = c(-cause_name, -cluster_no), names_to = "age_name", values_to = "incidence") %>%
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age = as.numeric(str_sub(age_name, 1, str_locate(age_name, "-")[,"start"]-1))) %>%
  mutate(cluster = case_when(cluster_no == 0  ~ "Senescent",
                             cluster_no == 1  ~ "Adult",
                             cluster_no == 2  ~ "Infant"))

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
                             cluster_no == 3  ~ "Senescent"))


cluster3_key <- unique(select(cluster3_df, c(cause_name, cluster))) %>%
  rename(cluster3 = cluster)
cluster4_key <- unique(select(cluster4_df, c(cause_name, cluster))) %>%
  rename(cluster4 = cluster)
#infant_names <- filter(cluster_key, cluster == "Infant") 
#adult_names <- filter(cluster_key, cluster == "Adult") 
#senescent_names <- filter(cluster_key, cluster == "Senescent")
cluster3_colors <- c("Senescent" = "firebrick1", "Adult" = "blue3", 
                              "Infant" = "forestgreen")
ggplot(cluster3_df, aes(x = age, y = incidence)) + theme_bw() + 
  geom_line(aes(group = cause_name, color = cluster), alpha = 0.2, size = 0.2) +
  geom_line(aes(color = cluster), stat = "summary", fun = "mean", size = 0.5) + 
  scale_color_manual("Cluster", values = cluster3_colors) + 
  xlab("Age") + ylab("Incidence (standardised)") + 
  scale_y_continuous(limits = c(-2.5,4.5), expand = c(0, 0)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
ggsave("figures/clusters3.pdf", width = 6, height = 3)

cluster4_colors <- c("Senescent" = "firebrick1", "Adult_late" = "blue3", 
                     "Adult_early" = "cornflowerblue", "Infant" = "forestgreen")
ggplot(cluster4_df, aes(x = age, y = incidence)) + theme_bw() + 
  geom_line(aes(group = cause_name, color = cluster), alpha = 0.2, size = 0.2) +
  geom_line(aes(color = cluster), stat = "summary", fun = "mean", size = 0.5) + 
  scale_color_manual("Cluster", values = cluster4_colors) + 
  xlab("Age") + ylab("Incidence (standardised)") + 
  scale_y_continuous(limits = c(-2.5,4.5), expand = c(0, 0)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
ggsave("figures/clusters4.pdf", width = 6, height = 3)



"
GBD disease hierarchy
"
gbd_new_df <- read.csv("raw_data/IHME-GBD_2019_DATA_WB_income_region.csv", stringsAsFactors = F) %>%
  filter(!str_detect(cause_name, "Total"))
gbd_hierarchy_df <- as.data.frame(read_xlsx("raw_data/IHME_GBD_2019_HIERARCHIES.xlsx", 
                                            sheet = "Cause Hierarchy")) 
names(gbd_hierarchy_df) <- str_replace_all(tolower(names(gbd_hierarchy_df)), " ", "_")

cause_tree_df <- gbd_new_df %>% distinct(cause_name, cause_id) %>%
  left_join(select(gbd_hierarchy_df, c(cause_id, cause_outline)))

cause_tree_df[,c("level1_outline",  "level2_outline", "level3_outline", "level4_outline")] <- 
  str_split_fixed(cause_tree_df$cause_outline,"\\.", 4)
cause_tree_df$level2_outline<- paste(cause_tree_df$level1_outline, cause_tree_df$level2_outline, sep = ".")
cause_tree_df$level3_outline<- paste(cause_tree_df$level2_outline, cause_tree_df$level3_outline, sep = ".")
cause_tree_df[,c("level1_id",  "level1_name", "level2_id", "level2_name")] <- NA

# Loop through each cause to get the membership at each level
for (ii in 1:nrow(cause_tree_df)){
  # Level 1 cause
  level1s <- gbd_hierarchy_df[which(gbd_hierarchy_df$cause_outline == cause_tree_df$level1_outline[ii]),
                              c("cause_name", "cause_id")]
  cause_tree_df$level1_id[ii] <- level1s$cause_id
  cause_tree_df$level1_name[ii] <- level1s$cause_name
  # Level 2 cause
  level2s <- gbd_hierarchy_df[which(gbd_hierarchy_df$cause_outline == cause_tree_df$level2_outline[ii]),
                              c("cause_name", "cause_id")]
  cause_tree_df$level2_id[ii] <- level2s$cause_id
  cause_tree_df$level2_name[ii] <- level2s$cause_name
  # Level 3 cause
  level3s <- gbd_hierarchy_df[which(gbd_hierarchy_df$cause_outline == cause_tree_df$level3_outline[ii]),
                              c("cause_name", "cause_id")]
  cause_tree_df$level3_id[ii] <- level3s$cause_id
  cause_tree_df$level3_name[ii] <- level3s$cause_name
}
# Keep only necessary columns and merge with clusters
cause_tree_df <- select(cause_tree_df, c(cause_name, cause_id, level1_name, level2_name, level3_name)) %>%
  full_join(cluster3_key, by = "cause_name") %>%
  full_join(cluster4_key, by = "cause_name") %>%
  arrange(cluster3, cluster4, level1_name, level2_name, level3_name) 
write.csv(cause_tree_df, "clean_data/cause_definitions.csv", row.names = F)


"
Clean GBD data
"
gbd_df <- filter(gbd_new_df, measure_id %in% c(1,3)) %>%
  mutate(measure_name = str_replace(measure_name, "YLDs \\(Years Lived with Disability\\)", "YLDs")) %>%
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age_low = as.numeric(str_sub(age_name, 1, str_locate(age_name, "-")[,"start"]-1))) %>%
  select(measure_name, location_name, sex_name, age_name, age_low, cause_name, cause_id, val, upper, lower) %>%
  full_join(cause_tree_df, by = c("cause_name", "cause_id"))
  


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
  mutate(mortality = sum(c_across(`Larynx cancer`:`Alcoholic cardiomyopathy`))) %>%
  relocate(mortality, .after = age_low) %>%
  full_join(fill_df, by = c("location_name","age_low")) %>% ungroup() %>%
  arrange(location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")
write.csv(mortality_med_df, "clean_data/mortality_medium.csv", row.names = F)

# Sum by cluster 
mortality_med_df$mortality_infant3 <- rowSums(mortality_med_df[,which(names(mortality_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster3 == "Infant")])])
mortality_med_df$mortality_adult3 <- rowSums(mortality_med_df[,which(names(mortality_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster3 == "Adult")])])
mortality_med_df$mortality_senescent3 <- rowSums(mortality_med_df[,which(names(mortality_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster3 == "Senescent")])])
mortality_med_df$mortality_infant4 <- rowSums(mortality_med_df[,which(names(mortality_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Infant")])])
mortality_med_df$mortality_adult_early4 <- rowSums(mortality_med_df[,which(names(mortality_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_early")])])
mortality_med_df$mortality_adult_late4 <- rowSums(mortality_med_df[,which(names(mortality_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_late")])])
mortality_med_df$mortality_senescent4 <- rowSums(mortality_med_df[,which(names(mortality_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Senescent")])])
# Plot mortality due to each cluster3
mortality3_plt <- ggplot(mortality_med_df, aes(x = age)) + theme_bw() + labs(fill="Cluster") +
  scale_fill_manual("Cluster", values = cluster3_colors) +
  geom_bar(aes(y = (mortality_infant3+mortality_adult3+mortality_senescent3)/100000, 
               fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (mortality_infant3+mortality_adult3)/100000, fill = "Adult"), stat = "identity") +
  geom_bar(aes(y = mortality_infant3/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = mortality/100000)) + facet_wrap(.~location) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  xlab("Age") + ylab("Mortality")
mortality3_plt
# Plot mortality due to each cluster4
mortality4_plt <- ggplot(mortality_med_df, aes(x = age)) + theme_bw() + labs(fill="Cluster") +
  scale_fill_manual("Cluster", values = cluster4_colors) +
  geom_bar(aes(y = (mortality_infant4+mortality_adult_early4+mortality_adult_late4
                    +mortality_senescent4)/100000, fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (mortality_infant4+mortality_adult_early4+mortality_adult_late4)/100000, 
               fill = "Adult_late"), stat = "identity") +
  geom_bar(aes(y = (mortality_infant4+mortality_adult_early4)/100000, 
               fill = "Adult_early"), stat = "identity") +
  geom_bar(aes(y = mortality_infant4/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = mortality/100000)) + facet_wrap(.~location) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  xlab("Age") + ylab("Mortality")
mortality4_plt


## Lower
mortality_low_df <- filter(gbd_df, measure_name == "Deaths") %>%
  select(-measure_name, -upper, -val) %>%
  pivot_wider(id_cols = c(location_name, sex_name, age_name, age_low), names_from = cause_name, 
              values_from = lower, values_fill = 0) %>%
  arrange(location_name, age_low) %>% rowwise() %>%
  mutate(mortality = sum(c_across(`Larynx cancer`:`Alcoholic cardiomyopathy`))) %>%
  relocate(mortality, .after = age_low)  %>%
  full_join(fill_df, by = c("location_name","age_low")) %>% ungroup() %>%
  arrange(location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")
# Save
write.csv(mortality_low_df, "clean_data/mortality_low.csv", row.names = F)

## Upper
mortality_up_df <- filter(gbd_df, measure_name == "Deaths") %>%
  select(-measure_name, -lower, -val) %>%
  pivot_wider(id_cols = c(location_name, sex_name, age_name, age_low), names_from = cause_name, 
              values_from = upper, values_fill = 0) %>%
  arrange(location_name, age_low) %>% rowwise() %>%
  mutate(mortality = sum(c_across(`Larynx cancer`:`Alcoholic cardiomyopathy`))) %>%
  relocate(mortality, .after = age_low) %>%
  full_join(fill_df, by = c("location_name","age_low")) %>% ungroup() %>%
  arrange(location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")
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
  mutate(disability = sum(c_across(`Conduct disorder`:`Attention-deficit/hyperactivity disorder`))) %>%
  relocate(disability, .after = age_low) %>%
  full_join(fill_df, by = c("location_name","age_low")) %>% ungroup() %>%
  arrange(location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")
write.csv(health_med_df, "clean_data/disability_medium.csv", row.names = F)
# Sum by cluster 
health_med_df$disability_infant3 <- rowSums(health_med_df[,which(names(health_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster3 == "Infant")])])
health_med_df$disability_adult3 <- rowSums(health_med_df[,which(names(health_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster3 == "Adult")])])
health_med_df$disability_senescent3 <- rowSums(health_med_df[,which(names(health_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster3 == "Senescent")])])
health_med_df$disability_infant4 <- rowSums(health_med_df[,which(names(health_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Infant")])])
health_med_df$disability_adult_early4 <- rowSums(health_med_df[,which(names(health_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_early")])])
health_med_df$disability_adult_late4 <- rowSums(health_med_df[,which(names(health_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_late")])])
health_med_df$disability_senescent4 <- rowSums(health_med_df[,which(names(health_med_df) %in% 
    cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Senescent")])])
# Plot disability due to each cluster3
health3_plt <- ggplot(health_med_df, aes(x = age)) + theme_bw() + labs(fill="Cluster") +
  scale_fill_manual("Cluster", values = cluster3_colors) +
  geom_bar(aes(y = (disability_infant3+disability_adult3+disability_senescent3)/100000, 
               fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (disability_infant3+disability_adult3)/100000, fill = "Adult"), stat = "identity") +
  geom_bar(aes(y = disability_infant3/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = disability/100000)) + facet_wrap(.~location) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  xlab("Age") + ylab("Disability")
health3_plt
# Plot disability due to each cluster4
health4_plt <- ggplot(health_med_df, aes(x = age)) + theme_bw() + labs(fill="Cluster") +
  scale_fill_manual("Cluster", values = cluster4_colors) +
  geom_bar(aes(y = (disability_infant4+disability_adult_early4+disability_adult_late4
                    +disability_senescent4)/100000, fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (disability_infant4+disability_adult_early4+disability_adult_late4)/100000, 
               fill = "Adult_late"), stat = "identity") +
  geom_bar(aes(y = (disability_infant4+disability_adult_early4)/100000, 
               fill = "Adult_early"), stat = "identity") +
  geom_bar(aes(y = disability_infant4/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = disability/100000)) + facet_wrap(.~location) +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0)) +
  xlab("Age") + ylab("Disability")
health4_plt

# Lower
health_low_df <- filter(gbd_df, measure_name == "YLDs") %>%
  select(-measure_name, -val, -upper) %>%
  pivot_wider(id_cols = c(location_name, sex_name, age_name, age_low), names_from = cause_name, 
              values_from = lower, values_fill = 0) %>%
  arrange(location_name, age_low) %>% rowwise() %>%
  mutate(disability = sum(c_across(`Conduct disorder`:`Attention-deficit/hyperactivity disorder`))) %>%
  relocate(disability, .after = age_low) %>%
  full_join(fill_df, by = c("location_name","age_low")) %>% ungroup() %>%
  arrange(location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")
# Save
write.csv(health_low_df, "clean_data/disability_low.csv", row.names = F)

# Upper
health_up_df <- filter(gbd_df, measure_name == "YLDs") %>%
  select(-measure_name, -lower, -val) %>%
  pivot_wider(id_cols = c(location_name, sex_name, age_name, age_low), names_from = cause_name, 
              values_from = upper, values_fill = 0) %>%
  arrange(location_name, age_low) %>% rowwise() %>%
  mutate(disability = sum(c_across(`Conduct disorder`:`Attention-deficit/hyperactivity disorder`))) %>%
  relocate(disability, .after = age_low) %>%
  full_join(fill_df, by = c("location_name","age_low")) %>% ungroup() %>%
  arrange(location_name, age_low) %>% 
  rename(age = age_low, location = location_name) %>%
  fill(!age_name, .direction = "down")
# Save
write.csv(health_up_df, "clean_data/disability_high.csv", row.names = F)




"
Plot global mortality and disability
"
ggarrange(mortality3_plt, mortality4_plt, ncol = 2, common.legend = F)
ggsave("figures/mortality_by_cluster.pdf", width = 15, height = 5)
ggarrange(health3_plt, health4_plt, ncol = 2, common.legend = F, legend = "right")
ggsave("figures/disability_by_cluster.pdf", width = 15, height = 5)






"
Get GNI per capita for each country in each region
"
loc_cols <- c("High income" = "forestgreen", "Upper middle income" = "green", 
              "Lower middle income" = "orange", "Low income" = "red")

classification_data <- read_xlsx("/Users/julianashwin/Downloads/WDIEXCEL.xlsx", sheet = "Country") %>%
  filter(!is.na(`Income Group`)) %>%
  select(`Country Code`, `Table Name`, `Income Group`) %>%
  rename(name = `Table Name`, code = `Country Code`, income_group = `Income Group`)
country_codes <- unique(classification_data$code)


macro_data_in <- read_xlsx("/Users/julianashwin/Downloads/WDIEXCEL.xlsx", sheet = "Data")
  
macro_data <- macro_data_in %>%
  filter(`Indicator Code` %in% c("NY.GNP.PCAP.CD", "SP.POP.TOTL")) %>%
  select(-`Indicator Name`) %>%
  rename(name = `Country Name`, code = `Country Code`, ind_code = `Indicator Code`) %>%
  pivot_longer(cols = -c(name, code, ind_code), names_to = "year", values_to = "val") %>% 
  mutate(series = case_when(ind_code == "SP.POP.TOTL"  ~ "population",
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
  filter(location != "Drop") %>% select(location, name, code, year, time, GNI_pc, population)  %>%
  left_join(classification_data, by = c("code", "name")) %>%
  filter(!is.na(income_group) | location != "Country") %>%
  group_by(code) %>% 
  mutate(GNI_pc_1lag = lag(GNI_pc, order_by = year)) %>% 
  mutate(GNI_growth = log(GNI_pc) - log(GNI_pc_1lag)) %>%
  mutate(GNI_growth_mean = mean(GNI_growth, na.rm = T)) %>% 
  ungroup()

# Check some special cases
unique(macro_data[which(macro_data$GNI_growth_mean < 0), c("name", "income_group", "GNI_growth_mean")])
unique(macro_data[which(macro_data$location != "Country"), c("name", "GNI_growth_mean")])
unique(macro_data[which(is.na(macro_data$income_group)), c("name", "income_group")])

  
  


# Plot the distribution of average growth rates 
ggplot(unique(select(filter(macro_data, location == "Country"), code, income_group, GNI_growth_mean))) + 
  theme_bw() + scale_fill_manual("Region", values = loc_cols) +
  geom_histogram(aes(x = GNI_growth_mean, fill = income_group), position = "dodge", breaks = seq(-0.09,0.14,0.01)) + 
  labs(x = "Average GNI growth rate", y = "")
ggsave("figures/GNI_growth_histogram.pdf", width = 5, height = 3)
# Plot GNI p.c. over time in each region 
ggplot(filter(macro_data, location == "Country" & !is.na(income_group))) + 
  facet_wrap(.~ income_group) + guides(color = "none") + theme_bw() + 
  geom_line(aes(x = year, y = log(GNI_pc), group = code), alpha = 0.3) +
  geom_smooth(aes(x = year, y = log(GNI_pc)), method = "lm", se = F) +
  labs(y = "log GNI per capita", x = "Year")
ggsave("figures/GNI_pc_time.pdf", width = 6, height = 4)



gni_stats <- macro_data %>% filter(!is.na(income_group) & year == 2021) %>% 
  group_by(income_group) %>%
  summarise(min = min(GNI_pc, na.rm = T), max = max(GNI_pc, na.rm = T), 
            median = median(GNI_pc, na.rm = T), mean_grwth = mean(GNI_growth_mean, na.rm = T),
            total_pop = sum(population, na.rm = T))
gni_stats

all_names <- unique(macro_data$name[which(!is.na(macro_data$income_group) & 
                                            !is.na(macro_data$GNI_pc))])
growth_proj <- macro_data %>% select(-location, -code, -GNI_growth_mean, -GNI_growth, -GNI_pc_1lag,) %>%
  arrange(name, year) %>%
  group_by(name) %>% fill(GNI_pc, population) %>% 
  filter(!is.na(GNI_pc) & !is.na(income_group) & year >= 2021) %>%
  full_join(crossing(name = all_names, year = 2021:2200)) %>% arrange(name, year) %>%
  left_join(select(gni_stats, income_group, mean_grwth, total_pop)) %>%
  group_by(name) %>% fill(GNI_pc, mean_grwth, income_group, population, total_pop, .direction = "down") %>%
  mutate(mean_grwth = case_when(year <= 2021 ~ 0, TRUE ~ mean_grwth)) %>%
  mutate(factor = cumprod(1 + mean_grwth), pop_share = population/total_pop) %>%
  mutate(GNI_pc = GNI_pc*factor) %>%
  mutate(income_group_proj = case_when(GNI_pc < 1085 ~ "Low income",
                                       GNI_pc < 4256 ~ "Lower middle income",
                                       GNI_pc < 13206 ~ "Upper middle income",
                                       GNI_pc >= 13206 ~ "High income"))



ggplot(filter(growth_proj, year <2122)) + facet_wrap(~income_group) +
  theme_bw() + scale_fill_manual("Region", values = loc_cols) +
  geom_line(aes(x = year, y = log(GNI_pc), group = name, color = income_group_proj)) +
  geom_smooth(aes(x = year, y = log(GNI_pc)), method = "lm", se = F) +
  labs(y = "log GNI per capita", x = "Year")



income_transition_shares <- growth_proj %>%
  group_by(income_group, income_group_proj, year) %>%
  summarise(pop_share = sum(pop_share))
  
ggplot(income_transition_shares, aes(x = year)) + theme_bw() + 
  facet_wrap(vars(income_group)) + xlim(c(2021,2122)) +
  scale_fill_manual("Region", values = loc_cols) +
  scale_color_manual("Region", values = loc_cols) +
  geom_bar(aes(y = pop_share, fill = income_group_proj, color = income_group_proj), width = 1, 
           stat = "identity", position = "stack") + guides(color = "none") + 
  labs(x = "Year", y = "Projected share")
ggsave("figures/WB_region_transitions.pdf", width = 8, height = 4)


income_transition_shares <- income_transition_shares %>%
  pivot_wider(id_cols = c(income_group, year), names_from = income_group_proj, 
              values_from = pop_share, values_fill = 0)

write.csv(income_transition_shares, "clean_data/income_transition_shares.csv", row.names = F)








