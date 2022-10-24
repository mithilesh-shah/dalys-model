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
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>%
  filter(!is.na(year)) %>%
  select(c(location, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location, -type), as.numeric) %>%
  pivot_longer(cols = -c(location, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>%
  filter(!is.na(fertility)) %>%
  mutate(location = case_when(location == "WORLD"  ~ "Global",
                              location == "High-income countries" ~ "World Bank High Income",
                              location == "Upper-middle-income countries" ~ "World Bank Upper Middle Income",
                              location == "Lower-middle-income countries" ~ "World Bank Lower Middle Income",
                              location == "Low-income countries" ~ "World Bank Low Income",
                              TRUE ~ location))
# Note that we divide by 2000 here to get the rate per person (rather than per female)

ggplot(filter(fertility_df, location == "Global")) + theme_bw() + xlab("Age") + ylab("Fertility rate") +
  geom_line(aes(x = age, y = fertility, group = as.factor(year), color = year))

write.csv(fertility_df, "clean_data/fertility_rates.csv", row.names = F)

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

gbd_new_df <- read.csv("raw_data/IHME-GBD_2019_DATA_WB_income_region.csv", stringsAsFactors = F)

gbd_df <- filter(gbd_new_df, measure_id %in% c(1,3)) %>%
  mutate(measure_name = str_replace(measure_name, "YLDs \\(Years Lived with Disability\\)", "YLDs")) %>%
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age_low = as.numeric(str_sub(age_name, 1, str_locate(age_name, "-")[,"start"]-1))) %>%
  select(measure_name, location_name, sex_name, age_name, age_low, cause_name, cause_id, val, upper, lower) %>%
  full_join(cluster_key, by = "cause_name") %>%
  filter(!str_detect(cause_name, "Total"))
  

"
GBD disease hierarchy
"
gbd_hierarchy_df <- as.data.frame(read_xlsx("raw_data/IHME_GBD_2019_HIERARCHIES.xlsx", 
                                            sheet = "Cause Hierarchy"))
names(gbd_hierarchy_df) <- str_replace_all(tolower(names(gbd_hierarchy_df)), " ", "_")

cause_tree_df <- gbd_df %>% distinct(cause_name, cause_id) %>%
  left_join(select(gbd_hierarchy_df, c(cause_id, cause_outline)))

cause_tree_df[,c("level1_outline",  "level2_outline", "level3_outline", "level4_outline")] <- 
  str_split_fixed(cause_tree_df$cause_outline,"\\.", 4)
cause_tree_df$level2_outline<- paste(cause_tree_df$level1_outline, cause_tree_df$level2_outline, sep = ".")
cause_tree_df$level3_outline<- paste(cause_tree_df$level2_outline, cause_tree_df$level3_outline, sep = ".")
cause_tree_df[,c("level1_id",  "level1_name", "level2_id", "level2_name")] <- NA


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
cause_tree_df <- select(cause_tree_df, c(cause_name, cause_id, level1_name, level2_name, level3_name))


# Parent causes
parent_causes <- unique(gbd_hierarchy_df[
  which(gbd_hierarchy_df$cause_id %in% cause_tree_df$cause_id), 
  c("cause_name", "cause_id", "parent_name", "parent_id")])
parent_causes <- rename(parent_causes, cause_name2 = cause_name)
grandparent_causes <- unique(gbd_hierarchy_df[
  which(gbd_hierarchy_df$cause_id %in% parent_causes$parent_id), 
  c("cause_name", "cause_id", "parent_name", "parent_id")])
grandparent_causes <- rename(grandparent_causes, grandparent_name = parent_name, grandparent_id = parent_id,
                             parent_name = cause_name, parent_id = cause_id)
greatgrandparent_causes <- unique(gbd_hierarchy_df[
  which(gbd_hierarchy_df$cause_id %in% grandparent_causes$grandparent_id), 
  c("cause_name", "cause_id", "parent_name", "parent_id")])
greatgrandparent_causes <- rename(greatgrandparent_causes, cause_name2 = parent_name, cause_id = parent_id)


cause_tree_df <- data.frame(cause_name = all_causes)




cause_tree_df$cause_name[which(is.na(cause_tree_df$parent_name))]




gbd_hierarchy_df$grandparent_name <- gbd_hierarchy_df$parent_name
gbd_hierarchy_df$parent_name <- gbd_hierarchy_df$cause_name
cause_tree_df <- merge(cause_tree_df, gbd_hierarchy_df[,c("parent_name", "grandparent_name")], 
                       by = "parent_name")
cause_tree_df <- cause_tree_df[,c("cause_name", "parent_name", "grandparent_name")]


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
ggplot(mortality_med_df, aes(x = age)) + theme_bw() + labs(fill="Cluster") +
  scale_fill_manual("Cluster", values = cluster_colors) +
  geom_bar(aes(y = (mortality_infant+mortality_adult+mortality_senescent)/100000, 
               fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (mortality_infant+mortality_adult)/100000, fill = "Adult"), stat = "identity") +
  geom_bar(aes(y = mortality_infant/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = mortality/100000)) + facet_wrap(.~location) +
  xlab("Age") + ylab("Mortality")
# Save 
write.csv(mortality_med_df, "clean_data/mortality_medium.csv", row.names = F)

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
  geom_line(aes(y = mortality/100000)) + facet_wrap(.~location) +
  xlab("Age") + ylab("Mortality")
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
  geom_line(aes(y = mortality/100000)) + facet_wrap(.~location) +
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
  mutate(disability = sum(c_across(`Conduct disorder`:`Attention-deficit/hyperactivity disorder`))) %>%
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
ggplot(health_med_df, aes(x = age)) + theme_bw() + labs(fill="Cluster") +
  scale_fill_manual("Cluster", values = cluster_colors) +
  geom_bar(aes(y = (disability_infant+disability_adult+disability_senescent)/100000, 
               fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (disability_infant+disability_adult)/100000, fill = "Adult"), stat = "identity") +
  geom_bar(aes(y = disability_infant/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = disability/100000)) + facet_wrap(.~location) +
  xlab("Age") + ylab("Disability")
# Save
write.csv(health_med_df, "clean_data/disability_medium.csv", row.names = F)

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
  geom_line(aes(y = disability/100000)) + facet_wrap(.~location) +
  xlab("Age") + ylab("Disability")
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
  geom_line(aes(y = disability/100000)) + facet_wrap(.~location) +
  xlab("Age") + ylab("Disability")
# Save
write.csv(health_up_df, "clean_data/disability_high.csv", row.names = F)




"
Plot global mortality and disability
"
mortality_plt <- ggplot(filter(mortality_med_df, location == "Global"), aes(x = age)) + 
  theme_bw() + labs(fill="Cluster") + scale_fill_manual("Cluster", values = cluster_colors) +
  geom_bar(aes(y = (mortality_infant+mortality_adult+mortality_senescent)/100000, 
               fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (mortality_infant+mortality_adult)/100000, fill = "Adult"), stat = "identity") +
  geom_bar(aes(y = mortality_infant/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = mortality/100000)) + facet_wrap(.~location) +
  xlab("Age") + ylab("Mortality")
health_plt <- ggplot(filter(health_med_df, location == "Global"), aes(x = age)) + 
  theme_bw() + labs(fill="Cluster") + scale_fill_manual("Cluster", values = cluster_colors) +
  geom_bar(aes(y = (disability_infant+disability_adult+disability_senescent)/100000, 
               fill = "Senescent"), stat = "identity") +
  geom_bar(aes(y = (disability_infant+disability_adult)/100000, fill = "Adult"), stat = "identity") +
  geom_bar(aes(y = disability_infant/100000, fill = "Infant"), stat = "identity") + 
  geom_line(aes(y = disability/100000)) + facet_wrap(.~location) +
  xlab("Age") + ylab("Disability")
ggarrange(mortality_plt, health_plt, ncol = 2, common.legend = T, legend = "right")
ggsave("figures/mortality_disability_by_cluster.pdf", width = 8, height = 3)



