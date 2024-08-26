"
Cluster causes by incidence across the life cycle. 
"

setwd("/Users/julianashwin/Documents/GitHub/dalys-model/")
rm(list=ls())



library(tidyverse)
library(readxl)
library(janitor)
library(maotai)


age_groups <- c("0-1 years", "1-2 years", "2-4 years", "5-9 years", "10-14 years", "15-19 years", 
                "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", 
                "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", 
                "80-84 years", "85-89 years", "90-94 years", "95-99 years")


# Import data 
all_causes <- readRDS("raw_data/GBD/gbd_data_causes.rds")

global_daly_df <- readRDS("raw_data/GBD/gbd_data_dalys.rds") %>%
  filter(location_name == "Global",
         metric_name == "Rate",
         year == 2021) %>%
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "12-23 months", "1-2 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age_name = factor(age_name, levels = age_groups, ordered = T))

# Check that all causes are covered 
tabyl(all_causes$cause_name %in% unique(global_daly_df$cause_name))



## Convert to matrix for clustering
cluster_matrix <- global_daly_df %>%
  group_by(cause_name) %>%
  mutate(val_std = (val - mean(val))/sd(val)) %>%
  mutate(no_val= is.na(val_std)) %>%
  tabyl(no_val)
  pivot_wider(id_cols = cause_name, names_from = age_name, values_from = val) 


names(cluster_matrix)

clusters<- kmeans(as.matrix(cluster_matrix[,2:ncol(cluster_matrix)]), centers = 3)


global_daly_df %>%
  mutate(age = as.numeric(str_sub(age_name, 1, str_locate(age_name, "-")[,"start"]-1))) %>%
  
  ggplot(aes(x = age, y = incidence))  + theme_bw() + 
  geom_line(aes(group = cause_name), alpha = 0.2, size = 0.2)




global_df_2019 <- gbd_df_all_2019 %>%
  filter(location_name == "Global")



test_df <- global_df %>%
  filter(metric_name == "Rate") %>%
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "12-23 months", "1-2 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age_name = factor(age_name, levels = age_groups, ordered = T)) %>%
  mutate(age = as.numeric(str_sub(age_name, 1, str_locate(age_name, "-")[,"start"]-1))) %>%
  group_by(cause_name, measure_name) %>%
  mutate(val_std = (val - mean(val))/sd(val)) %>%
  pivot_wider(id_cols = c(cause_name, age_name), names_from = "measure_name", values_from = "val_std") %>%
  full_join(cluster3_df)


test_df %>%
  ggplot(aes(x = age, y = incidence))  + theme_bw() + 
  geom_line(aes(group = cause_name, color = cluster_3), alpha = 0.2, size = 0.2) +
  geom_line(aes(color = cluster_3), stat = "summary", fun = "mean", size = 0.5) + 
  scale_color_manual("Disease clusters", values = c("Ageing-related" = "firebrick1", "Adult" = "blue3", "Infant" = "forestgreen")) + 
  xlab("Age") + ylab("Incidence (standardised)") + 
  scale_y_continuous(limits = c(-2.5,4.5), expand = c(0, 0)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())


test_df %>%
  group_by(cause_name) %>%
  summarise(DALYs = cor(DALYs, incidence, use = "pairwise.complete.obs"),
            Incidence = cor(Incidence, incidence, use = "pairwise.complete.obs"),
            Prevalence = cor(Prevalence, incidence, use = "pairwise.complete.obs"),
            YLDs = cor(YLDs, incidence, use = "pairwise.complete.obs"),
            Deaths = cor(Deaths, incidence, use = "pairwise.complete.obs"),
            YLLs = cor(YLLs, incidence, use = "pairwise.complete.obs")) %>%
  pivot_longer(cols = -cause_name) %>%
  group_by(name) %>%
  summarise(value = mean(value, na.rm = T))


test_df_2019 

test_df


cause_measure <-
  global_df %>%
  filter(metric_name == "Rate") %>%
  tabyl(cause_name, measure_name)

cluster4_membership <- cluster4_df %>%
  select(cause_name, cluster_no)

x<-cause_measure %>%
  full_join(cluster4_membership)

tabyl(all_causes$cause_name %in% x$cause_name)

global_incidence_df <- readRDS("raw_data/GBD/gbd_data_incidence.rds") %>%
  filter(location_name == "Global",
         metric_name == "Rate",
         year == 2021) %>%
  crossing
  tabyl(age_name)
  mutate(age = as.numeric(str_sub(age_name, 1, str_locate(age_name, "-")[,"start"]-1))) %>%
  mutate(age_name = factor(age_name, levels = age_groups, ordered = T)) %>%
  arrange(cause_name, age_name) %>%
  group_by(cause_name) %>%
  mutate(incidence = (val - mean(val))/sd(val)) 


x<-global_incidence_df %>%
  filter(is.na(incidence))






global_incidence_df  %>%
  ggplot(aes(x = age, y = incidence))  + theme_bw() + 
  geom_line(aes(group = cause_name), alpha = 0.2, size = 0.2) +
  #geom_line(aes(color = cluster_3), stat = "summary", fun = "mean", size = 0.5) + 
  #scale_color_manual("Disease clusters", values = c("Ageing-related" = "firebrick1", "Adult" = "blue3", "Infant" = "forestgreen")) + 
  xlab("Age") + ylab("Incidence (standardised)") + 
  scale_y_continuous(limits = c(-2.5,4.5), expand = c(0, 0)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())


# Import hierarchy of causes
gbd_hierarchy_df <- read_xlsx("raw_data/GBD/IHME_GBD_2021_HIERARCHIES.xlsx", sheet = "Cause Hierarchy") %>%  
  rename_all(~tolower(str_replace_all(., "\\s+", "_"))) %>%
  right_join(distinct(gbd_incidence_df, cause_name)) %>%
  separate(cause_outline, into = c("level1_outline",  "level2_outline", "level3_outline", "level4_outline"), remove = FALSE) %>%
  mutate(level2_outline = str_c(level1_outline, ".", level2_outline),
         level3_outline = str_c(level2_outline, ".", level3_outline),
         level4_outline = cause_outline)
# Clusters with K = 3
cluster3_df <- read_xlsx("raw_data/clusters/kmeans_clusters_3.xlsx") %>%
  select(-`...1`) %>%
  filter(!str_detect(cause_name, "Total")) %>%
  pivot_longer(cols = c(-cause_name, -cluster_no), names_to = "age_name", values_to = "incidence") %>%
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age = as.numeric(str_sub(age_name, 1, str_locate(age_name, "-")[,"start"]-1))) %>%
  mutate(cluster_3 = case_when(cluster_no == 0  ~ "Ageing-related",
                               cluster_no == 1  ~ "Adult",
                               cluster_no == 2  ~ "Infant")) %>%
  dplyr::select(cause_name, age, age_name, incidence, cluster_3)
# Clusters with K = 4
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
                               cluster_no == 3  ~ "Ageing-related")) %>%
  dplyr::select(cause_name, age, cluster_4)
# Merge clusters
clusters_df <- cluster3_df %>%
  left_join(cluster4_df) %>%
  left_join(select(gbd_hierarchy_df, cause_name, cause_outline)) %>%
  left_join(global_incidence_df) %>%
  mutate(incidence_rate = replace_na(incidence_rate, 0))
# Plot incidence for K = 3
clusters_df %>%
  ggplot(aes(x = age, y = incidence))  + theme_bw() + 
  geom_line(aes(group = cause_name, color = cluster_3), alpha = 0.2, size = 0.2) +
  geom_line(aes(color = cluster_3), stat = "summary", fun = "mean", size = 0.5) + 
  scale_color_manual("Disease clusters", values = c("Ageing-related" = "firebrick1", "Adult" = "blue3", "Infant" = "forestgreen")) + 
  xlab("Age") + ylab("Incidence (standardised)") + 
  scale_y_continuous(limits = c(-2.5,4.5), expand = c(0, 0)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
ggsave("figures/clustering/clusters3.pdf", width = 6, height = 3)
# Plot incidence for K = 4
clusters_df %>%
  ggplot(aes(x = age, y = incidence)) + theme_bw() + 
  geom_line(aes(group = cause_name, color = cluster_4), alpha = 0.2, size = 0.2) +
  geom_line(aes(color = cluster_4), stat = "summary", fun = "mean", size = 0.5) + 
  scale_color_manual("Disease cluster", values = c("Ageing-related" = "firebrick1", "Adult (late)" = "blue3", 
                                                   "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")) + 
  xlab("Age") + ylab("Incidence (standardised)") + 
  scale_y_continuous(limits = c(-2.5,4.5), expand = c(0, 0)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
ggsave("figures/clustering/clusters4.pdf", width = 6, height = 3)

clusters_df %>%
  mutate(cluster_4 = factor(cluster_4, ordered = T, levels = c("Infant", "Adult (early)", "Adult (late)", "Ageing-related"))) %>%
  group_by(cause_name) %>%
  mutate(incidence_sd = sd(incidence_rate)) %>%
  ggplot(aes(x = age, y = incidence_rate/incidence_sd)) + theme_bw() + 
  geom_line(aes(group = cause_name, color = cluster_4), alpha = 0.2, size = 0.1) +
  geom_line(aes(color = cluster_4), stat = "summary", fun = "mean", size = 0.5) + 
  scale_color_manual("Disease cluster", values = c("Ageing-related" = "firebrick1", "Adult (late)" = "blue3", 
                                                   "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")) + 
  xlab("Age") + ylab("Incidence (standardised to unit sd)") + 
  coord_cartesian(ylim = c(-0.1,4.5))
ggsave("figures/clustering/clusters4_nonneg.pdf", width = 9, height = 4)



# Save 
clusters_df %>%
  write_csv("clean_data/cluster_membership_data.csv")