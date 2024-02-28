setwd("/Users/julianashwin/Documents/GitHub/dalys-model/")
rm(list=ls())

library(ggplot2)
library(ggpubr)
library(readxl)
library(tidyverse)
library(janitor)


source("functions.R")





W_scenarios <- read_rds("temp.rds")
W_scenarios <- read_rds("figures/scenarios/W_scenarios.rds")

# Find the senesecent
W_scenarios_senesc <- W_scenarios %>%
  filter(eradication == 0 ) %>%
  mutate(diseases = "senescent4", eradication = erad_senes) %>% 
  distinct()
# Remove the cross partials 
W_scenarios_nocross <- W_scenarios %>%
  ungroup() %>%
  filter(erad_senes == 0) %>%
  rbind(W_scenarios_senesc) %>%
  select(-erad_senes)
# Clean up and add global
W_scenarios_nocross <- W_scenarios_nocross %>%
  mutate(location = "Global") %>%
  group_by(start_year, no_births, growth_transitions, diseases, type, location, eradication, year) %>%
  summarise(W = sum(W), Wnewborn = sum(Wnewborn), 
            average_age = sum(average_age*population)/sum(population), LE_birth = sum(LE_birth*population)/sum(population),
            population = sum(population), pop_newborns = sum(pop_newborns)) %>%
  rbind(W_scenarios_nocross) %>%
  mutate(diseases = case_when(str_detect(diseases, "infant4") ~ "Infant",
                              str_detect(diseases, "adult_early4") ~ "Adult (early)",
                              str_detect(diseases, "adult_late4") ~ "Adult (late)",
                              str_detect(diseases, "senescent4") ~ "Ageing-related", TRUE ~ "All")) %>%
  mutate(diseases = factor(diseases, ordered = T, levels = c("Infant", "Adult (early)",  "Adult (late)", "Ageing-related")),
         location = str_remove(location, "World Bank "),
         location = factor(location, ordered = T, levels = c("Global", "Low Income",  "Lower Middle Income", "Upper Middle Income", "High Income")))

W_scenarios_nocross %>%
  group_by(start_year, no_births, growth_transitions, diseases, type, location) %>%
  mutate(W_start = sum((eradication == 0)*W),
         Wnewborn_start = sum((eradication == 0)*Wnewborn))
  


"
Some summary statistics
"
# Average age
W_scenarios_nocross %>%
  filter(location %in% c("Global", "Low Income", "High Income")) %>%
  filter(year %in% c(2021, 2046, 2071, 2096, 2121, 2146)) %>%
  filter(type == "both") %>%
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, ncol = 4) +
  geom_line(aes(y = average_age, color = year, group = year)) +
  scale_color_gradientn("Year", colours = rainbow(6)) +
  guides(fill="none") +   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Disease Eradication", y = "Average Age", color = "Year")
ggsave("figures/derivatives/average_age.pdf", width = 8, height = 6)

# LE at birth
W_scenarios_nocross %>%
  filter(year %in% c(2021, 2046, 2071, 2096, 2121, 2146)) %>%
  filter(location %in% c("Global", "Low Income", "High Income")) %>% 
  filter(type == "both") %>%
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, ncol = 4) +
  geom_line(aes(y = LE_birth, color = year, group = year)) +
  scale_color_gradientn("Year", colours = rainbow(6)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Disease Eradication", y = "LE at birth", color = "Year")
ggsave("figures/derivatives/LE_birth.pdf", width = 8, height = 6)

# Population
W_scenarios_nocross %>%
  filter(year %in% c(2021, 2046, 2071, 2096, 2121, 2146)) %>%
  filter(location %in% c("Global", "Low Income", "High Income")) %>% 
  filter(type == "both") %>%
  group_by(location, diseases) %>%
  mutate(pop_start = sum((year == 2021)*(eradication == 0)*population)) %>%
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, ncol = 4) +
  geom_line(aes(y = population/pop_start, color = year, group = year)) +
  scale_color_gradientn("Year", colours = rainbow(6)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Disease Eradication", y = "Total Population", color = "Year")
ggsave("figures/derivatives/total_population.pdf", width = 8, height = 6)




"
DALYs saved over time for full eradication
"
W_scenarios_nocross %>%
  filter(location %in% c("Global", "Low Income", "High Income")) %>% 
  filter(type == "both") %>%
  filter(eradication %in% c(0,1)) %>%
  group_by(location, diseases) %>%
  mutate(W_start = sum((year == 2021)*(eradication == 0)*W)) %>%
  group_by(diseases, location, year) %>%
  arrange(eradication) %>%
  summarise(DALYs_saved = W/lag(W)) %>% 
  na.omit() %>%
  ggplot(aes(x = year)) + theme_bw() + 
  facet_wrap(~location, ncol = 3) +
  geom_line(aes(y = DALYs_saved, color = diseases)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Year", y = "DALYs saved", color = "Diseases")
ggsave("figures/scenarios/full_erad_overtime.pdf", width = 8, height = 3)



"
Welfare and semi-elasticity
"
# Compute the welfare measures and semi-elasticities
elast_df <- W_scenarios_nocross %>%
  group_by(start_year, no_births, growth_transitions, diseases, type, location, eradication) %>%
  summarise(W = sum(W), Wnewborn = sum(Wnewborn)) %>%
  group_by(start_year, no_births, growth_transitions, diseases, type, location) %>%
  mutate(W_start = sum((eradication == 0)*W),
         Wnewborn_start = sum((eradication == 0)*Wnewborn)) %>%
  filter(start_year == 2021, no_births == FALSE, growth_transitions == TRUE) %>%
  group_by(start_year, no_births, growth_transitions, diseases, location) %>%
  group_by(diseases, location, type) %>%
  select(-c(start_year, no_births, growth_transitions)) %>%
  arrange(eradication) %>%
  mutate(erad = 1 - eradication) %>%
  mutate(W_lag = lag(W, n = 1, order_by = eradication),
         Wnewborn_lag = lag(Wnewborn, n = 1, order_by = eradication),
         erad_lag = lag(erad, n = 1, order_by = eradication),
         erad_diff = erad - erad_lag) %>%
  mutate(dW_dd = (W - lag(W, n = 1, order_by = eradication))/erad_diff,
         dWnewborn_dd = (Wnewborn - lag(Wnewborn, n = 1, order_by = eradication))/erad_diff) %>%
  mutate(d2W_dd2 = (lead(dW_dd, n=1, order_by = eradication) - dW_dd)/erad_diff^2,
         d2Wnewborn_dd2 =  (lead(dWnewborn_dd, n=1, order_by = eradication) - dWnewborn_dd)/erad_diff^2) %>%
  group_by(diseases, location, eradication) %>%
  na.omit() %>%
  mutate(W_elast_1 = dW_dd*(1/W_lag),
         Wnewborn_elast_1 = dWnewborn_dd*(1/Wnewborn_lag),
         W_elast_2 = d2W_dd2*(1/W_lag),
         Wnewborn_elast_2 = d2Wnewborn_dd2*(1/Wnewborn_lag)) %>%
  mutate(W_sum = sum((type != "both")*W) - W_start,
         W_sum = case_when(type == "both" ~ W_sum, TRUE ~ W),
         Wnewborn_sum = sum((type != "both")*Wnewborn) - Wnewborn_start,
         Wnewborn_sum = case_when(type == "both" ~ Wnewborn_sum, TRUE ~ Wnewborn),
         dW_dd_sum = sum((type != "both")*dW_dd),
         dW_dd_sum = case_when(type == "both" ~ dW_dd_sum, TRUE ~ dW_dd),
         dWnewborn_dd_sum = sum((type != "both")*dWnewborn_dd),
         dWnewborn_dd_sum = case_when(type == "both" ~ dWnewborn_dd_sum, TRUE ~ dWnewborn_dd),
         d2W_dd2_sum = sum((type != "both")*d2W_dd2),
         d2W_dd2_sum = case_when(type == "both" ~ d2W_dd2_sum, TRUE ~ d2W_dd2),
         W_elast_1_sum = sum((type != "both")*W_elast_1),
         W_elast_1_sum = case_when(type == "both" ~ W_elast_1_sum, TRUE ~ W_elast_1),
         W_elast_2_sum = sum((type != "both")*W_elast_2),
         W_elast_2_sum = case_when(type == "both" ~ W_elast_2_sum, TRUE ~ W_elast_2)) %>%
  ungroup()

elast_df %>%
  #filter(location == "Global") %>% 
  mutate(type = str_to_title(type)) %>%
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, ncol = 4) +
  geom_line(aes(y = W/W_start, color = type)) +
  geom_ribbon(aes(ymax = W/W_start, ymin = W_sum/W_start, fill = type), alpha = 0.5) + 
  guides(fill="none") +   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Disease Eradication", y = "W", color = "Effect")
ggsave("figures/derivatives/W_levels.pdf", width = 8, height = 10)

elast_df %>%
  filter(location %in% c("Global", "Low Income", "High Income")) %>% 
  mutate(type = str_to_title(type)) %>%
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, ncol = 4) +
  geom_line(aes(y = -(dW_dd)/W_start, color = type)) +
  geom_ribbon(aes(ymax = -(dW_dd)/W_start, ymin = -(dW_dd_sum)/W_start, fill = type), alpha = 0.5) + 
  guides(fill="none") +   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Disease Eradication",  color = "Effect", y = expression(-frac(partialdiff~W,partialdiff~er[i])))
ggsave("figures/derivatives/W_diff.pdf", width = 8, height = 6)

elast_df %>%
  filter(location %in% c("Global", "Low Income", "High Income")) %>% 
  mutate(type = str_to_title(type)) %>%  
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, ncol = 4) +
  geom_line(aes(y = -(dW_dd)*(1/W_lag), color = type)) +
  geom_ribbon(aes(ymax = -(dW_dd)*(1/W_lag), ymin = -(dW_dd_sum)*(1/W_lag), fill = type), alpha = 0.5) + 
  guides(fill="none") +   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Disease Eradication", color = "Effect", y = expression(-frac(partialdiff~W,partialdiff~er[i])~ frac(1,W)))
ggsave("figures/derivatives/W_elast.pdf", width = 8, height = 6)


elast_df %>%
  filter(location %in% c("Global", "Low Income", "High Income")) %>% 
  mutate(type = str_to_title(type)) %>%  
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, ncol = 4) +
  geom_line(aes(y = -(d2W_dd2)*(1/W^1), color = type)) +
  geom_ribbon(aes(ymax = -(d2W_dd2)*(1/W^1), ymin = -(d2W_dd2_sum)*(1/W^1), fill = type), alpha = 0.5) + 
  guides(fill="none") +   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Disease Eradication", color = "Effect", y = expression(-frac(partialdiff^2~W,partialdiff~er[i]^2)~ frac(1,W)))
ggsave("figures/derivatives/W_elast_2.pdf", width = 8, height = 6)


elast_df %>%
  filter(location %in% c("Global", "Low Income", "High Income")) %>% 
  mutate(type = str_to_title(type)) %>%  
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, ncol = 4, scales = "free_y") +
  geom_line(aes(y = -(d2W_dd2)*(1/W^1), color = type)) +
  geom_ribbon(aes(ymax = -(d2W_dd2)*(1/W^1), ymin = -(d2W_dd2_sum)*(1/W^1), fill = type), alpha = 0.5) + 
  guides(fill="none") +   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Disease Eradication", color = "Effect", y = expression(-frac(partialdiff^2~W,partialdiff~er[i]^2)~ frac(1,W)))
ggsave("figures/derivatives/W_elast_2_freey.pdf", width = 8, height = 6)




"
Cross partials for disease categories
"
cross_elast_df <- W_scenarios %>%
  mutate(location = "Global") %>%
  group_by(start_year, no_births, growth_transitions, diseases, type, location, eradication, erad_senes, year) %>%
  summarise(W = sum(W), Wnewborn = sum(Wnewborn), 
            average_age = sum(average_age*population)/sum(population), LE_birth = sum(LE_birth*population)/sum(population),
            population = sum(population), pop_newborns = sum(pop_newborns)) %>%
  rbind(W_scenarios) %>%
  mutate(diseases = case_when(str_detect(diseases, "infant4") ~ "Infant",
                              str_detect(diseases, "adult_early4") ~ "Adult (early)",
                              str_detect(diseases, "adult_late4") ~ "Adult (late)",
                              str_detect(diseases, "senescent4") ~ "Ageing-related", TRUE ~ "All")) %>%
  mutate(diseases = factor(diseases, ordered = T, levels = c("Infant", "Adult (early)",  "Adult (late)", "Ageing-related")),
         location = str_remove(location, "World Bank "),
         location = factor(location, ordered = T, levels = c("Global", "Low Income",  "Lower Middle Income", "Upper Middle Income", "High Income"))) %>%
  group_by(start_year, no_births, growth_transitions, diseases, type, location, eradication, erad_senes) %>%
  summarise(W = sum(W), Wnewborn = sum(Wnewborn)) %>%
  filter(start_year == 2021, no_births == FALSE, growth_transitions == TRUE) 
  

cross_elast_df %>%
  filter(location %in% c("Global", "Low Income", "High Income")) %>% 
  filter(type == "mortality") %>%
  filter(eradication %in% c(0, 0.2, 0.4, 0.6, 0.8, 1)) %>%
  group_by(start_year, no_births, growth_transitions, diseases, type, location, eradication) %>%
  mutate(W_start = sum((erad_senes == 0)*W),
         Wnewborn_start = sum((erad_senes == 0)*Wnewborn)) %>%
  ggplot(aes(x = erad_senes)) + theme_bw() + 
  facet_wrap(~location+diseases, ncol = 3) +
  scale_color_gradientn(colours = rainbow(3)) +
  geom_line(aes(y = W/W_start, color = eradication, group = eradication)) +
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "bottom") + 
  labs(x = "Eradication of Ageing-Related Diseases", y = "W", color = "Eradiction of Non-Ageing-Related Diseases")
ggsave("figures/derivatives/W_levels_cross_ageing.pdf", width = 8, height = 6)  


cross_elast_df %>%
  filter(location %in% c("Global", "Low Income", "High Income")) %>% 
  filter(type == "mortality") %>%
  filter(erad_senes %in% c(0, 0.2, 0.4, 0.6, 0.8, 1)) %>%
  group_by(start_year, no_births, growth_transitions, diseases, type, location, erad_senes) %>%
  mutate(W_start = sum((eradication == 0)*W),
         Wnewborn_start = sum((eradication == 0)*Wnewborn)) %>%
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, ncol = 3) +
  scale_color_gradientn(colours = rainbow(3)) +
  geom_line(aes(y = W/W_start, color = erad_senes, group = erad_senes)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "bottom") + 
  labs(x = "Eradication of Non-Ageing-Related Diseases", y = "W", color = "Eradiction of Ageing-Related Diseases")
ggsave("figures/derivatives/W_levels_cross_nonageing.pdf", width = 8, height = 6)  



  


  
  