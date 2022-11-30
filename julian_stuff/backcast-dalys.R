setwd("/Users/julianashwin/Documents/GitHub/dalys-model/julian_stuff")
rm(list=ls())

library(ggplot2)
library(ggpubr)
library(readxl)
library(dplyr)
library(stringr)
library(reshape2)
library(tidyr)

source("functions.R")


# Import data 
all_fertility_df <- as_tibble(read.csv("clean_data/fertility_rates.csv",stringsAsFactors = F))
all_population_df <- as_tibble(read.csv("clean_data/population.csv",stringsAsFactors = F))
all_mortality_df <- as_tibble(read.csv("clean_data/mortality_medium.csv",stringsAsFactors = F, check.names=F))
all_disability_df <- as_tibble(read.csv("clean_data/disability_medium.csv",stringsAsFactors = F, check.names=F))
cause_tree_df <- as_tibble(read.csv("clean_data/cause_definitions.csv", stringsAsFactors = F))
income_transition_df <- as_tibble(read.csv("clean_data/income_transition_shares.csv", 
                                           stringsAsFactors = F, check.names=F))

locations <- unique(all_mortality_df$location)

loc_cols <- c("Global" = "black","World Bank High Income" = "forestgreen", 
              "World Bank Upper Middle Income" = "green", "World Bank Lower Middle Income" = "orange",
              "World Bank Low Income" = "red")
scenario_cols <- c("Senescent" = "firebrick1", "Adult_late" = "blue3", 
                   "Adult_early" = "cornflowerblue", "Infant" = "forestgreen", 
                   "Slow_10pc" = "darkgoldenrod3", "Slow_25pc" = "orange")


infant4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Infant")] 
adult_early4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_early")] 
adult_late4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Adult_late")] 
senescent4_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == "Senescent")] 

# Choose options
locations <- unique(all_mortality_df$location)[2:5]
start_years <- 1990:2021
end_year <- start_year + 100
end_age <- 100
no_births <- FALSE
fertility_type <- "fertility_med"
growth_transitions <- TRUE
disease_cluster <- c("Infant", "Adult_early", "Adult_late", "Senescent")


results_df <- data.frame()
for (loc_name in locations){
  print(paste0("Analysis for ", loc_name))
  # Select which region to focus on and which years to use as a starting point
  population_df <- filter(all_population_df, location == loc_name)
  fertility_df <- filter(all_fertility_df, location == loc_name, year >= start_year)
  mortality_df <- create_mortality_df(all_mortality_df, loc_name = loc_name, start_year = min(start_years),
                                      end_year = max(start_years) + 100, 
                                      end_age = end_age, growth_transitions = growth_transitions, 
                                      income_transition_df = income_transition_df)
  disability_df <- create_disability_df(all_disability_df, loc_name = loc_name, start_year = min(start_years), 
                                        end_year = max(start_years) + 100, 
                                        end_age = end_age, growth_transitions = growth_transitions, 
                                        income_transition_df = income_transition_df) 
  for (start_year in start_years[c(1,5,10,15,20,25,30,32)]){
    print(paste0("Starting in year ", start_year))
    end_year <- start_year + 100
    
    fertility_df <- filter(fertility_df, year >= start_year)
    mortality_df <- filter(mortality_df, year >= start_year)
    disability_df <- filter(disability_df, year >= start_year)
    
    for (clust in disease_cluster){
      clust_diseases <- cause_tree_df$cause_name[which(cause_tree_df$cluster4 == clust)] 
      
      mortality_df <- def_mortality_new(mortality_df, clust_diseases)
      disability_df <- def_disability_new(disability_df, clust_diseases)
      dalys_cluster <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                         start_year = start_year, end_year = end_year, no_births = no_births,
                                         fertility_type = fertility_type, growth_transitions = growth_transitions) %>%
        mutate(location = loc_name, intervention = clust, fertility_type = fertility_type, 
               start_year = start_year, growth_trans = growth_transitions)
      
      results_df <- rbind(results_df, dalys_cluster)
    }
    # Slow aging by 10%
    mortality_df <- slow_mortality(mortality_df, slow_by = 0.1)
    disability_df <- slow_disability(disability_df, slow_by = 0.1)
    dalys_slow_10pc <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                         start_year = start_year, end_year = end_year, no_births = no_births,
                                         fertility_type = fertility_type, growth_transitions = growth_transitions) %>% 
      mutate(location = loc_name, intervention = "Slow_10pc", fertility_type = fertility_type, 
             start_year = start_year, growth_trans = growth_transitions)
  # Slow aging by 25%
    mortality_df <- slow_mortality(mortality_df, slow_by = 0.25)
    disability_df <- slow_disability(disability_df, slow_by = 0.25)
    dalys_slow_25pc <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, 
                                         start_year = start_year, end_year = end_year, no_births = no_births,
                                         fertility_type = fertility_type, growth_transitions = growth_transitions) %>% 
      mutate(location = loc_name, intervention = "Slow_25pc", fertility_type = fertility_type, 
             start_year = start_year, growth_trans = growth_transitions)
    
    # Append all results
    results_df <- rbind(results_df, dalys_slow_10pc, dalys_slow_25pc)
    
  }
}

# Rearrange and make sure location is an ordered factor
results_df <- relocate(results_df, location,intervention, fertility_type, start_year, .before = year)
results_df <- results_df %>% 
  mutate(loc_order = case_when(str_detect(location, "High")  ~ 1,
                               str_detect(location, "Upper") ~ 2,
                               str_detect(location, "Lower") ~ 3,
                               str_detect(location, "Low ") ~ 4)) %>%
  arrange(loc_order, intervention, fertility_type, start_year, year)
results_df$location <- factor(results_df$location, levels = unique(results_df$location), ordered = T)



ggplot(results_df) + theme_bw() + facet_wrap(vars(location)) +
  scale_color_manual("Scenario", values = scenario_cols) +
  geom_line(aes(x = start_year, y = daly_diff/1e9, color = intervention, group = intervention), 
            stat = "summary", fun = "sum")
ggsave("figures/backcast_dalys.pdf", width = 8, height = 6)


baseline_results <- filter(results_df, start_year == 1990 & fertility_type == "fertility_med" &
                             growth_trans == TRUE)
ggplot(baseline_results) + theme_bw() +
  #facet_wrap(.~fertility_type) +
  geom_bar(aes(x = intervention, y = daly_diff/1e9, fill = location), stat = "summary", fun = "sum") +
  scale_fill_manual("Region", values = loc_cols) + 
  scale_color_manual("Region", values = loc_cols) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(y = "Extra DALYs (billions)", x = "Secnario", fill = "")



