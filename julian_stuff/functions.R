"
Create dataframe with mortlaity rates for each year
"
create_mortality_df <- function(all_mortality_df, loc_name = "Global", start_year = 2021, 
                                end_year = 2100, end_age = 100, growth_transitions = FALSE, 
                                income_transition_df = NULL){
  mortality_df <- filter(all_mortality_df, location == loc_name) %>%
    mutate(year = start_year) %>%
    relocate(year, .after = location) %>%
    full_join(crossing(year = start_year:end_year, age = 0:end_age)) %>%
    arrange(location, year, age) %>%
    fill(location, .direction = "down")
  
  if (growth_transitions){
    stopifnot(loc_name %in% income_transition_df$income_group)
    # Identify the transition proportions, groups and causes
    transitions_df <- filter(income_transition_df, income_group == loc_name) %>%
      arrange(year)
    group_names <- names(income_transition_df)[3:6]
    cause_names <- names(select(all_mortality_df, mortality:last_col()))
    
    # Loop through years
    pb <- txtProgressBar(min = start_year+1, max = end_year)
    for (yy in (start_year+1):end_year){
      yy_obs <- which(mortality_df$year == yy)
      mortality_df[yy_obs,cause_names] <- 0
      trans_props <- transitions_df[which(transitions_df$year == yy),group_names]
      # Add contribution from each income groups rates
      for (gg in group_names){
        add_rates <- trans_props[1,gg]*as.matrix(all_mortality_df[which(all_mortality_df$location == gg),cause_names])
        mortality_df[yy_obs,cause_names] <- mortality_df[yy_obs,cause_names] + add_rates
      }
      setTxtProgressBar(pb, yy)
    }
  } else {
    mortality_df <- mortality_df %>%
      group_by(location, age) %>% 
      fill(names(mortality_df)) %>% 
      ungroup()
  }
  return(mortality_df)
}




"
Create dataframe with mortlaity rates for each year
"
create_disability_df <- function(all_disability_df, loc_name = "Global", start_year = 2021, 
                                end_year = 2100, end_age = 100, growth_transitions = FALSE, 
                                income_transition_df = NULL){
  disability_df <- filter(all_disability_df, location == loc_name) %>%
    mutate(year = start_year) %>%
    relocate(year, .after = location) %>%
    full_join(crossing(year = start_year:end_year, age = 0:end_age)) %>%
    arrange(location, year, age) %>%
    fill(location, .direction = "down")
  
  if (growth_transitions){
    stopifnot(loc_name %in% income_transition_df$income_group)
    # Identify the transition proportions, groups and causes
    transitions_df <- filter(income_transition_df, income_group == loc_name) %>%
      arrange(year)
    group_names <- names(income_transition_df)[3:6]
    cause_names <- names(select(all_disability_df, disability:last_col()))
    
    # Loop through years
    pb <- txtProgressBar(min = start_year+1, max = end_year)
    for (yy in (start_year+1):end_year){
      yy_obs <- which(disability_df$year == yy)
      disability_df[yy_obs,cause_names] <- 0
      trans_props <- transitions_df[which(transitions_df$year == yy),group_names]
      # Add contribution from each income groups rates
      for (gg in group_names){
        add_rates <- trans_props[1,gg]*as.matrix(all_disability_df[which(all_disability_df$location == gg),cause_names])
        disability_df[yy_obs,cause_names] <- disability_df[yy_obs,cause_names] + add_rates
      }
      setTxtProgressBar(pb, yy)
    }
  } else {
    disability_df <- disability_df %>%
      group_by(location, age) %>% 
      fill(names(disability_df)) %>% 
      ungroup()
  }
  return(disability_df)
}






"
Function to change mortality through slowing aging
"
slow_mortality <- function(mortality_df, slow_by = 0.01, start_slowing = 30, 
                           end_age = 100){
  mortality_df$mortality_new <- mortality_df$mortality
  for (yy in unique(mortality_df$year)){
    for (aa in start_slowing:end_age){
      obs <- which(mortality_df$age == aa & mortality_df$year == yy)
      cont_age <- max((1 - slow_by)*aa, start_slowing)
      int_part <- cont_age%/%1
      int_obs <- which(mortality_df$age == int_part & mortality_df$year == yy)
      dec_part <- cont_age%%1
      # New mortality should be slowed version of old mortality 
      # m_new = m_old[int_part] + dec_part*(m_old[int_part+1] - m_old[in_part])
      mortality_df$mortality_new[obs] <- 
        mortality_df$mortality[int_obs] + dec_part*(mortality_df$mortality[int_obs+1]
                                                    - mortality_df$mortality[int_obs])
    }
  }
  return(mortality_df)
}


"
Function to change disability through slowing aging
"
slow_disability <- function(disability_df, slow_by = 0.01, start_slowing = 30, 
                           end_age = 100){
  disability_df$disability_new <- disability_df$disability
  for (yy in unique(mortality_df$year)){
    for (aa in start_slowing:end_age){
      obs <- which(disability_df$age == aa & disability_df$year == yy)
      cont_age <- max((1 - slow_by)*aa, start_slowing)
      int_part <- cont_age%/%1
      int_obs <- which(disability_df$age == int_part & disability_df$year == yy)
      dec_part <- cont_age%%1
      # New disability should be slowed version of old disability 
      # m_new = m_old[int_part] + dec_part*(m_old[int_part+1] - m_old[in_part])
      disability_df$disability_new[obs] <- 
        disability_df$disability[int_obs] + dec_part*(disability_df$disability[int_obs+1]
                                                    - disability_df$disability[int_obs])
    }
  }
  return(disability_df)
}


"
Function to change fertility through slowing aging
"
slow_fertility <- function(fertility_df, slow_by = 0.01, start_slowing = 30, 
                            end_age = 100, fertility_type = "fertility_est"){
  
  fertility_df$fertility <- fertility_df[,fertility_type]
  fertility_df$fertility_new <- fertility_df$fertility
  for (aa in start_slowing:end_age){
    obs <- which(fertility_df$age == aa)
    cont_age <- max((1 - slow_by)*aa, start_slowing)
    int_part <- cont_age%/%1
    int_obs <- which(fertility_df$age == int_part)
    dec_part <- cont_age%%1
    # New fertility should be slowed version of old fertility 
    # m_new = m_old[int_part] + dec_part*(m_old[int_part+1] - m_old[in_part])
    fertility_df$fertility_new[obs] <- 
      fertility_df$fertility[int_obs] + dec_part*(fertility_df$fertility[int_obs+1]
                                                    - fertility_df$fertility[int_obs])
  }
  return(fertility_df)
}


"
Define mortality_new by removing a proportion of causes in disease_list
"
def_mortality_new <- function(mortality_df, disease_list, remove_prop = 1){
  mortality_df$mortality_new <- mortality_df$mortality - 
    remove_prop*rowSums(mortality_df[,which(names(mortality_df) %in% disease_list)])
  return(mortality_df)
}

"
Define disability_new by removing a proportion of causes in disease_list
"
def_disability_new <- function(disability_df, disease_list, remove_prop = 1){
  disability_df$disability_new <- disability_df$disability - 
    remove_prop*rowSums(disability_df[,which(names(disability_df) %in% disease_list)])
  return(disability_df)
}



"
Forecast dalys from start_year to end_year given population, fertility, mortality and disability
"
forecast_dalys <- function(population_df, fertility_df, mortality_df, disability_df, 
                         new = "none", start_year = 2021, end_year = 2100, end_age = 100, 
                         no_births = FALSE, fertility_type = "fertility_est", 
                         growth_transitions = FALSE){
  # Set fertility rates according to fertility_type
  fertility_df$fertility <- fertility_df[,fertility_type]
  # If not using growth transitions, just use start_year data
  if (!growth_transitions){
    mortality_df <- filter(mortality_df, year == start_year) %>%
      full_join(crossing(year = start_year:end_year, age = 0:end_age)) %>%
      arrange(location, year, age) %>% fill(location, .direction = "down") %>% 
      group_by(location, age) %>% fill(names(mortality_df)) %>% 
      ungroup()
    
    disability_df <- filter(disability_df, year == start_year) %>%
      full_join(crossing(year = start_year:end_year, age = 0:end_age)) %>%
      arrange(location, year, age) %>% fill(location, .direction = "down") %>% 
      group_by(location, age) %>% fill(names(disability_df)) %>% 
      ungroup()
  }
  
  # Set mortality and/or disability to new version according to new
  stopifnot(new %in% c("none", "both", "mortality", "disability"))
  if (new == "both"){
    mortality_df$mortality <- mortality_df$mortality_new
    disability_df$disability <- disability_df$disability_new
  } else if (new == "mortality"){
    mortality_df$mortality <- mortality_df$mortality_new
  } else if (new == "disability"){
    disability_df$disability <- disability_df$disability_new
  }
  if (no_births){
    fertility_df$fertility <- 0
  }
  # Create a population dataframe to fill
  pop_data_long <- population_df %>% 
    filter(year == start_year) %>%
    select(year, age, population) %>%
    full_join(crossing(year = start_year:end_year, age = 0:end_age), by = c("year", "age")) %>%
    arrange(year, age) %>%
    full_join(select(mortality_df, c(year, age, mortality)), by = c("year", "age")) %>%
    full_join(select(disability_df, c(year, age, disability)), by = c("year", "age")) %>%
    full_join(select(fertility_df, c(year, age, fertility)), by = c("year", "age")) %>%
    arrange(age, year) %>% 
    group_by(age) %>% fill(fertility, .direction = "down") %>% ungroup() %>%
    mutate(fertility = replace_na(fertility, 0)) %>%
    mutate(mortality = mortality/100000, disability = disability/100000) %>%
    arrange(year, age)
  
  
  
  years <- start_year:end_year
  for (yy in years[2:length(years)]){
    # Isolate last year's pop and combine with rates
    pop_old <- pop_data_long[which(pop_data_long$year == yy-1),]
    # Calculate the new births (assume half of population are women)
    population_new <- rep(0, nrow(pop_old))
    population_new[1] <- sum(pop_old$population*pop_old$fertility)
    # Use mortality to calculate survivors from previous period
    population_new[2:(end_age+1)] <- pop_old$population[1:end_age]*(1-pop_old$mortality[1:end_age])
    # Add the new population numbers back into pop_data_long
    pop_data_long$population[which(pop_data_long$year == yy)] <- population_new
  }
  pop_data_long <- pop_data_long %>%
    mutate(daly = (1 - disability)*population)
  
  return(pop_data_long)
}




"
Compare forecasts under mortality/disability and mortality_new/disability_new
"
compare_forecasts <- function(population_df, fertility_df, mortality_df, disability_df, 
                              start_year = 2021, end_year = 2100, end_age = 100, no_births = FALSE,
                              fertility_type = "fertility_est", growth_transitions = FALSE){
  # Calculate the forecast dalys for baseline and new 
  pop_baseline <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                                 start_year = start_year, end_year = end_year, end_age = end_age, 
                                 no_births = no_births, new = "none", fertility_type = fertility_type, 
                                 growth_transitions = growth_transitions)
  pop_new <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                            start_year = start_year, end_year = end_year, end_age = end_age, 
                            no_births = no_births, new = "both", fertility_type = fertility_type, 
                            growth_transitions = growth_transitions)
  pop_mortonly <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                            start_year = start_year, end_year = end_year, end_age = end_age, 
                            no_births = no_births, new = "mortality", fertility_type = fertility_type, 
                            growth_transitions = growth_transitions)
  pop_disonly <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                                 start_year = start_year, end_year = end_year, end_age = end_age, 
                                 no_births = no_births, new = "disability", fertility_type = fertility_type, 
                                growth_transitions = growth_transitions)
  
  # Merge in the baseline and new
  dalys_df <- pop_baseline %>% select(year, age, population, daly, mortality, disability) %>%
    rename(population_base = population, daly_base = daly, mort_base = mortality, dis_base = disability) %>%
    full_join(select(pop_new, c(year, age, population, daly, mortality, disability)), by = c("year", "age")) %>%
    rename(population_new = population, daly_new = daly, mort_new = mortality, dis_new = disability) %>%
    full_join(select(pop_mortonly, c(year, age, population, daly, mortality, disability)), by = c("year", "age")) %>%
    rename(population_mort = population, daly_mort = daly, mort_mort = mortality, dis_mort = disability) %>%
    full_join(select(pop_disonly, c(year, age, population, daly, mortality, disability)), by = c("year", "age")) %>%
    rename(population_dis = population, daly_dis = daly, mort_dis = mortality, dis_dis = disability) %>%
    mutate(pop_diff = population_new - population_base, daly_diff = daly_new - daly_base,
           pop_diff_mort = population_mort - population_base, daly_diff_mort = daly_mort - daly_base,
           pop_diff_dis = population_dis - population_base, daly_diff_dis = daly_dis - daly_base) %>%
    arrange(year, age)
  

  # Aggregate to annual
  dalys_yly <- dalys_df %>% group_by(year) %>%
    mutate(LE_base = cumprod(1 - mort_base), LE_new = cumprod(1 - mort_new),
           LE_mort = cumprod(1 - mort_mort), LE_dis = cumprod(1 - mort_dis)) %>%
    mutate(HLE_base = (1-dis_base)*cumprod(1 - mort_base), HLE_new = (1-dis_new)*cumprod(1 - mort_new),
           HLE_mort = (1-dis_mort)*cumprod(1 - mort_mort), HLE_dis = (1-dis_dis)*cumprod(1 - mort_dis)) %>%
    summarise(across(c(population_base, population_new, population_mort, population_dis, 
                       daly_base, daly_new, daly_mort, daly_dis, 
                       pop_diff, pop_diff_mort, pop_diff_dis, daly_diff, daly_diff_mort, daly_diff_dis,
                       LE_base, LE_new, LE_mort, LE_dis, HLE_dis, HLE_new, HLE_mort, HLE_dis), sum))
  return(dalys_yly)
}