
"
Function to change mortality through slowing aging
"
slow_mortality <- function(mortality_df, slow_by = 0.01, start_slowing = 30, 
                           end_age = 100){
  mortality_df$mortality_new <- mortality_df$mortality
  for (aa in start_slowing:end_age){
    obs <- which(mortality_df$age == aa)
    cont_age <- max((1 - slow_by)*aa, start_slowing)
    int_part <- cont_age%/%1
    int_obs <- which(mortality_df$age == int_part)
    dec_part <- cont_age%%1
    # New mortality should be slowed version of old mortality 
    # m_new = m_old[int_part] + dec_part*(m_old[int_part+1] - m_old[in_part])
    mortality_df$mortality_new[obs] <- 
      mortality_df$mortality[int_obs] + dec_part*(mortality_df$mortality[int_obs+1]
                                                  - mortality_df$mortality[int_obs])
  }
  return(mortality_df)
}


"
Function to change disability through slowing aging
"
slow_disability <- function(disability_df, slow_by = 0.01, start_slowing = 30, 
                           end_age = 100){
  disability_df$disability_new <- disability_df$disability
  for (aa in start_slowing:end_age){
    obs <- which(disability_df$age == aa)
    cont_age <- max((1 - slow_by)*aa, start_slowing)
    int_part <- cont_age%/%1
    int_obs <- which(disability_df$age == int_part)
    dec_part <- cont_age%%1
    # New disability should be slowed version of old disability 
    # m_new = m_old[int_part] + dec_part*(m_old[int_part+1] - m_old[in_part])
    disability_df$disability_new[obs] <- 
      disability_df$disability[int_obs] + dec_part*(disability_df$disability[int_obs+1]
                                                  - disability_df$disability[int_obs])
  }
  return(disability_df)
}


"
Function to change fertility through slowing aging
"
slow_fertility <- function(fertility_df, slow_by = 0.01, start_slowing = 30, 
                            end_age = 100){
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
Forecast dalys from start_year to end_year given population, fertility, mortality and disability
"
forecast_dalys <- function(population_df, fertility_df, mortality_df, disability_df, 
                         new = FALSE, start_year = 2021, end_year = 2100, end_age = 100, 
                         no_births = FALSE){
  if (new){
    mortality_df$mortality <- mortality_df$mortality_new
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
    full_join(select(fertility_df, c(age, fertility)), by = "age") %>%
    full_join(select(mortality_df, c(age, mortality)), by = "age") %>%
    full_join(select(disability_df, c(age, disability)), by = "age") %>%
    arrange(age) %>% mutate(fertility = replace_na(fertility, 0)) %>%
    mutate(mortality = mortality/100000, disability = disability/100000)
  
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
                              start_year = 2021, end_year = 2100, end_age = 100, no_births = FALSE){
  # Calculate the forecast dalys for baseline and new 
  pop_baseline <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                                 start_year = start_year, end_year = end_year, end_age = end_age, 
                                 no_births = no_births, new = FALSE)
  pop_new <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                            start_year = start_year, end_year = end_year, end_age = end_age, 
                            no_births = no_births, new = TRUE)
  # Merge in the baseline and new
  dalys_df <- pop_baseline %>% select(year, age, population, mortality, disability, daly) %>%
    rename(population_base = population, daly_base = daly,
           mortality_base = mortality, disability_base = disability) %>%
    full_join(select(pop_new, c(year, age, population, mortality, disability, daly)), by = c("year", "age")) %>%
    rename(population_new = population, daly_new = daly,
           mortality_new = mortality, disability_new = disability) %>%
    mutate(pop_diff = population_new - population_base, daly_diff = daly_new - daly_base,
           mortality = mortality_new - mortality_base, disability = disability_new - disability_base) %>%
    arrange(year, age)
  # Aggregate to annual
  dalys_yly <- dalys_df %>% group_by(year) %>%
    mutate(LE_base = cumprod((1 - mortality_base)), LE_new = cumprod((1 - mortality_new))) %>%
    mutate(HLE_base = (1-disability_base)*cumprod(1 - mortality_base), 
           HLE_new = (1-disability_new)*cumprod(1 - mortality_new)) %>%
    summarise(across(c(population_base, population_new, pop_diff, daly_base, daly_new, daly_diff,
                       LE_base, LE_new, HLE_base, HLE_new), sum))
  return(dalys_yly)
}