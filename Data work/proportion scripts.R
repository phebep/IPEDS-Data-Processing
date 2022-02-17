library(readr)
library(tidyverse)
library(janitor)

states <- read_csv("Data work/State abbreviations.csv") %>% clean_names()

proportion_processing <- function(ds, statelist){
  ds <- ds %>% left_join(states, by = "unitid") %>%
    rename(state = hd_2020_state_abbreviation, year = year.x, institution_name = institution_name.x) %>%
    filter(state %in% statelist) %>%
    mutate(degree_group = if_else(degree == "Bachelor's", "Undergraduate",
                                  if_else(degree != "Associate's", "Graduate", "2yr"))) %>%
    select(-c(institution_name.y, year.y))
  
  ds_eng <- ds %>% filter(type == "Engineering Students") %>% 
    group_by(unitid, institution_name, year, degree_group, gender, race_ethnicity) %>%
    summarize(count = sum(count))
  
  ds_grand <- ds %>% filter(type == "All Students") %>%
    group_by(unitid, institution_name, year, degree_group, gender, race_ethnicity) %>%
    summarize(count = sum(count))
  
  ## REGION PROPORTIONS -------------------------------------------------------------------------------------------
  
  # summing totals for different demographic categories 
  grand_sum <- ds_grand %>%
    group_by(year, degree_group, gender, race_ethnicity) %>%
    summarize(total = sum(count)) %>%
    filter(gender != "total") %>%
    filter(race_ethnicity != "Total")
  
  # summing general totals by degree type and year 
  totals <- grand_sum %>%
    group_by(year, degree_group) %>%
    summarize(grand_total = sum(total))
  
  # creating proportions 
  total_props <- inner_join(grand_sum, totals, by = c("year", "degree_group")) %>%
    mutate(proportion = total/grand_total)
  
  # summing total engineering degrees for different demographic categories 
  engineering_sum <- ds_eng %>%
    group_by(year, degree_group, gender, race_ethnicity) %>%
    summarize(total = sum(count)) %>%
    filter(gender != "total") %>%
    filter(race_ethnicity != "Total")
  
  # summing general totals by engineering degree type and year
  engineering_totals <- engineering_sum %>%
    group_by(year, degree_group) %>%
    summarize(grand_total = sum(total))
  
  # creating proportions for general college engineering populations
  engineering_props <- inner_join(engineering_sum, engineering_totals, by = c("year", "degree_group")) %>%
    mutate(proportion = total/grand_total)
  
  
  
  ## BY INSTITUTION-----------------------------------------------------------------------------------------------------
  
  # summing total enrollment per degree type (not best method, kind of roundabout)
  school_totals <- ds_grand %>%
    filter(!race_ethnicity == "Total") %>%
    filter(gender != "total") %>%
    group_by(unitid, year, degree_group) %>%
    summarize(total = sum(count))
  
  # getting demographic proportions at the degree type level
  total_joined <- inner_join(ds_grand, school_totals, by = c("year", "degree_group", "unitid")) %>%
    mutate(obs_prop = count/total)
  
  # summing total enrollment per engineering program (not best method, kind of roundabout)
  engineering_school_totals <- ds_eng %>%
    filter(!race_ethnicity == "Total") %>%
    filter(gender != "total") %>%
    group_by(unitid, year, degree_group) %>%
    summarize(total = sum(count))
  
  # getting demographic proportions at the engineering program level
  engineering_joined <- inner_join(ds_eng, 
                                   engineering_school_totals, 
                                   by = c("year", "degree_group", "unitid")) %>%
    mutate(obs_prop = count/total)
  
  ## COMPARISONS---------------------------------------------------------------------------------------------------------
  # total comparison dataframe
  total_comp <- total_joined %>%
    inner_join(total_props, by = c("year", "degree_group", "gender", "race_ethnicity")) %>%
    select(-c(total.x, total.y, grand_total)) %>%
    rename(regional_prop = proportion, school_prop = obs_prop) %>%
    mutate(regional_school_diff = school_prop - regional_prop)
  
  # engineering comparison dataframe
  engineering_comp <- engineering_joined %>%
    inner_join(engineering_props, by = c("year", "degree_group", "gender", "race_ethnicity")) %>%
    select(-c(total.x, total.y, grand_total)) %>%
    rename(regional_prop = proportion) %>%
    mutate(regional_eng_diff = obs_prop - regional_prop)
  
  # engineering comparison dataframe with school proportions
  total_comp_for_join <- total_comp %>%
    select(-c(regional_prop, diff, count, institution_name))
  
  engineering_comp_all <- inner_join(engineering_comp, total_comp_for_join, by = c("unitid", "year", "degree_group", "gender", "race_ethnicity")) %>%
    mutate(within_school_diff = obs_prop - school_prop) %>%
    select(-institution_name.x) %>% rename(institution_name = institution_name.y)
  return(engineering_comp_all)
}