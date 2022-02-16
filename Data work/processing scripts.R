process_completions <- function(ds){
  completions <- ds %>% 
    select(-IDX_C)
  length <- length(completions)
  completions <- completions %>%
    pivot_longer(8:length, names_to = "population", values_to = "count") %>%
    clean_names() %>%
    mutate(gender = word(population, -1), race_ethnicity = gsub("\\s*\\w*$", "",gsub(".*\\.", "", population)))%>%
    select(-population)
  names(completions)[4] <- substring(names(completions[4]), 9)
  names(completions)[5] <- substring(names(completions[5]), 9)
  names(completions)[7] <- substring(names(completions[7]), 9)
  if("cip_code_2010_classification" %in% colnames(completions)){
    completions <- completions %>% 
      mutate(race_ethnicity = if_else(race_ethnicity %in% c("Grand", "Grand total"), "Total", race_ethnicity)) %>%
      mutate(type = if_else(cip_code_2010_classification == "'99'", "All Students",
                            "Engineering Students"), degree = word(award_level_code , 1)) %>%
      select(-c(cip_code_2010_classification, cip_title))
  } else {
    completions <- completions %>% 
      mutate(race_ethnicity = if_else(race_ethnicity %in% c("Grand", "Grand total"), "Total", race_ethnicity)) %>%
      mutate(type = if_else(cip_code_2020_classification == "'99'", "All Students",
                            "Engineering Students"), degree = word(award_level_code , 1)) %>%
      select(-c(cip_code_2020_classification, cip_title))
  }
  completions <- completions %>% select(-award_level_code)
  return(completions)
}

process_enrollment <- function(filename, eng = FALSE){
  enrollment <- readr::read_csv(filename) %>% 
    select(-IDX_EF)
  names(enrollment)[4:34] <- substring(names(enrollment)[4:34], 9)  
  enrollment <- enrollment %>%
    pivot_longer(5:34, names_to = "population", values_to = "count") %>%
    clean_names() %>%
    mutate(gender = word(population, -1), race_ethnicity = gsub("\\s*\\w*$", "", population)) %>%
    select(-population) 
  if(eng){
    enrollment <- enrollment %>%
      mutate(race_ethnicity = sub("RV.", "", race_ethnicity)) %>%
      mutate(type = "Engineering", degree = word(major_field_of_study , 2))
  } else {
    enrollment <- enrollment %>%
      mutate(race_ethnicity = sub("RV.", "", race_ethnicity)) %>%
      mutate(race_ethnicity = if_else(race_ethnicity == "Grand total", "Total", race_ethnicity)) %>%
      mutate(type = "All Students", degree = word(level_of_student , 3))
  }
  
  return(enrollment)
}