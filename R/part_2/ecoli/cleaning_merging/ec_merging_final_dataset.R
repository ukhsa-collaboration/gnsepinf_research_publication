##script where we produce a dataframe where each row represents
##one infant with a reported episode for EC
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  data.table,
  skimr
)

source("paths/import_datasets.R")

ec_infants <- ec_all_joined %>% 
  arrange(data_collection_date) %>% 
  group_by(nhs.x) %>% 
  slice_max(number_episodes, with_ties = FALSE) %>% 
  summarize(
    across(everything(),
            first),
    DateOfDeath = last(DateOfDeath),
    death_90 = last(death_90),
    baby_dead = as.integer(any(baby_dead== 1)),
    diff_time_death_bact = last(diff_time_death_bact),
    diff_time_death_bact_grp = last(diff_time_death_bact_grp),
    number_episodes = max(number_episodes)
  )


## we will recalculate the difference between the first positive and the death

ec_infants <- ec_infants %>% 
  mutate(
    first_episode_to_death = difftime(
        DateOfDeath,
        data_collection_date, 
        units = "days")
  ) %>%
  mutate_if(is.difftime,as.integer) %>% 
  mutate(
    first_episode_to_death_grp=case_when(
      first_episode_to_death %in% c(0,1,2,3,4,5,6,7) ~ "0-7 days",
      first_episode_to_death >=8 & first_episode_to_death<31 ~"8-30 days",
      first_episode_to_death >=31 & first_episode_to_death<61 ~"31-60 days",
      first_episode_to_death >=61 & first_episode_to_death<91 ~"61-90 days",
      first_episode_to_death >=91  ~"90+ days",
      TRUE ~ "NA")
  ) %>% 
  mutate(
    first_episode_death_90 = case_when(
      first_episode_to_death <91 ~ 1, #baby died within 90 days of the diagnosed GNS
      first_episode_to_death >=91 ~ 0,
      is.na(first_episode_to_death) ~ 0 )
  ) %>% 
  mutate(
    death_90 = replace_na(death_90, 0)
  ) %>%
  mutate(
    address = case_when(
      is.na(address) ~ "invalid postcode",
      TRUE ~ address
    )
  ) %>% 
  mutate(
    address = case_when(
      address == "invalid postcode" ~ NA_character_,
      TRUE ~ address
    )
  ) %>% 
  mutate(
    imd_quintile = case_when(
      imd_quintile == "Unknown" ~ NA_character_,
      TRUE ~ imd_quintile
    )
  )


