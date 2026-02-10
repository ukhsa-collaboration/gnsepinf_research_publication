####dataset merging for klebsiella
source("paths/import_datasets.R")

##merging the clean dataset with the postcodes to work out missing valies of imd
kleb_postcodes <- left_join(kleb_clean,
                          england,
                          by = c("homeadd"))



##merging previous dataset with vitality records


kleb_all_joined <- left_join(kleb_postcodes,
                             kleb_vitc,
                             by = c("id"))

##merging with comorbidities
kleb_infants_comorb <- left_join(kleb_infants,
                                 kleb_comorbidity,
                                 by = c("id"))

###merging dataset with procedures
kleb_procedures <- left_join(kleb_invasive,
                             kleb_spell,
                             by = c("newnhsno" = "nhs.x"))

kleb_final <- left_join(kleb_infants_comorb,
                        kleb_group_procedures,
                        by = c("nhs.x" = "newnhsno"))
kleb_final <- kleb_final %>% 
  mutate(death_30 = case_when(
    death_30 ==1 ~ 1,
    death_30 ==0 ~0,
    is.na(death_30) ~0
  ))


###other bits of cleaning below::

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


kleb_infants <- kleb_all_joined %>% 
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

kleb_infants <- kleb_infants %>% 
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
