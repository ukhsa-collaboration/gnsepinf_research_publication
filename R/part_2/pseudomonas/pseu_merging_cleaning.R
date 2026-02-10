####dataset merging for pseudomonas
source("paths/import_datasets.R")

##merging the clean dataset with the postcodes to work out missing valies of imd
pseu_postcodes <- left_join(pseud_clean,
                            england,
                            by = c("homeadd"))



##merging previous dataset with vitality records


pseu_all_joined <- left_join(pseu_postcodes,
                             pseu_vitc,
                             by = c("id"))

##merging with comorbidities
pseu_infants_comorb <- left_join(pseu_infants,
                                 pseu_comorbidity,
                                 by = c("nhs.x" = "nhs"))

###merging dataset with procedures
pseu_procedures <- left_join(pseu_invasive,
                             pseu_spell,
                             by = c("newnhsno" = "nhs.x"))

pseu_final <- left_join(pseu_infants_comorb,
                        pseu_group_procedures,
                        by = c("nhs.x" = "newnhsno"))

pseu_final <- pseu_final %>% 
  mutate(infage_days = difftime(
    data_collection_date,
    dob_br,
    unit="days"
  )
  ) %>%  # age cat is based on the age in days of the infant when the sample was taken
  mutate(infant_age_cat = case_when(
    infage_days <= as.difftime(3, units = "days") ~ "0-3 days",
    infage_days > as.difftime(3, units = "days") & infage_days <= as.difftime(28, units = "days") ~ "4-28 days",
    infage_days > as.difftime(28, units = "days") & infage_days <= as.difftime(90, units = "days") ~ "29-90 days",
    infage_days > as.difftime(90, units = "days") & infage_days <= as.difftime(365, units = "days") ~ "91-365 days",
    TRUE ~ NA_character_
  )
  # here i found that for the age_Description that the previous person used they probably
  #used dob instead of dob_br to code for the age_description variable which is diff for 2 values in my dataset
  # we trust dob_br more than dob so i am dropping them
  ) %>% 
  mutate(death_30 = case_when(
    death_30 == 1 ~1,
    death_30 == 0 ~0,
    is.na(death_30) ~ 0
  )) %>% 
  mutate(delivery_location = case_when(
    delivery_location == "Hospital" ~ "NHS hospital"
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


pseu_infants <- pseu_all_joined %>% 
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

pseu_infants <- pseu_infants %>% 
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
