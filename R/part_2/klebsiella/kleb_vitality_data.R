#Cleaning vitality records for klebsiella

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


kleb_vitc <- kleb_vit %>% 
  filter(RecType %in% c(30, 33, 40)
  ) %>% 
  mutate(
   data_collection_date = as.Date(data_collection_date) 
  ) %>% 
  # mutate_at(vars(
  #   data_collection_date),
  #   ~dmy(.)
  # ) %>% 
  mutate_at(vars(
    DateOfDeath),
    ~ymd(.)
  ) %>% 
  mutate(
    baby_dead=if_else(
      is.na(DateOfDeath),
      0,
      1)
  ) %>% 
  filter(baby_dead==1) %>% 
  mutate(
    diff_time_death_bact=difftime(
      DateOfDeath,
      data_collection_date, 
      units = "days")
  ) %>%
  mutate_if(is.difftime,as.integer) %>% 
  mutate(
    diff_time_death_bact_grp=case_when(
      diff_time_death_bact %in% c(0,1,2,3,4,5,6,7) ~ "0-7 days",
      diff_time_death_bact >=8 & diff_time_death_bact<31 ~"8-30 days",
      diff_time_death_bact >=31 & diff_time_death_bact<61 ~"31-60 days",
      diff_time_death_bact >=61 & diff_time_death_bact<91 ~"61-90 days",
      diff_time_death_bact >=91  ~"90+ days",
      TRUE ~ "NA")
  ) %>% 
  mutate(
    death_90 = case_when(
      diff_time_death_bact <91 ~ 1, #baby died within 90 days of the diagnosed GNS
      diff_time_death_bact >=91 ~ 0,
      is.na(diff_time_death_bact) ~ 0 )
  ) %>% 
  mutate(
    diff_time_death_bact_grp=case_when(
      diff_time_death_bact %in% c(0,1,2,3,4,5,6,7) ~ "0-7 days",
      diff_time_death_bact >=8 & diff_time_death_bact<31 ~"8-30 days",
      diff_time_death_bact >=31 & diff_time_death_bact<61 ~"31-60 days",
      diff_time_death_bact >=61 & diff_time_death_bact<91 ~"61-90 days",
      diff_time_death_bact >=91  ~"90+ days",
      diff_time_death_bact <0 ~ "post-mortem",   ##negative values in the difference
      is.na(diff_time_death_bact) ~ "survived",
      TRUE ~ "NA")) %>%
  #now I will create a variable to filter for death within 30 days of first diagnosis
  mutate(
    death_30 = case_when(
      diff_time_death_bact <31 ~ 1, #baby died within 30 days of the diagnosed GNS
      diff_time_death_bact >=31 ~ 0,
      is.na(diff_time_death_bact) ~ 0 )
  ) %>% 
  dplyr::select(nhs,
         id,
         DateOfDeath,
         baby_dead,
         diff_time_death_bact,
         diff_time_death_bact_grp,
         death_90,
         death_30)




##merge datasets

kleb_all_joined <- left_join(kleb_postcodes,
                           kleb_vitc,
                           by = c("id"))

