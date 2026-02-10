##CLEAN postcodes dataset
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  data.table,
  skimr,
  string
)
##We select english postcodes only
source("paths/import_datasets.R")


england <- postcodes %>% 
  filter(ctry %in% c("E92000001", "W92000004", "S92000003", "N92000002")) %>% 
  mutate(
    pcd = gsub("\\s+", "", pcd)
  ) %>% 
  select(pcd, ctry, rgn, ru11ind, imd
         ) %>% 
  mutate(
    address = case_when(
      rgn == "E12000001" ~ "North East",
      rgn == "E12000002" ~ "North West",
      rgn == "E12000003" ~ "Yorkshire and the Humber",
      rgn == "E12000004" ~ "East Midlands",
      rgn == "E12000005" ~ "West Midlands",
      rgn == "E12000006" ~ "East of England",
      rgn == "E12000007" ~ "London",
      rgn == "E12000008" ~ "South East",
      rgn == "E12000009" ~ "South West",
      ctry == "W92000004" ~ "Wales",
      ctry == "S92000003" ~ "Scotland",
      ctry == "N92000002" ~ "Northen Ireland",
      TRUE ~ rgn  # Keep original value if no match
    )
  ) %>% 
  mutate(
    urban_rural = case_when(
      ru11ind %in% c("A1", "B1", "C1", "C2") ~ "urban",
      ru11ind %in% c("D1", "D2", "E1", "E2", "F1", "F2") ~ "rural"
    ) 
  ) %>% 
  mutate(
    imd_quintile = case_when(
      ctry=="E92000001" & imd<=6568 ~ "1st",
      ctry=="E92000001" & imd<=13137 & imd>=6569 ~"2nd",
      ctry=="E92000001" & imd<=19706 & imd>=13138 ~"3rd",
      ctry=="E92000001" & imd<=26275 & imd>=19707 ~"4th",
      ctry=="E92000001" & imd<= 32844 & imd>=26276 ~"5th",
      ctry=="E92000001" & is.na(imd) ~ "Unknown",
      ctry=="W92000004" & imd<=382 ~ "1st",
      ctry=="W92000004" & imd<=763 & imd>=383 ~ "2nd",
      ctry=="W92000004" & imd<=1145 & imd>=764 ~ "3rd",
      ctry=="W92000004" & imd<=1527 & imd>=1146 ~ "4th",
      ctry=="W92000004" & imd<=1909 & imd>=1528 ~ "5th",
      ctry=="W92000004" & is.na(imd) ~ "Unknown"
      )
    )
  

england <- as.data.frame(england) %>% 
  rename(homeadd = pcd)


##select variables of interest

##merging dataframes

ec_postcodes <- left_join(ec_clean,
                          england,
                          by = c("homeadd"))
