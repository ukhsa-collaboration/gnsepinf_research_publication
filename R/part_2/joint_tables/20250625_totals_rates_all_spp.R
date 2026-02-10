##Totals rate row


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


livebirths11 <- livebirths %>% 
  clean_names() %>% 
  mutate_at(
    vars(live_births),
    ~ as.numeric(gsub("[^0-9.-]", "", .))
  ) %>% 
  summarise(total_births = sum(live_births, na.rm=TRUE))

livebirths17 <- livebirths %>% 
  clean_names() %>% 
  filter(year == c(2017, 2018, 2019)) %>% 
  mutate_at(
    vars(live_births),
    ~ as.numeric(gsub("[^0-9.-]", "", .))
  ) %>% 
  summarise(total_births = sum(live_births, na.rm=TRUE))


dataframe<- data.frame(species = c ("ec", "kleb", "pseu"),
                       cases = c(3115, 218, 62),
                       deaths = c(300, 23, 19),
                       population = c(6145670, 656723, 656723)
                       ) %>% 
  mutate(case_rate = (cases / population) * 100000,
         death_rate = (deaths/population) * 100000,
         CFR = (deaths/cases))


