#### ethnicity rates for pseudomonas


pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,     # converting tables to pretty images
  gt
)
source("paths/import_datasets.R")


pseu_eth_live <- livebirths %>% 
  pivot_longer(
    cols = 2:12,
    names_to = "ethnicity",
    values_to = "live_births"
  ) %>% 
  clean_names() %>% 
  filter(ethnicity %in% c("Pakistani",
                          "Black African",
                          "Black Caribbean",
                          "White British",
                          "White Other",
                          "Bangladeshi",
                          "Indian",
                          "All Others",
                          "Live births"
  )
  ) %>% 
  filter(year %in% c(2019, 2018, 2017))


skim(eth_live$live_births)

pseu_eth_live$live_births <- as.numeric(gsub(",","", pseu_eth_live$live_births))



pseu_eth14 <- pseu_eth_live %>%   
  group_by(ethnicity) %>% 
  summarise(population_ethn14 = sum(live_births, na.rm = TRUE))


##now we take the data from the ec final dataset

pseu_ethnos <- pseu_final %>% 
  drop_na(any_of(c("ethnicity")
  )
  ) %>% 
  mutate(ethnicity = case_when(
    ethnicity %in% c("Asian other", 
                     "Other ethnicity",
                     "mixed",
                     "Chinese",
                     "Black British") ~ "All Others",
    ethnicity %in% c("White other") ~ "White Other",
    TRUE ~ ethnicity
  )) %>% 
  group_by(ethnicity) %>% 
  tally()


pseu_eth_rate <- left_join(pseu_ethnos,
                           pseu_eth14,
                           by = "ethnicity")

pseu_eth_rate <- pseu_eth_rate %>% 
  mutate( n = as.numeric(n),
          case_rate = (n/population_ethn14)*100000,
          case_rate = round(case_rate, digits=2))

##now for death rates
pseu_death_eth_num <- pseu_final %>% 
  filter(death_30 == 1) %>% 
  drop_na(any_of(c("ethnicity")
  )
  ) %>% 
  mutate(ethnicity = case_when(
    ethnicity %in% c("Asian other", 
                     "Other ethnicity",
                     "mixed",
                     "Chinese",
                     "Black British") ~ "All Others",
    ethnicity %in% c("White other") ~ "White Other",
    TRUE ~ ethnicity
  )) %>% 
  group_by(ethnicity) %>% 
  tally()

pseu_eth_death <- left_join(pseu_eth14,
                            pseu_death_eth_num,
                            by = c("ethnicity"))

pseu_eth_death <- pseu_eth_death %>% 
  mutate(deaths = as.numeric(n),
         death_rate = (deaths/population_ethn14) *100000,
         death_rate = round(death_rate, 2)
  ) %>% 
  replace_na(list(deaths = 0, n=0, death_rate = 0))


pseu_eth_rate <- pseu_eth_rate %>% 
  mutate(cases = as.numeric(n),
         case_rate = (cases/population_ethn14)*100000,
         case_rate = round(case_rate, 2)) %>% 
  mutate(cases = case_when(
    cases== NA_real_ ~0,
    TRUE ~cases
  )
  )


pseu_ethtable <- left_join(pseu_eth_death,
                           pseu_eth_rate,
                           by= c("ethnicity"))


pseu_ethtable <- pseu_ethtable %>% 
  mutate(case_fatality = (deaths/cases)*100,
         case_fatality = round(case_fatality, 1))

pseu_ethrates_table <- pseu_ethtable %>% 
  select(ethnicity,
         cases,
         case_rate,
         deaths,
         death_rate
  ) %>% 
  mutate(variable = case_when(
    cases >= 0 ~ "ethnicity" 
  )) %>% 
  rename(
    incidence = case_rate,
    mortality_rate = death_rate,
    group = ethnicity
  ) %>% 
  filter(group != "Live births")

###############
#old table
###Making a table for a word presentation

pseuethtable <- pseu_ethtable %>% 
  select(
    ethnicity,
    population_ethn14.x,
    cases,
    case_rate,
    deaths,
    death_rate,
    case_fatality) %>% 
  arrange(-case_rate)

pseu_flexeth <- flextable(pseuethtable)


border_style = officer::fp_border(color="black", width=1)

pseu_flexteh <- pseu_flexeth %>% 
  autofit() %>% 
  add_header_row(
    top = TRUE,
    values = c("",
               "",
               "Bacteremia events",
               "",
               "30d Deaths",
               "",
               "")
  ) %>% 
  set_header_labels(
    ethnos = "Ethnicity",
    population_ethn14.x = "Live births 2011-2014",
    cases = "Total",
    case_rate = "Rate per 100,000",
    deaths = "Total",
    death_rate = "Rate per 100,000",
    case_fatality = "Case-fatality rate (%)"
  ) %>% 
  merge_at(i = 1, j=3:4, part = "header") %>% 
  merge_at(i=1, j=5:6, part="header") %>% 
  border_remove(
  ) %>% 
  theme_booktabs() %>% 
  vline(part="all", j=2, border=border_style) %>% 
  vline(part = "all", j=4, border=border_style) %>% 
  vline(part="all", j=6, border=border_style) %>% 
  align(align = "center", j= c(2:7), part = "all") %>% 
  bold(i=1, bold=TRUE, part="header") %>% 
  bold(j=1, bold=TRUE, part="body") %>% 
  bold(i=2, bold=TRUE, part="header")

print(pseu_flexteh, preview = "docx")
