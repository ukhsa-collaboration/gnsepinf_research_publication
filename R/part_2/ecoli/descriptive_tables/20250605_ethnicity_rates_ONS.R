###cleaning of ONS datasets

pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  skimr
)

regions <- ons_regions %>% 
  clean_names() %>% 
  select(region_name, month_of_birth, x2001:x2019) %>% 
  mutate_at(
    vars(x2011:x2019),
    ~ as.numeric(gsub("[^0-9.-]", "", .))
  )
  #mutate_at(vars(x2011, x2012, x2013, x2014, x2015, x2016, x2017, x2018, x2019), as.numeric)

regions <- regions %>% 
  group_by(region_name) %>% 
  summarise(across(x2011:x2019, sum, na.rm = TRUE))


onseth <- ons_eth %>% 
  clean_names() %>% 
  mutate_at(
    vars(live_births_total: not_stated),
    ~ as.numeric(gsub("[^0-9.-]", "", .))
  )

onseth <- onseth %>% 
  group_by(year) %>% 
  summarise(across(live_births_total:not_stated, ~ sum(as.numeric(.), na.rm = TRUE)), .groups = "drop")


onsimd <- imdbirth %>% 
  clean_names()


### ethnicity figures
eth_live <- onseth %>% 
  pivot_longer(
    cols = 2:13,
    names_to = "ethnicity",
    values_to = "live_births"
  ) %>% 
  select(year, ethnicity, live_births) %>% 
  #clean_names() %>% 
  # filter(ethnicity %in% c("pakistani",
  #                         "blacka_african",
  #                         "black_caribbean",
  #                         "other_black",
  #                         "white_british",
  #                         "white_other",
  #                         "any_other_asian",
  #                         "bangladeshi",
  #                         "indian",
  #                         "all_others",
  #                         "other",
  #                         "live_births_total",
  #                         "mixed"
  # )
  # ) #%>% 
mutate(ethnos = case_when(
  ethnicity == "bangladeshi" ~ "Bangladeshi",
  ethnicity == "indian" ~ "Indian",
  ethnicity == "pakistani" ~ "Pakistani",
  ethnicity == "white_other" ~ "White other",
  ethnicity == "white_british" ~ "White British",
  ethnicity == "black_african" ~ "Black African",
  ethnicity == "black_caribbean" ~ "Black Caribbean",
  ethnicity == "any_other_asian" ~ "Asian other",
  ethnicity == "mixed" ~ "Mixed",
  ethnicity == "other_black" ~ "Black other",
  ethnicity == "other" ~ "Other ethnicity"
)
) %>% 
  filter(ethnicity != "live_births_total") %>% 
  filter(year %in% 2011:2019)
## so this is for ecoli

eth11 <- eth_live %>%   
  group_by(ethnos) %>% 
  summarise(population_ethn11 = sum(live_births, na.rm = TRUE))


##now we take the data from the ec final dataset

ec_ethnos <- ec_final %>% 
  drop_na(any_of(c("ethnicity")
  )
  ) %>% 
  mutate(ethnicity = case_when(
    ethnicity == "Chinese" ~ "Asian other",
    ethnicity == "Black British" ~ "Black other",
    ethnicity == "mixed" ~ "Mixed",
    TRUE ~ ethnicity
  )) %>% 
  group_by(ethnicity) %>% 
  tally()


ec_eth_rate <- left_join(ec_ethnos,
                         eth11,
                         by = c("ethnicity" = "ethnos"))

ec_eth_rate <- ec_eth_rate %>% 
  mutate( n = as.numeric(n),
          case_rate = (n/population_ethn11)*100000,
          case_rate = round(case_rate, digits=2))

##now for death rates
ec_death_eth_num <- ec_final %>% 
  filter(death_30 == 1) %>% 
  drop_na(any_of(c("ethnicity")
  )
  ) %>% 
  mutate(ethnicity = case_when(
    ethnicity == "Chinese" ~ "Asian other",
    ethnicity == "Black British" ~ "Black other",
    ethnicity == "mixed" ~ "Mixed",
    TRUE ~ ethnicity
  )) %>% 
  group_by(ethnicity) %>% 
  tally()

ec_eth_death <- left_join(eth11,
                          ec_death_eth_num,
                          by = c("ethnos" = "ethnicity"))

ec_eth_death <- ec_eth_death %>% 
  mutate(deaths = as.numeric(n),
         death_rate = (deaths/population_ethn11) *100000,
         death_rate = round(death_rate, 2)
  ) %>% 
  replace_na(list(deaths = 0, n=0, death_rate = 0))


ec_eth_rate <- ec_eth_rate %>% 
  mutate(cases = as.numeric(n),
         case_rate = (cases/population_ethn11)*100000,
         case_rate = round(case_rate, 2)) %>% 
  mutate(cases = case_when(
    cases== NA_real_ ~0,
    TRUE ~cases
  )
  )


ec_ethtable <- left_join(ec_eth_death,
                         ec_eth_rate,
                         by= c("ethnos"="ethnicity"))


ec_ethtable <- ec_ethtable %>% 
  mutate(case_fatality = (deaths/cases)*100,
         case_fatality = round(case_fatality, 1))

##table tor merging
ec_ethrates_table <- ec_ethtable %>% 
  select(ethnos,
         cases,
         case_rate,
         deaths,
         death_rate,
         population_ethn11.x
  ) %>% 
  mutate(variable = case_when(
    cases >= 1 ~ "ethnicity" 
  )) %>% 
  rename(
    incidence = case_rate,
    mortality_rate = death_rate,
    group = ethnos
  ) 

###Statistical test to compare incidence rates
#table for poisson
ec_ethnicity_poisson <- ec_ethrates_table %>% 
  mutate(livebirths = population_ethn11.x) %>% 
  mutate(
    group = fct_relevel(
      group,
      "White British",
      after = 0
    ) %>% 
      factor(levels=c(
        "White British",
        "White other",
        "Black African",
        "Black Caribbean",
        "Black other",
        "Indian",
        "Bangladeshi",
        "Pakistani",
        "Asian other",
        "Mixed",
        "Other ethnicity")
      )
  ) 
  

##poisson

ec_ethnpoisson <- glm(cases ~ group,
                      offset = log(livebirths),
                      #family = quasipoisson(link = "log"),
                      family = poisson(link = "log"),
                      data = ec_ethnicity_poisson)


ec_eth_poisson_tbl <- gtsummary::tbl_regression(ec_ethnpoisson, exponentiate = TRUE)


ec_eth_tbl_custom <- ec_eth_poisson_tbl %>%
  modify_table_body(
    ~ .x %>%
      mutate(
        `IRR (95% CI)` = ifelse(
          !is.na(estimate),
          sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
          NA
        )
      )
  ) %>%
  modify_column_hide(columns = c(estimate, conf.low, conf.high)) %>%
  modify_header(`IRR (95% CI)` ~ "**IRR (95% CI)**")

