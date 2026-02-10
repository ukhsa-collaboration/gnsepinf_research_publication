###RATES for gestational week groups

pacman::p_load(janitor,
               dplyr,
               tidyr,
               tidyverse,
               gtsummary,
               rio)

ons_gestage <- ons_gestage %>% 
  rename(gestweek = ...1) %>% 
  slice(-1, -24, -25) %>% 
  select(-"2010")
  

ec_ons_gestage <- ons_gestage %>% 
  pivot_longer(
    cols = -gestweek,
    names_to = "year",
    values_to = "live_births"
  ) %>% 
  mutate(
    gestage_group = case_when(
      gestweek %in% c("Under 22 weeks and birthweight less than 1,000g",
                      "22 weeks",
                      "23 weeks",
                      "24 weeks",
                      "25 weeks",
                      "26 weeks",
                      "27 weeks") ~ "Extremely pre-term (<28wks)",
      gestweek %in% c("28 weeks",
                      "29 weeks",
                      "30 weeks",
                      "31 weeks") ~ "Very pre-term (28-31wks)",
      gestweek %in% c("32 weeks",
                      "33 weeks",
                      "34 weeks",
                      "35 weeks",
                      "36 weeks") ~ "Moderate pre-term (32-36wks)",
      gestweek %in% c("37 weeks",
                      "38 weeks",
                      "39 weeks",
                      "40 weeks",
                      "41 weeks",
                      "42 weeks and above") ~ "Term (37+wks)"
    )
  ) %>% 
  group_by(gestage_group) %>% 
  summarise(total_births = sum(live_births, na.rm = TRUE), .groups = "drop")

kleb_pseu_gestage <- ons_gestage %>% 
  select("gestweek",
         "2019",
         "2018",
         "2017") %>% 
  pivot_longer(
    cols = -gestweek,
    names_to = "year",
    values_to = "live_births"
  ) %>% 
  mutate(
    gestage_group = case_when(
      gestweek %in% c("Under 22 weeks and birthweight less than 1,000g",
                      "22 weeks",
                      "23 weeks",
                      "24 weeks",
                      "25 weeks",
                      "26 weeks",
                      "27 weeks") ~ "Extremely pre-term (<28wks)",
      gestweek %in% c("28 weeks",
                      "29 weeks",
                      "30 weeks",
                      "31 weeks") ~ "Very pre-term (28-31wks)",
      gestweek %in% c("32 weeks",
                      "33 weeks",
                      "34 weeks",
                      "35 weeks",
                      "36 weeks") ~ "Moderate pre-term (32-36wks)",
      gestweek %in% c("37 weeks",
                      "38 weeks",
                      "39 weeks",
                      "40 weeks",
                      "41 weeks",
                      "42 weeks and above") ~ "Term (37+wks)"
    )
  ) %>% 
  group_by(gestage_group) %>% 
  summarise(total_births = sum(live_births, na.rm = TRUE), .groups = "drop")


##EC rates calculations
ec_gestage <- ec_final %>%
  drop_na(any_of(c("gestage_grp")
  )
  ) %>% 
  mutate(gestage_grp = case_when(
    gestage_grp == "Term" ~ "Term (37+wks)",
    gestage_grp == "Moderate Preterm" ~ "Moderate pre-term (32-36wks)",
    gestage_grp == "Very Preterm" ~ "Very pre-term (28-31wks)",
    gestage_grp == "Extremely Preterm" ~ "Extremely pre-term (<28wks)"
  ),
  gestage_grp = factor(gestage_grp,
                       levels = c("Extremely pre-term (<28wks)",
                                  "Very pre-term (28-31wks)",
                                  "Moderate pre-term (32-36wks)",
                                  "Term (37+wks)"
                                  )
                       )
  ) %>% 
  group_by(gestage_grp) %>% 
  tally() %>% 
  rename(cases = n)
  
ec_gestage_death <- ec_final %>% 
  filter(death_30 == 1) %>% 
  drop_na(any_of(c("gestage_grp")
  )
  ) %>% 
  mutate(gestage_grp = case_when(
    gestage_grp == "Term" ~ "Term (37+wks)",
    gestage_grp == "Moderate Preterm" ~ "Moderate pre-term (32-36wks)",
    gestage_grp == "Very Preterm" ~ "Very pre-term (28-31wks)",
    gestage_grp == "Extremely Preterm" ~ "Extremely pre-term (<28wks)"
  ),
  gestage_grp = factor(gestage_grp,
                       levels = c("Extremely pre-term (<28wks)",
                                  "Very pre-term (28-31wks)",
                                  "Moderate pre-term (32-36wks)",
                                  "Term (37+wks)"
                       )
  )
  ) %>% 
  group_by(gestage_grp) %>% 
  tally() %>% 
  rename(deaths = n)

ec_gestage_join <- left_join(ec_gestage,
                             ec_gestage_death,
                             by = "gestage_grp")
ec_gestage_join <- left_join(ec_gestage_join,
                             ec_ons_gestage,
                             by = c("gestage_grp"="gestage_group"))

ec_gestage_join <- ec_gestage_join %>% 
  mutate(case_rate = (cases/total_births) *100000,
         case_rate = round(case_rate, 2),
         death_rate = (deaths/total_births) *100000,
         death_rate = round(death_rate, 2)
)     

###Statistical test to compare incidence rates
#table for poisson
ec_gestage_poisson <- ec_gestage_join %>%
  mutate(
    gestage_grp = fct_relevel(
      gestage_grp,
      "Term (37+wks)",
      after = 0
    ) %>% 
      factor(levels=c(
        "Term (37+wks)",
        "Moderate pre-term (32-36wks)", 
        "Very pre-term (28-31wks)",
        "Extremely pre-term (<28wks)"
        )
      )
  ) 


##poisson

ec_gestagepoisson <- glm(cases ~ gestage_grp,
                      offset = log(total_births),
                      #family = quasipoisson(link = "log"),
                      family = poisson(link = "log"),
                      data = ec_gestage_poisson)


ec_gest_poisson_tbl <- gtsummary::tbl_regression(ec_gestagepoisson, exponentiate = TRUE)


ec_gest_tbl_custom <- ec_gest_poisson_tbl %>%
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
