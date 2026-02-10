### rates by IMD for pseu

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


pseu_imd_live <- imdbirth %>% 
  pivot_longer(
    cols = 2:6,
    names_to = "IMD quintile",
    values_to = "live_births"
  ) %>% 
  clean_names() %>% 
  filter(year %in% c(2017,
                     2018,
                     2019)
  ) %>% 
  mutate(imd_quintile = case_when(
    imd_quintile == "1" ~ "1st",
    imd_quintile == "2" ~ "2nd",
    imd_quintile == "3" ~ "3rd",
    imd_quintile == "4" ~ "4th",
    imd_quintile == "5" ~ "5th",
    TRUE ~ imd_quintile
  )
  )
skim(pseu_imd_live$live_births)

#eth_live$live_births <- as.numeric(gsub(",","", eth_live$live_births))



pseu_imd11 <- pseu_imd_live %>%   
  group_by(imd_quintile) %>% 
  summarise(population_imd11 = sum(live_births, na.rm = TRUE))


##now we take the data from the pseu final dataset

pseu_imd <- pseu_final %>% 
  drop_na(any_of(c("imd_quintile")
  )
  ) %>% 
  # mutate(ethnicity = case_when(
  #   ethnicity %in% c("Asian other", 
  #                    "Other ethnicity",
  #                    "mixed",
  #                    "Chinese",
  #                    "Black British") ~ "All Others",
  #   ethnicity %in% c("White other") ~ "White Other",
  #   TRUE ~ ethnicity
  # )) %>% 
  group_by(imd_quintile) %>% 
  tally()


pseu_imd_rate <- left_join(pseu_imd,
                           pseu_imd11,
                           by = "imd_quintile")

pseu_imd_rate <- pseu_imd_rate %>% 
  mutate( case = as.numeric(n),
          case_rate = (n/population_imd11)*100000,
          case_rate = round(case_rate, digits=2))

##now for death rates
pseu_death_imd_num <- pseu_final %>% 
  filter(death_30 == 1) %>% 
  drop_na(any_of(c("imd_quintile")
  )
  ) %>% 
  # mutate(ethnicity = case_when(
  #   ethnicity %in% c("Asian other", 
  #                    "Other ethnicity",
  #                    "mixed",
  #                    "Chinese",
  #                    "Black British") ~ "All Others",
  #   ethnicity %in% c("White other") ~ "White Other",
  #   TRUE ~ ethnicity
  # )) %>% 
  group_by(imd_quintile) %>% 
  tally()

pseu_imd_death <- left_join(pseu_imd11,
                            pseu_death_imd_num,
                            by = c("imd_quintile"))

pseu_imd_death <- pseu_imd_death %>% 
  mutate(deaths = as.numeric(n),
         death_rate = (deaths/population_imd11) *100000,
         death_rate = round(death_rate, 2)
  ) %>% 
  replace_na(list(deaths = 0, n=0, death_rate = 0))


# pseu_imd_death <- pseu_imd_death %>% 
#   mutate(cases = as.numeric(n),
#          case_rate = (cases/population_imd11)*100000,
#          case_rate = round(case_rate, 2)) %>% 
#   mutate(cases = case_when(
#     cases== NA_real_ ~0,
#     TRUE ~cases
#   )
#   )


pseu_imdtable <- left_join(pseu_imd_death,
                           pseu_imd_rate,
                           by= c("imd_quintile"))


pseu_imdtable <- pseu_imdtable %>% 
  mutate(case_fatality = (deaths/case)*100,
         case_fatality = round(case_fatality, 1))

pseu_imdtable_paper <- pseu_imdtable %>% 
  select(imd_quintile,
         case,
         case_rate,
         deaths,
         death_rate
  ) %>% 
  mutate(variable = case_when(
    case >= 0 ~ "IMD quintile" 
  )) %>% 
  rename(
    incidence = case_rate,
    mortality_rate = death_rate,
    group = imd_quintile,
    cases = case
  )


###########
#old tables

###Making a table for a word presentation

pseuimdtable <- pseu_imdtable %>% 
  select(
    imd_quintile,
    population_imd11.x,
    case,
    case_rate,
    deaths,
    death_rate,
    case_fatality) %>% 
  arrange(-case_rate)

pseu_fleximd <- flextable(pseuimdtable)


border_style = officer::fp_border(color="black", width=1)

pseu_fleximd <- pseu_fleximd %>% 
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
    imd_quintile = "IMD quintile",
    population_imd11.x = "Live births 2017-2019",
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

print(pseu_fleximd, preview = "docx")





pseu_imd_poisson <- pseuimdtable %>% 
  mutate(livebirths = population_imd11.x) %>% 
  mutate(
    imd_quintile = fct_relevel(
      imd_quintile,
      "5th",
      after = 0
    ) %>% 
      factor(levels=c(
        "5th",
        "4th",
        "3rd",
        "2nd",
        "1st")
      )
  ) 


##poisson

pseu_imdpoisson <- glm(case ~ imd_quintile,
                       offset = log(livebirths),
                       #family = quasipoisson(link = "log"),
                       family = poisson(link = "log"),
                       data = pseu_imd_poisson)

pseu_imd_poisson_tbl <- gtsummary::tbl_regression(pseu_imdpoisson, exponentiate = TRUE)



pseu_imd_tbl_custom <- pseu_imd_poisson_tbl %>%
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
