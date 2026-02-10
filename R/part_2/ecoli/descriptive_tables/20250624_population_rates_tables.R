## rates of incidence by region of origin
## with data from ONS

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
  gtsummary,
  dplyr
)

regions <- ons_regions %>% 
  clean_names() %>% 
  select(region_name, month_of_birth, x2011:x2019) %>% 
   
#mutate_at(vars(x2011, x2012, x2013, x2014, x2015, x2016, x2017, x2018, x2019), as.numeric)

regions <- regions %>% 
  mutate(region_name = case_when(
    region_name == "East" ~ "East of England",
    TRUE ~ region_name
  )) %>% 
  group_by(region_name) %>% 
  summarise(across(x2011:x2019, sum, na.rm = TRUE))


regions_birth <- regions %>% 
  pivot_longer(
    cols = 2:10,
    names_to = "year",
    values_to = "live_births"
  ) %>% 
  group_by(region_name) %>% 
  summarise(total_births = sum(live_births, na.rm = TRUE))
  


ec_group <- ec_final %>% 
  filter(address %in% c("East of England",
                        "East Midlands",
                        "London",
                        "North East",
                        "North West",
                        "South East",
                        "South West",
                        "West Midlands",
                        "Yorkshire and the Humber")
  ) %>% 
  mutate(year_no = as.numeric(year_no)
  ) %>% 
  group_by(address) %>% 
  tally()

ecrate <- left_join(ec_group,
                    regions_birth,
                    by= c("address"="region_name"))

#I will also add death rate
ec_deathrate <- ec_final %>% 
  filter(address %in% c("East of England",
                        "East Midlands",
                        "London",
                        "North East",
                        "North West",
                        "South East",
                        "South West",
                        "West Midlands",
                        "Yorkshire and the Humber")
  ) %>% 
  filter(death_30 == 1) %>% 
  mutate(year_no = as.numeric(year_no)
  ) %>% 
  group_by(address) %>% 
  tally()

ecrates <- left_join(ecrate,
                     ec_deathrate,
                     by= c("address"))

##case rate per 100K children under 1
ecrates_table <- ecrates %>%
  rename(cases = n.x,
         deaths = n.y) %>% 
  group_by(address) %>%  # Keep data grouped by region and year
  mutate(cases = as.numeric(cases),
         deaths = as.numeric(deaths),
         population = as.numeric(total_births),
         case_rate = (cases / total_births) * 100000,
         death_rate = (deaths/total_births) * 100000,
         CFR = (deaths/cases))

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
#


###make a nice flextable:

ecregtable <- ecrates_table %>% 
  select(
    address,
    total_births,
    cases,
    case_rate,
    deaths,
    death_rate,
    CFR) %>% 
  arrange(-case_rate)


ec_flexreg <- flextable(ecregtable)


border_style = officer::fp_border(color="black", width=1)

ec_flexreg <- ec_flexreg %>% 
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
    address = "Region of address",
    total_births = "Live births",
    cases = "Total",
    case_rate = "Rate per 100,000",
    deaths = "Total",
    death_rate = "Rate per 100,000",
    deaths_cases = "Case-fatality rate (%)"
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



##table for merging:

ec_pop_rates<- ecrates_table %>% 
  select(address,
         cases,
         case_rate,
         deaths,
         death_rate
  ) %>% 
  mutate(variable = case_when(
    cases >= 1 ~ "address" 
  )) %>% 
  rename(
    incidence = case_rate,
    mortality_rate = death_rate,
    group = address
  )




###Statistical test to compare incidence rates
#table for poisson
ec_region_poisson <- ecregtable %>% 
  mutate(livebirths = 6145670) %>% 
  mutate(address = as.factor(address)) %>% 
  mutate(
    address = fct_relevel(
      address,
      "London",
      after = 0
    ) %>% 
      factor(levels=c(
        "London",
        "East of England",
        "East Midlands",
        "West Midlands",
        "North East",
        "North West",
        "South East",
        "South West",
        "Yorkshire and the Humber"
        )
      )
  ) 


##poisson

ec_regpoisson <- glm(cases ~ address,
                      offset = log(total_births),
                      #family = quasipoisson(link = "log"),
                      family = poisson(link = "log"),
                      data = ec_region_poisson)

ec_reg_poisson_tbl <- gtsummary::tbl_regression(ec_regpoisson, exponentiate = TRUE)



# Add a custom column with IRR (95% CI) formatting

ec_reg_tbl_custom <- ec_reg_poisson_tbl %>%
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



ec_reg_ft <- as_flextable(ec_reg_tbl_custom)




