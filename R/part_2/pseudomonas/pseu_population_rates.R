###getting denominator data for rate calculations from gitlab

source("paths/import_datasets.R")


# Install from gitlab
if (!require(remotes)) install.packages("remotes")
remotes::install_gitlab(repo = "packages/DataLakeR",
                        host = "https://gitlab.phe.gov.uk",
                        upgrade = "always",
                        INSTALL_opts = '--no-lock')

library(DataLakeR)
library(dplyr)
library(janitor)


con1 <- dbConnect(odbc(), Driver = "SQL Server",
                  Server = "SQLCLUSCOLHPO19\\HPO19",
                  Database = "Populations",
                  Trusted_connection = "TRUE")

kleb_pseu_regions_pop <- paste0("SELECT
      [GeoName]
      ,[Period]
      , [Age]
      ,sum([Population]) AS Population
      FROM [Populations].[dbo].[vRes21_RGN09_SingleYear]
      WHERE Period BETWEEN 2017 AND 2019
      GROUP BY GeoName, Period, Age")

#Now I create a filtered object in R that I can use to calculate rates

kleb_pseu_regions_pop <- odbc::dbGetQuery(con1,kleb_pseu_regions_pop) %>%
  clean_names

kleb_pseu_popdenom <- kleb_pseu_regions_pop %>% 
  filter(age==0) %>% 
  mutate(period = as.numeric(period)) %>% 
  mutate(geo_name = case_when(
    geo_name == "Yorkshire and The Humber" ~ "Yorkshire and the Humber",
    TRUE ~ geo_name
  ))


##same for the ec dataset
pacman::p_load(
  rio,       # to import data
  here,      # to locate files
  tidyverse, # to clean, handle, and plot the data (includes dplyr)
  janitor)   # adding total rows and columns


pseu_group <- pseu_final %>% 
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
  group_by(address, year_no) %>% 
  tally()

pseu_rate <- left_join(pseu_group,
                       kleb_pseu_popdenom,
                       by= c("address"="geo_name",
                             "year_no"="period"))

#I will also add death rate
pseu_deathrate <- pseu_final %>% 
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
  filter(death_90 == 1) %>% 
  mutate(year_no = as.numeric(year_no)
  ) %>% 
  group_by(address, year_no) %>% 
  tally()

pseu_rates <- left_join(pseu_rate,
                        pseu_deathrate,
                        by= c("address",
                              "year_no"))

##case rate per 100K children under 1
pseu_rates <- pseu_rates %>%
  # rename(cases = n.x,
  #        deaths = n.y) %>% 
  group_by(address, year_no) %>%  # Keep data grouped by region and year
  mutate(cases = as.numeric(n.x),
         deaths = as.numeric(n.y),
         population = as.numeric(population),
         case_rate = (cases / population) * 100000,
         death_rate = (deaths/population) * 100000,
         deaths_cases = (deaths/cases)*1000)

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
##Now make a table with those results

pseu_caserates_tbl <- pseu_rates %>% 
  mutate(case_rate = round(case_rate, 2)
  ) %>% 
  select(address, year_no, case_rate) %>% 
  pivot_wider(names_from = year_no, values_from = case_rate) %>% 
  gt()
# quite ugly but it does the job
pseu_deathrates_tbl <- pseu_rates %>% 
  mutate(death_rate = round(death_rate, 2)
  ) %>% 
  select(address, year_no, death_rate) %>% 
  pivot_wider(names_from = year_no, values_from = death_rate) #%>% 
gt()

##summary stats
pseu_rate_numerator <- pseu_final %>% 
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
  group_by(address) %>% 
  tally()

##now for deaths
pseu_death_numerator <- pseu_final %>% 
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
  filter(death_90 == 1) %>% 
  group_by(address) %>% 
  tally()

kleb_pseu_region_denominator <- kleb_pseu_popdenom %>% 
  group_by(geo_name) %>% 
  mutate(population = as.numeric(population)
  ) %>% 
  mutate(population_reg = sum(population, na.rm = TRUE)
  ) 

kleb_pseu_region_denominator <- kleb_pseu_popdenom %>% 
  group_by(geo_name) %>% 
  mutate(population = as.numeric(population)
  ) %>% 
  summarise(population_reg = sum(population, na.rm = TRUE)
  ) 

pseu_total_rates <- left_join(pseu_rate_numerator,
                              kleb_pseu_region_denominator,
                              by = c("address" = "geo_name"))
pseu_rate_final <- left_join(pseu_total_rates,
                             pseu_death_numerator,
                             by = "address")

pseu_totalrates <- pseu_rate_final %>% 
  group_by(address) %>%  # Keep data grouped by region and year
  mutate(cases = as.numeric(n.x),
         deaths = as.numeric(n.y),
         population_reg = as.numeric(population_reg),
         case_rate = (cases / population_reg) * 100000,
         death_rate = (deaths/population_reg) * 100000,
         deaths_cases = (deaths/cases)*1000,
         case_rate = round(case_rate, 2),
         death_rate = round(death_rate, 2),
         deaths_cases = round(deaths_cases,2))


pseu_pop_rates<- pseu_totalrates %>% 
  select(address,
         cases,
         case_rate,
         deaths,
         death_rate
  ) %>% 
  mutate(variable = case_when(
    cases >= 0 ~ "address" 
  )) %>% 
  rename(
    incidence = case_rate,
    mortality_rate = death_rate,
    group = address
  )


###make a nice flextable:

pseu_regtable <- pseu_totalrates %>% 
  select(
    address,
    population_reg,
    cases,
    case_rate,
    deaths,
    death_rate,
    deaths_cases) %>% 
  arrange(-case_rate)


pseu_flexreg <- flextable(pseu_regtable)


border_style = officer::fp_border(color="black", width=1)

pseu_flexreg <- pseu_flexreg %>% 
  autofit() %>% 
  add_header_row(
    top = TRUE,
    values = c("",
               "",
               "Bacteremia events",
               "",
               "90d Deaths",
               "",
               "")
  ) %>% 
  set_header_labels(
    address = "Region of address",
    population_reg = "Population",
    cases = "Total",
    case_rate = "Rate per 100,000",
    deaths = "Total",
    death_rate = "Rate per 100,000",
    deaths_cases = "Case-fatality rate per 1,000"
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


##now we save with the other

save_as_docx(
  "Regional trends in Pseudomonas aeruginosa bacteremia among infants in England: incidence, mortality and case-fatality rates for the period 2011-2019" = pseu_flexreg,
  "Trends in Pseudomonas aeruginosa bacteremia among infants in England: incidence, mortality and case-fatality rate for the period 2011-2019 by mother ethnicity" = pseu_flexteh,
  path = here("outputs", "pseu_rates_tables.docx"))
