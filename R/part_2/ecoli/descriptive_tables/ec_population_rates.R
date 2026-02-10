#getting denominator data for rate calculations from gitlab

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

regions_pop <- paste0("SELECT
      [GeoName]
      ,[Period]
      , [Age]
      ,sum([Population]) AS Population
      FROM [Populations].[dbo].[vRes21_RGN09_SingleYear]
      WHERE Period BETWEEN 2011 AND 2019
      GROUP BY GeoName, Period, Age")

#Now I create a filtered object in R that I can use to calculate rates

regions_pop <- odbc::dbGetQuery(con1,regions_pop) %>%
  clean_names

popdenom <- regions_pop %>% 
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
  group_by(address, year_no) %>% 
  tally()

ecrate <- left_join(ec_group,
                     popdenom,
                     by= c("address"="geo_name",
                           "year_no"="period"))

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
  group_by(address, year_no) %>% 
  tally()

ecrates <- left_join(ecrate,
                    ec_deathrate,
                    by= c("address",
                          "year_no"))

##case rate per 100K children under 1
ecrates <- ecrates %>%
   rename(cases = n.x,
          deaths = n.y) %>% 
  group_by(address, year_no) %>%  # Keep data grouped by region and year
  mutate(cases = as.numeric(cases),
         deaths = as.numeric(deaths),
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

eccaserates_tbl <- ecrates %>% 
  mutate(case_rate = round(case_rate, 2)
         ) %>% 
  select(address, year_no, case_rate) %>% 
  pivot_wider(names_from = year_no, values_from = case_rate) %>% 
  gt()
  # quite ugly but it does the job
ecdeathrates_tbl <- ecrates %>% 
  mutate(death_rate = round(death_rate, 2)
  ) %>% 
  select(address, year_no, death_rate) %>% 
  pivot_wider(names_from = year_no, values_from = death_rate) #%>% 
  gt()

##summary stats
  ec_rate_numerator <- ec_final %>% 
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
  ec_death_numerator <- ec_final %>% 
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
  
  region_denominator <- popdenom %>% 
    group_by(geo_name) %>% 
    mutate(population = as.numeric(population)
           ) %>% 
    mutate(population_reg = sum(population, na.rm = TRUE)
           ) 
    
  region_denominator <- popdenom %>% 
    group_by(geo_name) %>% 
    mutate(population = as.numeric(population)
    ) %>% 
    summarise(population_reg = sum(population, na.rm = TRUE)
    ) 
  
total_rates <- left_join(ec_rate_numerator,
                         region_denominator,
                         by = c("address" = "geo_name"))
ec_rate_final <- left_join(total_rates,
                           ec_death_numerator,
                           by = "address")

totalrates <- ec_rate_final %>% 
  group_by(address) %>%  # Keep data grouped by region and year
  mutate(cases = as.numeric(n.x),
         deaths = as.numeric(n.y),
         population_reg = as.numeric(population_reg),
         case_rate = (cases / population_reg) * 100000,
         death_rate = (deaths/population_reg) * 100000,
         deaths_cases = (deaths/cases)*100,
         case_rate = round(case_rate, 2),
         death_rate = round(death_rate, 2),
         deaths_cases = round(deaths_cases,2))

###make a nice flextable:

ecregtable <- totalrates %>% 
  select(
    address,
    population_reg,
    cases,
    case_rate,
    deaths,
    death_rate,
    deaths_cases) %>% 
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
    population_reg = "Population",
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


##now we save with the other

save_as_docx(
  "Regional trends in E.coli bacteremia among infants in England: incidence, mortality and case-fatality rates for the period 2011-2019" = ec_flexreg,
  "Trends in E. coli bacteremia among infants in England: incidence, mortality and case-fatality rate for the period 2011-2019 by mother ethnicity" = ec_flexteh,
  path = here("outputs", "ec_rates_tables.docx"))

##table for merging:

ec_pop_rates<- totalrates %>% 
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
  
