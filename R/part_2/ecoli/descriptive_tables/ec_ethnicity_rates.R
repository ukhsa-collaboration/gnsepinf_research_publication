###ethnicity rates tables

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


## first recode ethnicity categories so they match the ones in the clean dataset
##The final product should be a dataframe with two columns 1 with ethnicity names and 2nd with total population counts
##i will only add up numbers for females aged 20-49yo

ethnicity <- ethn %>% 
  pivot_longer(
    cols = 2:19,
    names_to = "ethnicity",
    values_to = "population"
  ) %>% 
  clean_names() %>% 
  filter(age %in% c("20 to 29 years",
                    "30 to 39 years",
                    "40 to 49 years")
  ) %>% 
  mutate(ethnos = case_when(
    ethnicity %in% c("White & Asian", "White & Black African", "White & Black Caribbean", "Other Mixed") ~ "mixed",
    ethnicity == "Other Black" ~ "Black British",
    ethnicity == "Other Asian" ~ "Asian other",
    ethnicity %in% c("White Gypsy/ Irish Traveller", "White Irish", "Other White") ~ "White other",
    ethnicity %in% c("Arab", "Other") ~ "Other ethnicity",
    TRUE ~ ethnicity
  )
  )

ethnicity$population <- as.numeric(gsub(",","", ethnicity$population))



granular_eth <- ethnicity %>%   
  group_by(ethnos) %>% 
  summarise(population_ethn = sum(population, na.rm = TRUE))


##now we take the data from the ec final dataset

ec_ethnos <- ec_final %>% 
  drop_na(any_of(c("ethnicity")
                 )
          ) %>% 
  group_by(ethnicity) %>% 
  tally()


ec_eth_rate <- left_join(ec_ethnos,
                         granular_eth,
                         by = c("ethnicity" = "ethnos"))

ec_eth_rate <- ec_eth_rate %>% 
  mutate( n = as.numeric(n),
    case_rate = (n/population_ethn)*100000,
    case_rate = round(case_rate, digits=2))

##now for death rates
ec_death_eth_num <- ec_final %>% 
  filter(death_90 == 1) %>% 
  drop_na(any_of(c("ethnicity")
                 )
          ) %>% 
  group_by(ethnicity) %>% 
  tally()

ec_eth_death <- left_join(granular_eth,
                          ec_death_eth_num,
                          by = c("ethnos" = "ethnicity"))

ec_eth_death <- ec_eth_death %>% 
  mutate(deaths = as.numeric(n),
         death_rate = (deaths/population_ethn) *100000,
         death_rate = round(death_rate, 2)
         ) %>% 
  replace_na(list(deaths = 0, n=0, death_rate = 0))


ec_eth_rate <- ec_eth_rate %>% 
  mutate(cases = as.numeric(n),
          case_rate = (cases/population_ethn)*100000,
          case_rate = round(case_rate, 2)) %>% 
  mutate(cases = case_when(
    cases== NA_real_ ~0,
    TRUE ~cases
  )
  )


ec_ethtable <- left_join(ec_eth_death,
                         ec_eth_rate,
                         by= c("ethnos"= "ethnicity"))


ec_ethtable <- ec_ethtable %>% 
  mutate(case_fatality = (deaths/cases)*1000,
         case_fatality = round(case_fatality, 2))
  
###Making a table for a word presentation

ecethtable <- ec_ethtable %>% 
  select(
    ethnos,
    population_ethn.x,
    cases,
    case_rate,
    deaths,
    death_rate,
    case_fatality) %>% 
  arrange(-case_rate)

ec_flexeth <- flextable(ecethtable)


border_style = officer::fp_border(color="black", width=1)

ec_flexteh <- ec_flexeth %>% 
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
    ethnos = "Ethnicity",
    population_ethn.x = "Population",
    cases = "Total",
    case_rate = "Rate per 100,000",
    deaths = "Total",
    death_rate = "Rate per 100,000",
    case_fatality = "Case-fatality rate per 1,000"
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

print(ec_flexteh, preview = "docx")

# ec_final %>% 
#   tabyl(ethnicity, matage_grp) %>% 
#   adorn_totals(where = "row") %>% 
#   adorn_percentages(denominator = "row") %>% 
#   adorn_pct_formatting(digits=1)
# 
# table(ec_final$ethnicity, ec_final$matage_grp)
# table(ec_final$ethnicity, ec_final$imd_quintile)
# table(ec_final$matage_grp, ec_final$imd_quintile)
