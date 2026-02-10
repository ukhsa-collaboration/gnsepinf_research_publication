### ethnicity rates for E coli but instead of using as a denominator the number of females from 20-45
## I will use the ethnicity data for the number of live birth by ethnicity for each year

# I am hoping to compensate for the fact that mothers with particular ethnicities
# might have more babies compared to other ethnicities (seems like white british and asian other were haveing less babies)

## I have data on this from 2014-2019. for e coli i have assumed that the number of live births
## for the years 2011-2013 was the same as for 2014
## for kleb and pseudomonas as we just have data from 2017-19 we will just use that population data as a denominator


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

eth_live <- livebirths %>% 
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
  ) #%>% 
  mutate(ethnos = case_when(
    ethnicity %in% c("White & Asian", "White & Black African", "White & Black Caribbean", "Other Mixed") ~ "mixed",
    ethnicity == "Other Black" ~ "Black British",
    ethnicity == "Other Asian" ~ "Asian other",
    ethnicity %in% c("White Gypsy/ Irish Traveller", "White Irish", "Other White") ~ "White other",
    ethnicity %in% c("Arab", "Other") ~ "Other ethnicity",
    TRUE ~ ethnicity
  )
  )
skim(eth_live$live_births)
  
eth_live$live_births <- as.numeric(gsub(",","", eth_live$live_births))



 eth14 <- eth_live %>%   
  group_by(ethnicity) %>% 
  summarise(population_ethn14 = sum(live_births, na.rm = TRUE))


##now we take the data from the ec final dataset

ec_ethnos <- ec_final %>% 
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


ec_eth_rate <- left_join(ec_ethnos,
                         eth14,
                         by = "ethnicity")

ec_eth_rate <- ec_eth_rate %>% 
  mutate( n = as.numeric(n),
          case_rate = (n/population_ethn14)*100000,
          case_rate = round(case_rate, digits=2))

##now for death rates
ec_death_eth_num <- ec_final %>% 
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

ec_eth_death <- left_join(eth14,
                          ec_death_eth_num,
                          by = c("ethnicity"))

ec_eth_death <- ec_eth_death %>% 
  mutate(deaths = as.numeric(n),
         death_rate = (deaths/population_ethn14) *100000,
         death_rate = round(death_rate, 2)
  ) %>% 
  replace_na(list(deaths = 0, n=0, death_rate = 0))


ec_eth_rate <- ec_eth_rate %>% 
  mutate(cases = as.numeric(n),
         case_rate = (cases/population_ethn14)*100000,
         case_rate = round(case_rate, 2)) %>% 
  mutate(cases = case_when(
    cases== NA_real_ ~0,
    TRUE ~cases
  )
  )


ec_ethtable <- left_join(ec_eth_death,
                         ec_eth_rate,
                         by= c("ethnicity"))


ec_ethtable <- ec_ethtable %>% 
  mutate(case_fatality = (deaths/cases)*100,
         case_fatality = round(case_fatality, 1))

##table tor merging
ec_ethrates_table <- ec_ethtable %>% 
  select(ethnicity,
         cases,
         case_rate,
         deaths,
         death_rate
  ) %>% 
  mutate(variable = case_when(
    cases >= 1 ~ "ethnicity" 
  )) %>% 
  rename(
    incidence = case_rate,
    mortality_rate = death_rate,
    group = ethnicity
  ) %>% 
  filter(group != "Live births")



############################
#oldtable
###Making a table for a word presentation

ecethtable <- ec_ethtable %>% 
  select(
    ethnicity,
    population_ethn14.x,
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

