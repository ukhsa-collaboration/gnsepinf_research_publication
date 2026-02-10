#### funnel plots for E.coli data --> centre clusters

pacman::p_load(rio,
               here,
               tidyverse,
               janitor,
               ggplot2,
               dplyr,
               ukhsacharts)

source("paths/import_datasets.R")

## funnel plots looking at the clustering of the proportion of DEATHS/cases per centresum

ec_count_centre <- ec_univariate3 %>% 
  group_by(phe_centre_reporting) %>% 
  summarize(n_cases = dplyr::n())


ec_deaths_centre <- ec_univariate3 %>% 
  filter(death_90 == 1) %>% 
  group_by(phe_centre_reporting) %>% 
  summarize(n_cases = dplyr::n())

ec_centre_joint <- ec_count_centre %>% 
  left_join(ec_deaths_centre, by = c("phe_centre_reporting"))

ec_centre_joint <- ec_centre_joint %>% 
  rename(cases_total = n_cases.x,
         deaths_total = n_cases.y)

ec_centre_joint <- ec_centre_joint %>%
  filter(phe_centre_reporting != "") %>% 
  dplyr:: mutate(
    deaths_total = case_when(
      is.na(deaths_total) ~ 0,
      TRUE ~ deaths_total
    ),
    death_rate = deaths_total / cases_total,
    overall_death_rate = sum(deaths_total)/sum(cases_total),
    se = sqrt( overall_death_rate * ((1- overall_death_rate) /cases_total)),
    lcl95 = overall_death_rate   - (1.96* se),
    ucl95 = overall_death_rate   + (1.96* se),
    phe_centre_reporting = tolower(phe_centre_reporting),
    outlier = case_when(
      death_rate > lcl95 & death_rate < ucl95 ~ 0,
      death_rate < lcl95 | death_rate > ucl95 ~ 1) 
  )


##create the plot

ec_centre_funnel <- 
  ggplot(
    data = ec_centre_joint,
    aes(x = cases_total,
        group = overall_death_rate)) +
  geom_smooth(
    aes(y = lcl95), 
    se = FALSE,
    linetype = "dotted",
    color = "black",
    size = 0.5) +
  geom_smooth(
    aes(y = ucl95), 
    se = FALSE,
    linetype = "dotted",
    color = "black",
    size = 0.5) +
  geom_smooth(
    aes(y = overall_death_rate), 
    se = FALSE,
    color = "black") +
  geom_point(
    aes(y = death_rate),
    color = "red"
  )+
  geom_text(
    aes(y = death_rate,
        label = ifelse(outlier == 1,
                       stringr::str_wrap(phe_centre_reporting, width = 10), "")),
    color = "black",
    size = 3,
    hjust = -0.2,
    vjust = -0.3) +
  ggtitle("Funnel plot showing E. coli mortality rates for \nPHE Centres")+
  geom_label(aes(x = 300, y = .15, label ="ICC = 0.0043"))+
  labs(x = "number of cases") +
  labs(y = "death rate") +
  theme_ukhsa(theme = "ukhsa")

ec_centre_joint %>% 
  filter(outlier == 1) %>% 
  pull(phe_centre_reporting)