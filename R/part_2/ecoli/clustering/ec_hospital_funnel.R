#### funnel plots for E.coli data --> hospital clusters

pacman::p_load(rio,
               here,
               tidyverse,
               janitor,
               ggplot2,
               dplyr,
               ukhsacharts)
library(dplyr)
source("paths/import_datasets.R")

## funnel plots looking at the clustering of the proportion of DEATHS/cases per HOSPITAL


ec_count_hospital <- ec_univariate3 %>% 
  group_by(hospital_site_name) %>% 
  summarize(n_cases = n())


ec_deaths_hospital <- ec_univariate3 %>% 
  filter(death_90 == 1) %>% 
  group_by(hospital_site_name) %>% 
  summarize(n_cases = dplyr::n())

ec_hospital_joint <- ec_count_hospital %>% 
  left_join(ec_deaths_hospital, by = c("hospital_site_name"))

ec_hospital_joint <- ec_hospital_joint %>% 
  rename(cases_total = n_cases.x,
         deaths_total = n_cases.y)

ec_hospital_joint <- ec_hospital_joint %>% 
  filter(hospital_site_name != "") %>% 
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
    hospital_site_name = tolower(hospital_site_name),
    outlier = case_when(
      death_rate > lcl95 & death_rate < ucl95 ~ 0,
      death_rate < lcl95 | death_rate > ucl95 ~ 1),
    deaths_total = as.integer(deaths_total)
  )


##create the plot

ec_funnel <- 
  ggplot(
    data = ec_hospital_joint,
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
  # geom_text(
  #   aes(y = death_rate,
  #       label = ifelse(outlier == 1,
  #                      stringr::str_wrap(hospital_site_name, width = 10), "")),
  #       color = "black",
  #       size = 2,
  #       hjust = -0.2,
  #       vjust = -0.3) +
  labs(x = "number of cases") +
  labs(y = "death rate") +
  ggtitle("Funnel plot showing E. coli mortality rates for \nEngland Hospitals")+
  geom_label(aes(x = 50, y = .7, label ="ICC = 0.1215908"))+
  theme_ukhsa(theme = "ukhsa")

ec_hospital_joint %>% 
  filter(outlier == 1) #%>% 
  pull(hospital_site_name)
