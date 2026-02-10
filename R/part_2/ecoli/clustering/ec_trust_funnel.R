#### funnel plots for E.coli data --> trust clusters

pacman::p_load(rio,
               here,
               tidyverse,
               janitor,
               ggplot2,
               dplyr,
               ukhsacharts)
source("paths/import_datasets.R")

## funnel plots looking at the clustering of the proportion of DEATHS/cases per trustsum

ec_count_trust <- ec_univariate3 %>% 
  group_by(trust_provider) %>% 
  summarize(n_cases = dplyr::n())


ec_deaths_trust <- ec_univariate3 %>% 
  filter(death_90 == 1) %>% 
  group_by(trust_provider) %>% 
  summarize(n_cases = dplyr::n())

ec_trust_joint <- ec_count_trust %>% 
  left_join(ec_deaths_trust, by = c("trust_provider"))

ec_trust_joint <- ec_trust_joint %>% 
  rename(cases_total = n_cases.x,
         deaths_total = n_cases.y)

ec_trust_joint <- ec_trust_joint %>% 
  filter(trust_provider != "") %>% 
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
    trust_provider = tolower(trust_provider),
    outlier = case_when(
      death_rate > lcl95 & death_rate < ucl95 ~ 0,
      death_rate < lcl95 | death_rate > ucl95 ~ 1) 
  )


##create the plot

ec_trust_funnel <- 
  ggplot(
    data = ec_trust_joint,
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
  #                      stringr::str_wrap(trust_provider, width = 10), "")),
  #       color = "black",
  #       size = 2,
  #       hjust = -0.2,
  #       vjust = -0.3) +
  labs(x = "number of cases") +
  labs(y = "death rate") +
  ggtitle("Funnel plot showing E. coli mortality rates for \nEngland Trusts")+
  geom_label(aes(x = 80, y = .5, label ="ICC = 0.1215908"))+
  theme_ukhsa(theme = "ukhsa")

ec_trust_joint %>% 
  filter(outlier == 1) #%>% 
  pull(trust_provider)