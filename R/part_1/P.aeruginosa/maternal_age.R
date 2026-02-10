## Pfizer infant spesis Work Package 3
## Maternal Age
## Original Author: Luisa Hallmaier-Wacker

source("path/path.R")


PA1_BR <- PA1_BR %>%  mutate_at(vars(mother_dob_br), ~ymd(.)) %>%
                      mutate_at(vars(dob_br), ~ymd(.)) %>%
                      mutate(matage=floor(decimal_date(dob_br) - decimal_date(mother_dob_br)))

PA1_BR <- PA1_BR %>% mutate(matage_grp=case_when(matage<20 ~ "<20 years",
                                                           matage>=20 & matage<30 ~"20-29 years",
                                                           matage>=30 & matage<40 ~"30-39 years",
                                                           matage>=40 & matage<50 ~"40-49 years",
                                                           matage>=50 ~" 50+ year"))

data_3.2G <- PA1_BR %>% group_by(matage_grp) %>% tally()

