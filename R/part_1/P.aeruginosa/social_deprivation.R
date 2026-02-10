## Pfizer infant spesis Work Package 3
## Multiple Deprevation (IMD) quintile
## Original Author: Luisa Hallmaier-Wacker
source("path/path.R")


PA1_BR_mHES <- PA1_BR_mHES %>% mutate(imd04_grp=case_when(imd04rk<=6568 ~ "1st",
                                                          imd04rk<=13137 & imd04rk>=6569 ~"2nd",
                                                          imd04rk<=19706 & imd04rk>=13138 ~"3rd",
                                                          imd04rk<=26275 & imd04rk>=19707 ~"4th",
                                                          imd04rk<= 32844 & imd04rk>=26276 ~"5th",
                                                          is.na(imd04rk) ~ "Unknown"))

data_3.2F <- PA1_BR_mHES %>% group_by(imd04_grp) %>% tally()

#Examining records that have a gestational age of <22 weeks

data <- PA1_BR_mHES %>% filter(imd04_grp=="Unknown") %>% select(imd04,imd04rk,imd04_grp)