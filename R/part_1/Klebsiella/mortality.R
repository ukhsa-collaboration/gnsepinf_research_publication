## Pfizer infant spesis Work Package 3
## Mortality
## Original Author: Luisa Hallmaier-Wacker
source("path/path.R")


#Filter for records that were successfully DBS traced
KS_vit<- KS_vit %>% filter(RecType %in% c(30, 33))


KS_vit <- KS_vit %>% mutate_at(vars(data_collection_date), ~ymd(.)) %>% 
                    mutate_at(vars(DateOfDeath), ~ymd(.)) %>% 
                    mutate(baby_dead=if_else(is.na(DateOfDeath), 0, 1))

#Filter for dead babies
KS_vit_D <- KS_vit %>% filter(baby_dead==1) %>% mutate(diff_time_death_bact=difftime(
                        DateOfDeath, data_collection_date, units = "days")) %>%
                        mutate_if(is.difftime,as.integer)



KS_vit_D <- KS_vit_D %>%  mutate(diff_time_death_bact_grp=case_when(diff_time_death_bact %in% c(0,1,2,3,4,5,6,7) ~ "0-7 days",
                                                             diff_time_death_bact >=8 & diff_time_death_bact<31 ~"8-30 days",
                                                             diff_time_death_bact >=31 & diff_time_death_bact<61 ~"31-60 days",
                                                             diff_time_death_bact >=61 & diff_time_death_bact<91 ~"61-90 days",
                                                             diff_time_death_bact >=91  ~"90+ days",
                                                             TRUE ~ "NA"))

data_3.3E <- KS_vit_D %>% group_by(diff_time_death_bact_grp) %>% tally()