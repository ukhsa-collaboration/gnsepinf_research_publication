## Pfizer infant spesis Work Package 3
## Mortality
## Original Author: Luisa Hallmaier-Wacker
source("path/path.R")


#Filter for records that were successfully DBS traced

EC_vit<- EC_vit %>% filter(RecType %in% c(30, 33, 40))


EC_vit <- EC_vit %>% mutate_at(vars(data_collection_date), ~dmy(.)) %>% 
                    mutate_at(vars(DateOfDeath), ~ymd(.)) %>% 
                    mutate(baby_dead=if_else(is.na(DateOfDeath), 0, 1))

#Filter for dead babies
EC_vit_D <- EC_vit %>% filter(baby_dead==1) %>% mutate(diff_time_death_bact=difftime(
                        DateOfDeath, data_collection_date, units = "days")) %>%
                        mutate_if(is.difftime,as.integer)


#Generate classification for mortality
EC_vit_D <- EC_vit_D %>%  mutate(diff_time_death_bact_grp=case_when(diff_time_death_bact %in% c(0,1,2,3,4,5,6,7) ~ "0-7 days",
                                                             diff_time_death_bact >=8 & diff_time_death_bact<31 ~"8-30 days",
                                                             diff_time_death_bact >=31 & diff_time_death_bact<61 ~"31-60 days",
                                                             diff_time_death_bact >=61 & diff_time_death_bact<91 ~"61-90 days",
                                                             diff_time_death_bact >=91  ~"90+ days",
                                                             TRUE ~ "NA"))

#Generate Output
data_3.3E <- EC_vit_D %>% group_by(diff_time_death_bact_grp) %>% tally()

#Additional OUtput to look at those records that classify as NA
data_3.3E_NA <- EC_vit_D %>% filter(diff_time_death_bact_grp=="NA")

###############Add gestational age to vitality records###############################################


EC1_BR_mHES <- EC1_BR_mHES %>% mutate(gestage_grp=case_when(gestat_1 %in% c(22,23,24,25,26,27) ~ "Extremely Preterm",
                                                            gestat_1 %in% c(28,29,30,31) ~"Very Preterm",
                                                            gestat_1 %in% c(32,33,34,35,36) ~"Moderate Preterm",
                                                            gestat_1>=37 & gestat_1<50 ~"Term",
                                                            gestat_1==99 ~"NA",
                                                            TRUE ~ "NA"))

###Generate Dataset with gestational age that is then linked to vitality records
EC1_gestage <- EC1_BR_mHES %>% select(id,nhs,gestat_1, gestage_grp)

##Linkage to vitality records
EC_vit_gestage <- inner_join(EC_vit,EC1_gestage, by=c("id", "nhs"))

#Generate Decnominator Data for gestational age of new linked datset
data_3.5A_denominator <- EC_vit_gestage %>% group_by(gestage_grp) %>% tally()

#Filter for dead infants and calculate time difference between dod and specimen date
EC_vit_gestage_D <- EC_vit_gestage %>% filter(baby_dead==1) %>% mutate(diff_time_death_bact=difftime(
                     DateOfDeath, data_collection_date, units = "days")) %>%
                      mutate_if(is.difftime,as.integer)


#Generate classification for mortality

EC_vit_gestage_D <- EC_vit_gestage_D %>%  mutate(diff_time_death_bact_grp=case_when(diff_time_death_bact %in% c(0,1,2,3,4,5,6,7) ~ "0-7 days",
                                                                    diff_time_death_bact >=8 & diff_time_death_bact<31 ~"8-30 days",
                                                                    diff_time_death_bact >=31 & diff_time_death_bact<61 ~"31-60 days",
                                                                    diff_time_death_bact >=61 & diff_time_death_bact<91 ~"61-90 days",
                                                                    diff_time_death_bact >=91  ~"90+ days",
                                                                    TRUE ~ "NA"))

#Generate output (numerators) by gestational age
data_3.5A <- EC_vit_gestage_D %>% group_by(diff_time_death_bact_grp,gestage_grp) %>% tally()

data_3.5A <- data_3.5A %>%  pivot_wider(names_from =diff_time_death_bact_grp, values_from = n)
