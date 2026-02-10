## Pfizer infant spesis Work Package 3
## Gestational Age Calculation
## Original Author: Luisa Hallmaier-Wacker
source("path/path.R")


EC1_BR_mHES <- EC1_BR_mHES %>% mutate(gestage_grp=case_when(gestat_1 %in% c(22,23,24,25,26,27) ~ "Extremely Preterm",
                                                            gestat_1 %in% c(28,29,30,31) ~"Very Preterm",
                                                            gestat_1 %in% c(32,33,34,35,36) ~"Moderate Preterm",
                                                            gestat_1>=37 & gestat_1<50 ~"Term",
                                                            gestat_1==99 ~"NA",
                                                            TRUE ~ "NA"))

data_3.2B <- EC1_BR_mHES %>% group_by(gestage_grp) %>% tally()

EC1_BR_mHES <- EC1_BR_mHES %>% mutate(FY=year(date) + (month(date) >= 4))

data_3.4A <- EC1_BR_mHES %>% group_by(gestage_grp, FY) %>% tally()


data_3.4A <- data_3.4A %>% pivot_wider(names_from = FY, values_from = n)


#Examining records that have a gestational age of <22 weeks

data_3.2B_GAless22weeks <- EC1_BR_mHES %>% filter(gestage_grp=="NA") %>% filter(!gestat_1==99)

