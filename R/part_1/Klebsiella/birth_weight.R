## Pfizer infant spesis Work Package 3
## BIRTH WEIGHT 
## Original Author: Luisa Hallmaier-Wacker
source("path/path.R")


KS1_BR_mHES <- KS1_BR_mHES %>% mutate(birth_weight_equal=case_when(birthwgt_br==birweit_1 | birthwgt_br==birweit_2 | birthwgt_br==birweit_3~1,
                                                                   TRUE ~ 0)) %>% 
                               mutate(birth_weight_equal=as.integer(birth_weight_equal)) 



KS1_BR_mHES <- KS1_BR_mHES %>% mutate(birthwgt_combined=case_when(birth_weight_equal=="1" ~1,
                                                                  birth_weight_equal=="0" & multbth_br== "1" & birthwgt_br!="0" ~ 1,
                                                                  birth_weight_equal=="0" & is.na(multbth_br) & birthwgt_br!="0" ~ 1,
                                                                  birth_weight_equal=="0" & is.na(multbth_br) & birthwgt_br=="0" ~ 2,
                                                                  birth_weight_equal=="0" & is.na(multbth_br) & is.na(birthwgt_br) ~ 2,
                                                                  birth_weight_equal=="0" & multbth_br=="1" ~ 0))

KS1_BR_mHES <- KS1_BR_mHES %>% mutate(birthwgt_fin=case_when(birthwgt_combined=="1" ~ birthwgt_br,
                                                                  birthwgt_combined=="2" ~ birweit_1))




KS1_BR_mHES <- KS1_BR_mHES %>% mutate(birthwgt_grp=case_when(birthwgt_fin < 1000 ~ "Extremely Low",
                                         birthwgt_fin >=1000 & birthwgt_fin <1500 ~"Very Low",
                                         birthwgt_fin >=1500 & birthwgt_fin <2500 ~"Low",
                                         birthwgt_fin >=2500 ~ "Normal",
                                         TRUE ~ "Unknown"))
                                                            

data_3.2C <- KS1_BR_mHES %>% group_by(birthwgt_grp) %>% tally()
