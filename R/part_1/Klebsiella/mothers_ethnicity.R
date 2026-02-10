## Pfizer infant spesis Work Package 3
## Mothers Ethnicity
## Original Author: Luisa Hallmaier-Wacker
source("path/path.R")


KS1_BR_mHES <- KS1_BR_mHES %>% mutate(ethnos2=as.character(ethnos)) 

KS1_BR_mHES <- KS1_BR_mHES %>% mutate(ethnicity_grp=case_when(ethnos %in% c("A ") ~ " White British",
                                                              ethnos %in% c("B ","C ") ~"White Other",
                                                              ethnos %in% c("N ") ~"Black African",
                                                              ethnos %in% c("M ") ~"Black Caribbean",
                                                              ethnos %in% c("H ","J ","K ","L ","R ") ~"Asian",
                                                              ethnos %in% c("D ","E ","F ","G ","P ","S ") ~"Other",
                                                              ethnos %in% c("Z ", "X ", "99", "9 ")  ~"Not states"))

data_3.2E <- KS1_BR_mHES %>% group_by(ethnicity_grp) %>% tally()

