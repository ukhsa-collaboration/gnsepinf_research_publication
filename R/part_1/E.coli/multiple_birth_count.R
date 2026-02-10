## Pfizer infant spesis Work Package 3
## Number of infants (multiple/singelton)
## Original Author: Luisa Hallmaier-Wacker
source("path/path.R")


EC1_BR <- EC1_BR %>% mutate(mutlibirth_grp=case_when(multtype_br %in% c(0,1,2,3,4,5,6,7,8,9) ~ "Twin",
                                                  multtype_br>9 ~"Triplet +",
                                                  is.na(multtype_br) ~"Single"))

data_3.2D <- EC1_BR %>% group_by(mutlibirth_grp) %>% tally()

