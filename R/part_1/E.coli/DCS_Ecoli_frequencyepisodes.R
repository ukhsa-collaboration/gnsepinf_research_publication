#Incidence calculations infant cohort using Mandetory dataset (DCS)
#Part of WP3 Pfizer Infant Projekt
#Author: Luisa Hallmaier-Wacker
source("path/path.R")

#Load infant cohort (DCS data) if required
EC <- EC %>% mutate_at(vars(date), ~dmy(.))

#Generate financial year variable

EC <- EC %>% mutate(FY=year(date) + (month(date) >= 4))

data_3.1A <- EC %>% group_by(FY) %>% tally()

