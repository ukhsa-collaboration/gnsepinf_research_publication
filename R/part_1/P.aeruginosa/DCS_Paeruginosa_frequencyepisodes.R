#Incidence calculations infant cohort using Mandetory dataset (DCS)
#Part of WP3 Pfizer Infant Projekt
#Author: Luisa Hallmaier-Wacker

source("path/path.R")


#Load infant cohort (DCS data) if required
PA <- PA %>% mutate_at(vars(date), ~ymd(.))
#Generate financial year variable

PA <- PA %>% mutate(FY=year(date) + (month(date) >= 4))

data_3.1A <- PA %>% group_by(FY) %>% tally()

