## Pfizer infant spesis Work Package 3
## PRIMARY FOCUS OF BACTERAEMIA
## Original Author: Luisa Hallmaier-Wacker

#Data completion is low for these records
source("path/path.R")


data_3.2A <- EC %>% group_by(source_primary_focus_of_bacteraemia) %>% tally()

