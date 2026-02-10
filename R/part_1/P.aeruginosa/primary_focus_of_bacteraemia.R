## Pfizer infant spesis Work Package 3
## PRIMARY FOCUS OF BACTERAEMIA
## Original Author: Luisa Hallmaier-Wacker
source("path/path.R")


#Data completion is low for these records
data_3.2A <- PA %>% group_by(source_primary_focus_of_bacteraemia) %>% tally()
