## Pfizer infant spesis Work Package 3
## Recurrence (>14 days between episodes)
## Original Author: Luisa Hallmaier-Wacker

source("path/path.R")


#Check for valid NHS number

nhsvalid <-valid_nhs(EC$nhs)
nhsvalid <- nhsvalid %>% mutate(nhs_number=as.numeric(nhs_number))
EC <- left_join(EC, nhsvalid, by = c("nhs" = "nhs_number"), keep=FALSE)


#Filter EC records for records wit valid NHS number for BR linkage
EC1 <- EC %>% filter(valid_nhs) 

data3.3C <- EC1 %>% group_by(nhs) %>%  tally() %>% 
                ungroup() %>% rename(n_episodes=n) %>% 
              group_by(n_episodes) %>%  tally()


