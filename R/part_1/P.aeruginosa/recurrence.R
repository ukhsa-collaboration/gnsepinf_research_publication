## Pfizer infant spesis Work Package 3
## Recurrence (>14 days between episodes)
## Original Author: Luisa Hallmaier-Wacker

source("path/path.R")


#Check for valid NHS number

nhsvalid <-valid_nhs(PA$nhs)
nhsvalid <- nhsvalid %>% mutate(nhs_number=as.numeric(nhs_number))
PA <- left_join(PA, nhsvalid, by = c("nhs" = "nhs_number"), keep=FALSE)


#Filter EC records for records wit valid NHS number for BR linkage
PA1 <- PA %>% filter(valid_nhs) 

data3.3C <- PA1 %>% group_by(nhs) %>%  tally() %>% 
                ungroup() %>% rename(n_episodes=n) %>% 
              group_by(n_episodes) %>%  tally()

