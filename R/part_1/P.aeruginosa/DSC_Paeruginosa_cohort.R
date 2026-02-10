#Infant cohort definition
#1.Remove infants that have been identified as not infants
#2.Add NHS information recovered from SGSS database
#Written by: Luisa Hallmaier-Wacker
#Project: Pfizer infant sepsis project
#Pathogen: P.aeruginosa

source("path/path.R")


#Load Ecoli extract from DCS (if run independently)

#Remove records in DSC records that are not infantsd

PA1_remove <- PA1_remove %>% select(nhs)

PA <- anti_join(PA1, PA1_remove, by="nhs")

#Import combined SGSS records and add update NHS records to cohort

SGSS <- SGSS %>% rename(id=i_id, nhs_sgss=nhs)

#Replace missing NHS number in MDS with NHS number found in SGSS
PAa <- left_join(SGSS, PA, by=("id"))
PAa <- PAa %>% select(-nhs) %>% rename(nhs=nhs_sgss)

#Join MDS with valid NHS number with records with NHS found in SGSS
PAb <- anti_join(PA, SGSS, by=("id"))
PA <- bind_rows(PAa, PAb)
PA <- PA %>% select(-valid_nhs, -hos_unknown, -age, -dob_unknown)

#Save cohort
write.csv(PA,paste(path_out,'DSC_Paeruginosa_cohort.csv',sep = ''), row.names = FALSE)

remove(PAa, PAb, nhsvalid, SGSS, PA1_remove)

#Validity check on Ecoli cohort

PA <- PA %>% mutate(nhs=as.numeric(nhs))
nhsvalid <-valid_nhs(PA$nhs)
nhsvalid <- nhsvalid %>% mutate(nhs_number=as.numeric(nhs_number))
PA <- left_join(PA, nhsvalid, by = c("nhs" = "nhs_number"), keep=FALSE)

PA <- PA %>% mutate(hos_unknown=hos %in% c("UNKNOWN","NO PATIENT ID",NA)) %>% 
  mutate(dob_unknown=dob %in% c("1900-01-01",NA))


## Create missing data summary output

missing1 <- PA %>% select(dob_unknown, hos_unknown) %>% summarise_all(~sum(.))
missing2 <- PA %>% filter(valid_nhs == "FALSE") %>%  tally() %>% rename(nhs_invalid=n)
missing_data_sum <- bind_cols(missing1, missing2)

rm(missing1)
rm(missing2)
rm(nhsvalid)