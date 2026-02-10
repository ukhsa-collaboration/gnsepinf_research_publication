#Infant cohort definition
#1.Remove infants that have been identified as not infants (n=551 records)
#2.Add NHS information recovered from SGSS database (n=35 records)
#Written by: Luisa Hallmaier-Wacker
#Project: Pfizer infant sepsis project
#Pathogen: E.coli

#Load Ecoli extract from DCS (if run independently)
source("path/path.R")

#Remove records in DSC records that are not infantsd

EC1_remove <- EC1_remove %>% select(nhs)

EC <- anti_join(EC1, EC1_remove, by="nhs")

#Import combined SGSS records and add update NHS records to cohort

SGSS <- SGSS %>% rename(id=i_id)

#Replace missing NHS number in MDS with NHS number found in SGSS
ECa <- left_join(SGSS, EC, by=("id"))
ECa <- ECa %>% select(-nhs) %>% rename(nhs=nhs_sgss)

#Join MDS with valid NHS number with records with NHS found in SGSS
ECb <- anti_join(EC, SGSS, by=("id"))
EC <- bind_rows(ECa, ECb)
EC <- EC %>% select(-valid_nhs, -hos_unknown, -age, -dob_unknown)

#Save cohort
write.csv(EC,paste(path_out,'DSC_Ecoli_cohort.csv',sep = ''), row.names = FALSE)

remove(ECa, ECb, nhsvalid, SGSS, EC1_remove)

#Validity check on Ecoli cohort

EC <- EC %>% mutate(nhs=as.numeric(nhs))
nhsvalid <-valid_nhs(EC$nhs)
nhsvalid <- nhsvalid %>% mutate(nhs_number=as.numeric(nhs_number))
EC <- left_join(EC, nhsvalid, by = c("nhs" = "nhs_number"), keep=FALSE)

EC <- EC %>% mutate(hos_unknown=hos %in% c("UNKNOWN","NO PATIENT ID",NA)) %>% 
  mutate(dob_unknown=dob %in% c("1900-01-01",NA))


## Create missing data summary output

missing1 <- EC %>% select(dob_unknown, hos_unknown) %>% summarise_all(~sum(.))
missing2 <- EC %>% filter(valid_nhs == "FALSE") %>%  tally() %>% rename(nhs_invalid=n)
missing_data_sum <- bind_cols(missing1, missing2)

rm(missing1)
rm(missing2)
