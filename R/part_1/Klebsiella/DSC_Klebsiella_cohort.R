#Infant cohort definition
#1.Remove infants that have been identified as not infants (n=551 records)
#2.Add NHS information recovered from SGSS database (n=35 records)
#Written by: Luisa Hallmaier-Wacker
#Project: Pfizer infant sepsis project
#Pathogen: E.coli
source("path/path.R")

#Load Ecoli extract from DCS (if run independently)

#Remove records in DSC records that are not infantsd

KS1_remove <- KS1_remove %>% select(nhs)

KS <- anti_join(KS1, KS1_remove, by="nhs")

#Import combined SGSS records and add update NHS records to cohort

SGSS <- SGSS %>% rename(id=i_id, nhs_sgss=nhs)

#Replace missing NHS number in MDS with NHS number found in SGSS
KSa <- left_join(SGSS, KS, by=("id"))
KSa <- KSa %>% select(-nhs) %>% rename(nhs=nhs_sgss)

#Join MDS with valid NHS number with records with NHS found in SGSS
KSb <- anti_join(KS, SGSS, by=("id"))
KS <- bind_rows(KSa, KSb)
KS <- KS %>% select(-valid_nhs, -hos_unknown, -age, -dob_unknown)

#Save cohort
path_out = "//filepor10.phe.gov.uk/pid_hes$/Colindale/HCAI/Pfizer Infant Gram-negative/R/Cohort/Klebsiella/"
write.csv(KS,paste(path_out,'DSC_Klebsiella_cohort.csv',sep = ''), row.names = FALSE)

remove(KSa, KSb, nhsvalid, SGSS, KS1_remove)

#Validity check on Ecoli cohort

KS <- KS %>% mutate(nhs=as.numeric(nhs))
nhsvalid <-valid_nhs(KS$nhs)
nhsvalid <- nhsvalid %>% mutate(nhs_number=as.numeric(nhs_number))
KS <- left_join(KS, nhsvalid, by = c("nhs" = "nhs_number"), keep=FALSE)

KS <- KS %>% mutate(hos_unknown=hos %in% c("UNKNOWN","NO PATIENT ID",NA)) %>% 
  mutate(dob_unknown=dob %in% c("1900-01-01",NA))


## Create missing data summary output

missing1 <- KS %>% select(dob_unknown, hos_unknown) %>% summarise_all(~sum(.))
missing2 <- KS %>% filter(valid_nhs == "FALSE") %>%  tally() %>% rename(nhs_invalid=n)
missing_data_sum <- bind_cols(missing1, missing2)

rm(missing1)
rm(missing2)
rm(nhsvalid)