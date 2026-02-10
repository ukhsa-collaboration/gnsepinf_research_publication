## Project:Pfizer infant spesis Work Package 3
## DCS inital pulling E.coli data 
## Original Author: Luisa Hallmaier-Wacker
source("path/path.R")

#Read in Data

EC1 <- tibble(EC1) 
EC1 <- EC1 %>% clean_names()

#Change date formats of variables to be used
EC1 <- EC1 %>% mutate(specimen_date=as.Date(specimen_date, format = "%d/%m/%Y")) %>% 
             mutate(data_collection_date=as.Date(data_collection_date, format = "%d/%m/%Y")) %>% 
             mutate(date_of_birth=as.Date(date_of_birth, format = "%d/%m/%Y"))

#Rename
EC1 <- EC1 %>% ungroup() %>%  rename(nhs=nhs_number,
                                   hos=hospital_number,
                                   name1=forename,
                                   name2=surname,
                                   sex=sex,
                                   dob=date_of_birth,
                                   date=specimen_date)
                            
EC1 <- EC1 %>% mutate_at(vars(name1, name2),~(toupper(.)))

#Validity check

EC1 <- EC1 %>% mutate(nhs=as.numeric(nhs))
nhsvalid <-valid_nhs(EC1$nhs)
nhsvalid <- nhsvalid %>% mutate(nhs_number=as.numeric(nhs_number))
EC1 <- left_join(EC1, nhsvalid, by = c("nhs" = "nhs_number"), keep=FALSE)

EC1 <- EC1 %>% mutate(hos_unknown=hos %in% c("UNKNOWN","NO PATIENT ID",NA)) %>% 
               mutate(dob_unknown=dob %in% c("1900-01-01",NA))


## Create missing data summary output

missing1 <- EC1 %>% select(dob_unknown, hos_unknown) %>% summarise_all(~sum(.))
missing2 <- EC1 %>% filter(valid_nhs == "FALSE") %>%  tally() %>% rename(nhs_invalid=n)
missing_data_sum <- bind_cols(missing1, missing2)

rm(missing1)
rm(missing2)

#Calculate patients age --> filter for infants (<=365 days)
#Missing DOB will be excluded from the analysis

EC1 <- EC1 %>% mutate(age=(date-dob)) %>% filter(age <= 365)


##Save file

path_out = "//filepor10.phe.gov.uk/pid_hes$/Colindale/HCAI/Pfizer Infant Gram-negative/R/Cohort/Ecoli/"
write.csv(EC1,paste(path_out,'DSC_Ecoli_Extract.csv',sep = ''))

