## Project:Pfizer infant spesis Work Package 3
## DCS inital pulling P.aeruginosa data 
## Original Author: Luisa Hallmaier-Wacker
source("path/path.R")


#Read in Data

PA1 <- tibble(PA1) 
PA1 <- PA1 %>% clean_names()

#Change date formats of variables to be used
PA1 <- PA1 %>% mutate(specimen_date=as.Date(specimen_date, format = "%d/%m/%Y")) %>% 
             mutate(data_collection_date=as.Date(data_collection_date, format = "%d/%m/%Y")) %>% 
             mutate(date_of_birth=as.Date(date_of_birth, format = "%d/%m/%Y"))

#Rename
PA1 <- PA1 %>% ungroup() %>%  rename(nhs=nhs_number,
                                   hos=hospital_number,
                                   name1=forename,
                                   name2=surname,
                                   sex=sex,
                                   dob=date_of_birth,
                                   date=specimen_date)
                            
PA1 <- PA1 %>% mutate_at(vars(name1, name2),~(toupper(.)))

#Validity check

PA1 <- PA1 %>% mutate(nhs=as.numeric(nhs))
nhsvalid <-valid_nhs(PA1$nhs)
nhsvalid <- nhsvalid %>% mutate(nhs_number=as.numeric(nhs_number))
PA1 <- left_join(PA1, nhsvalid, by = c("nhs" = "nhs_number"), keep=FALSE)

PA1 <- PA1 %>% mutate(hos_unknown=hos %in% c("UNKNOWN","NO PATIENT ID",NA)) %>% 
               mutate(dob_unknown=dob %in% c("1900-01-01",NA))


## Create missing data summary output

missing1 <- PA1 %>% select(dob_unknown, hos_unknown) %>% summarise_all(~sum(.))
missing2 <- PA1 %>% filter(valid_nhs == "FALSE") %>%  tally() %>% rename(nhs_invalid=n)
missing_data_sum <- bind_cols(missing1, missing2)

rm(missing1)
rm(missing2)
rm(nhsvalid)

#Calculate patients age --> filter for infants (<=365 days)
#Missing DOB will be excluded from the analysis

PA1 <- PA1 %>% mutate(age=(date-dob)) %>% filter(age <= 365)


##Save file

write.csv(PA1,paste(path_out,'DSC_Paeruginosa_Extract.csv',sep = ''))

