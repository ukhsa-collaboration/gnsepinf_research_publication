#Linkage of cohort betweeen Mandetory dataset (DCS) -  ONS birth registrations (BR)
#Part of WP3 Pfizer Infant Projekt
#Author: Luisa Hallmaier-Wacker

#Load infant cohort (DCS data) and Birth registration
#Note to Simon: Birth notification linked to BR can be added here once they have been sent by Neelam

source("path/path.R")

#read in birth registrations

BR <- BR %>% clean_names()

BR <- BR %>% ungroup() %>%  select(-x, -nhsind) %>% 
                            rename(nhs=nhsno,
                                     dob_br=dob,
                                     birthwgt_br=birthwgt,
                                     multtype_br=multtype,
                                     multbth_br=multbth,
                                     mother_dob_br=dobm,
                                     infant_surname_br=snamch,
                                     infant_forname1_br=fnamch1,
                                     infant_forname2_br=fnamch2,
                                     infant_forname3_br=fnamch3,
                                     mother_forname_br=fnamm_1,
                                     mother_surname_br=snamm,
                                     mother_maidenname_br=namemaid) %>% 
                           mutate_at(vars(dob_br), ~ymd(.))


#Check for valid NHS number

nhsvalid <-valid_nhs(EC$nhs)
nhsvalid <- nhsvalid %>% mutate(nhs_number=as.numeric(nhs_number))
EC <- left_join(EC, nhsvalid, by = c("nhs" = "nhs_number"), keep=FALSE)


#Filter EC records for records wit valid NHS number for BR linkage
EC1 <- EC %>% filter(valid_nhs) 
  
#Link EC data to Birth registrations
EC1_BR <- left_join(EC1, BR, by="nhs")

#Look at Match Sucess Just based on NHS
EC1_BR <- EC1_BR %>% mutate(nhs_match=!is.na(dob_br))

EC1_BR_unlink <- EC1_BR %>% filter(!nhs_match)

write.csv(EC1_BR_unlink,paste(path_out,'Linkage_Ecoli_DCS_BR_Unlinked.csv',sep = ''), row.names = FALSE)

EC1_BR <- EC1_BR %>% filter(nhs_match)

write.csv(EC1_BR,paste(path_out,'Linkage_Ecoli_DCS_BR_Success.csv',sep = ''), row.names = FALSE)

#Import mother NHS Numbers that have been traced using mothers DOB, Names & Sex (provided by NICK)
mTraced <- read.csv("//filepor10.phe.gov.uk/pid_hes$/Colindale/HCAI/Pfizer Infant Gram-negative/R/BR_mother_data/Births_ecoli_DCS_br_mother_data_traced_FINAL.csv")

mTraced <- mTraced %>% distinct(infant_nhs, .keep_all = TRUE) %>% 
                select(infant_nhs,NHSnum, mother_dob, RecType) %>% rename(mother_nhs=NHSnum)

#Link cohort data to mNHS number
EC1_BR_mTrace <- left_join(EC1_BR, mTraced, by=c("nhs"="infant_nhs"))

#Filter for records that have not been successfully traced
EC1_BR_mTrace_nomatch <-EC1_BR_mTrace %>% filter(!RecType==20)

#Save
write.csv(EC1_BR_mTrace_nomatch,paste(path_out,'Linkage_Ecoli_DCS_BR_mTRACE_Unlinked.csv',sep = ''), row.names = FALSE)


#Filter for records that have been successfully traced (rectype=20)
EC1_BR_mTrace <-EC1_BR_mTrace %>% filter(RecType==20)

#Save
write.csv(EC1_BR_mTrace,paste(path_out,'Linkage_Ecoli_DCS_BR_mTRACE_Success.csv',sep = ''), row.names = FALSE)


#Import mothers HES records
mHES <- read.csv("//filepor10.phe.gov.uk/pid_hes$/Colindale/HCAI/Pfizer Infant Gram-negative/R/BR_mother_data/births_ecoli_mothers_traced_HES_extract_20210301.csv")
mHES <- mHES %>% clean_names()

#Filter for HES records for the delivery episode
mHES_delivery <- mHES %>% filter(epitype==2)

#Select relevant variables & basic formatting
mHES_delivery <- mHES_delivery %>% select(newnhsno, dob, homeadd, admidate, epiend, epistart, epidur, 
                                          ethnos, dobbaby_1_dv, dobbaby_2_dv, dobbaby_3_dv, dobbaby_4_dv, 
                                          dobbaby_5_dv, dobbaby_6_dv, dobbaby_7_dv, dobbaby_8_dv, dobbaby_9_dv,
                                          antedur, biresus_1, biresus_2, biresus_3, biresus_4, biresus_5, 
                                          biresus_6, biresus_7, biresus_8, biresus_9, birordr_1, birordr_2,
                                          birordr_3, birordr_4, birordr_5, birordr_6, birordr_7, birordr_8,
                                          birordr_9, birstat_1, birstat_2, birstat_3, birstat_4, birstat_5,
                                          birstat_6, birstat_7, birstat_8, birstat_9, birweit_1, birweit_2, 
                                          birweit_3, birweit_4, birweit_5, birweit_6, birweit_7, birweit_8,
                                          birweit_8, delmeth_1, delmeth_2, delmeth_3, delmeth_4, delmeth_5,
                                          delmeth_6, delmeth_7, delmeth_8, delmeth_9, delplac_1, delplac_2,
                                          delplac_3, delplac_4, delplac_5, delplac_6, delplac_7, delplac_8,
                                          delplac_9, delstat_1, delstat_2, delstat_3, delstat_4, delstat_5,
                                          delstat_6, delstat_7, delstat_8, delstat_9, gestat_1, gestat_2, 
                                          gestat_3, gestat_4, gestat_5, gestat_6, gestat_7, gestat_8, gestat_9,
                                          well_baby_ind, matage, imd04, imd04rk) %>% 
                                          rename(mother_nhs=newnhsno, mother_dob=dob) %>% 
                                          mutate_at(vars(dobbaby_1_dv), ~ymd_hms(.)) %>% 
                                          mutate_at(vars(dobbaby_2_dv), ~ymd_hms(.)) %>%
                                          mutate_at(vars(dobbaby_3_dv), ~ymd_hms(.)) %>%
                                          mutate(dobbaby_1_dv=as.Date(dobbaby_1_dv)) %>% 
                                          mutate(dobbaby_2_dv=as.Date(dobbaby_2_dv)) %>% 
                                          mutate(dobbaby_3_dv=as.Date(dobbaby_3_dv)) %>% 
                                          mutate(birordr_1=as.numeric(birordr_1)) %>% 
                                          mutate(birordr_2=as.numeric(birordr_2)) %>% 
                                          mutate(birordr_3=as.numeric(birordr_3))  

mHES2 <- read.csv("//filepor10.phe.gov.uk/pid_hes$/Colindale/HCAI/Pfizer Infant Gram-negative/R/BR_mother_data/Ecoli_DCS_BR_mother_data_2_traced_HES_extract_2.csv")
mHES2 <- mHES2 %>% clean_names()

#Filter for HES records for the delivery episode
mHES2_delivery <- mHES2 %>% filter(epitype==2)

#Select relevant variables & basic formatting
mHES2_delivery <- mHES2_delivery %>% select(newnhsno, dob, homeadd, admidate, epiend, epistart, epidur, 
                                          ethnos, dobbaby_1_dv, dobbaby_2_dv, dobbaby_3_dv, dobbaby_4_dv, 
                                          dobbaby_5_dv, dobbaby_6_dv, dobbaby_7_dv, dobbaby_8_dv, dobbaby_9_dv,
                                          antedur, biresus_1, biresus_2, biresus_3, biresus_4, biresus_5, 
                                          biresus_6, biresus_7, biresus_8, biresus_9, birordr_1, birordr_2,
                                          birordr_3, birordr_4, birordr_5, birordr_6, birordr_7, birordr_8,
                                          birordr_9, birstat_1, birstat_2, birstat_3, birstat_4, birstat_5,
                                          birstat_6, birstat_7, birstat_8, birstat_9, birweit_1, birweit_2, 
                                          birweit_3, birweit_4, birweit_5, birweit_6, birweit_7, birweit_8,
                                          birweit_8, delmeth_1, delmeth_2, delmeth_3, delmeth_4, delmeth_5,
                                          delmeth_6, delmeth_7, delmeth_8, delmeth_9, delplac_1, delplac_2,
                                          delplac_3, delplac_4, delplac_5, delplac_6, delplac_7, delplac_8,
                                          delplac_9, delstat_1, delstat_2, delstat_3, delstat_4, delstat_5,
                                          delstat_6, delstat_7, delstat_8, delstat_9, gestat_1, gestat_2, 
                                          gestat_3, gestat_4, gestat_5, gestat_6, gestat_7, gestat_8, gestat_9,
                                          well_baby_ind, matage, imd04, imd04rk) %>% 
                                          rename(mother_nhs=newnhsno, mother_dob=dob) %>% 
                                          mutate_at(vars(dobbaby_1_dv), ~ymd_hms(.)) %>% 
                                          mutate_at(vars(dobbaby_2_dv), ~ymd_hms(.)) %>%
                                          mutate_at(vars(dobbaby_3_dv), ~ymd_hms(.)) %>%
                                          mutate(dobbaby_1_dv=as.Date(dobbaby_1_dv)) %>% 
                                          mutate(dobbaby_2_dv=as.Date(dobbaby_2_dv)) %>% 
                                          mutate(dobbaby_3_dv=as.Date(dobbaby_3_dv)) %>% 
                                          mutate(birordr_1=as.numeric(birordr_1)) %>% 
                                          mutate(birordr_2=as.numeric(birordr_2)) %>% 
                                          mutate(birordr_3=as.numeric(birordr_3))  
                                          

mHES_delivery <- bind_rows(mHES_delivery, mHES2_delivery)

                                                                  
#LINKAGE of DCS records to mothers delivery records
#Link Based on Mother NHS number and infants DOB
#Multiple iterations of the linkage nessesary in case of twins/triplets born on different days

EC1_BR_mHES_1 <- inner_join(EC1_BR_mTrace, mHES_delivery, by=c("mother_nhs"="mother_nhs","dob_br"="dobbaby_1_dv"))

EC1_BR_mHES_0 <- anti_join(EC1_BR_mTrace, mHES_delivery, by=c("mother_nhs"="mother_nhs","dob_br"="dobbaby_1_dv"))

EC1_BR_mHES_2 <- inner_join(EC1_BR_mHES_0, mHES_delivery, by=c("mother_nhs"="mother_nhs","dob_br"="dobbaby_2_dv"))

EC1_BR_mHES_0 <- anti_join(EC1_BR_mHES_0, mHES_delivery, by=c("mother_nhs"="mother_nhs","dob_br"="dobbaby_2_dv"))

EC1_BR_mHES_3 <- inner_join(EC1_BR_mHES_0, mHES_delivery, by=c("mother_nhs"="mother_nhs","dob_br"="dobbaby_3_dv"))

EC1_BR_mHES_0 <- anti_join(EC1_BR_mHES_0, mHES_delivery, by=c("mother_nhs"="mother_nhs","dob_br"="dobbaby_2_dv"))

EC1_BR_mHES <- bind_rows(EC1_BR_mHES_1, EC1_BR_mHES_2, EC1_BR_mHES_3)

EC1_BR_mHES <- EC1_BR_mHES %>% distinct(id, .keep_all = TRUE)

EC1_BR_mHES_0 <- EC1_BR_mHES_0 %>% distinct(id, .keep_all = TRUE)

path_out = "//filepor10.phe.gov.uk/pid_hes$/Colindale/HCAI/Pfizer Infant Gram-negative/R/Linkage (DCS-BR-mHES)/"
write.csv(EC1_BR_mHES,paste(path_out,'Linkage_Ecoli_DCS_BR_mHES_Success.csv',sep = ''), row.names = FALSE)

#Create output for unlinked records
#1. Records with mHES from mother but no infant DOB match
#2. Records with no mHES match for infant


EC1_BR_mHES_unliked_HESrecord <- inner_join(EC1_BR_mHES_0, mHES_delivery, by=c("mother_nhs"="mother_nhs"))

EC1_BR_mHES_unliked_HESrecord <- EC1_BR_mHES_unliked_HESrecord %>% select(id, nhs, infant_surname_br, mother_nhs, mother_forname_br,
                                                                          mother_dob_br, mother_surname_br, mother_maidenname_br,
                                                                          infant_forname1_br, infant_forname2_br, 
                                                                          infant_forname3_br, date, dob_br, imd04) %>% 
                                                                          distinct(id, .keep_all = TRUE)

EC1_BR_mHES_unliked_noHESrecord <- anti_join(EC1_BR_mHES_0, mHES_delivery, by=c("mother_nhs"="mother_nhs"))

write.csv(EC1_BR_mHES_unliked_HESrecord,paste(path_out,'Linkage_Ecoli_DCS_BR_mHES_Unlinked_HESrecords.csv',sep = ''), row.names = FALSE)
write.csv(EC1_BR_mHES_unliked_noHESrecord,paste(path_out,'Linkage_Ecoli_DCS_BR_mHES_Unlinked_NO-HESrecords.csv',sep = ''), row.names = FALSE)

#Summary Records
S1 <- EC %>% summarise(n=length(id)) %>% mutate(step="cohort") %>% mutate(Rdata="EC")
S2 <- EC1 %>% summarise(n=length(id)) %>% mutate(step="cohort (removed missing NHS)") %>% mutate(Rdata="EC1")
S3 <- EC1_BR %>% summarise(n=length(id)) %>% mutate(step="cohort - BR") %>% mutate(Rdata="EC1_BR")
S4 <- EC1_BR_mTrace %>% summarise(n=length(id)) %>% mutate(step="cohort - BR - Traced Records") %>% mutate(Rdata="EC1_BR_mTrace")
S5 <- EC1_BR_mHES %>% summarise(n=length(id)) %>% mutate(step="cohort - BR - Traced Records - mHES (delivery records)") %>%  mutate(Rdata="EC1_BR_mHES")

linkage_summary <- bind_rows(S1,S2,S3,S4,S5)

linkage_summary<- linkage_summary %>% mutate(percentage_link=(n/length(EC$id))*100)

#Clean-up
remove(S1,S2,S3,S4,S5)
remove(EC1_BR_mHES_1, EC1_BR_mHES_2, EC1_BR_mHES_3)
remove(mHES, mHES2, mHES_delivery, mHES2_delivery, BR, mTraced, nhsvalid)
remove(EC1_BR_mTrace_nomatch, EC1_BR_unlink, EC1_BR_mHES_0, EC1_BR_mHES_unliked_HESrecord, EC1_BR_mHES_unliked_noHESrecord)

