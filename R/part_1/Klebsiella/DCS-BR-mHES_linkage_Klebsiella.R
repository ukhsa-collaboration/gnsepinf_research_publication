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

nhsvalid <-valid_nhs(KS$nhs)
nhsvalid <- nhsvalid %>% mutate(nhs_number=as.numeric(nhs_number))
KS <- left_join(KS, nhsvalid, by = c("nhs" = "nhs_number"), keep=FALSE)


#Filter EC records for records wit valid NHS number for BR linkage
KS1 <- KS %>% filter(valid_nhs) 
  
#Link EC data to Birth registrations
KS1_BR <- left_join(KS1, BR, by="nhs")

#Look at Match Sucess Just based on NHS
KS1_BR <- KS1_BR %>% mutate(nhs_match=!is.na(dob_br))

KS1_BR_unlink <- KS1_BR %>% filter(!nhs_match)

write.csv(KS1_BR_unlink,paste(path_out,'Linkage_Klebsiella_DCS_BR_Unlinked.csv',sep = ''), row.names = FALSE)

KS1_BR <- KS1_BR %>% filter(nhs_match)

write.csv(KS1_BR,paste(path_out,'Linkage_Klebsiella_DCS_BR_Success.csv',sep = ''), row.names = FALSE)

#Import mother NHS Numbers that have been traced using mothers DOB, Names & Sex (provided by NICK)
mTraced <- read.csv("//filepor10.phe.gov.uk/pid_hes$/Colindale/HCAI/Pfizer Infant Gram-negative/R/BR_mother_data/klebsiella_infants_and_mothers_traced_20210222.csv")

mTraced <- mTraced %>% distinct(infant_nhs, .keep_all = TRUE) %>% 
                select(infant_nhs,NHSnum, mother_dob, RecType) %>% rename(mother_nhs=NHSnum)

#Link cohort data to mNHS number
KS1_BR_mTrace <- left_join(KS1_BR, mTraced, by=c("nhs"="infant_nhs"))

#Filter for records that have not been successfully traced
KS1_BR_mTrace_nomatch <-KS1_BR_mTrace %>% filter(!RecType==20)

#Save
write.csv(KS1_BR_mTrace_nomatch,paste(path_out,'Linkage_Klebsiella_DCS_BR_mTRACE_Unlinked.csv',sep = ''), row.names = FALSE)


#Filter for records that have been successfully traced (rectype=20)
KS1_BR_mTrace <-KS1_BR_mTrace %>% filter(RecType==20)

#Save
write.csv(KS1_BR_mTrace,paste(path_out,'Linkage_Klebsiella_DCS_BR_mTRACE_Success.csv',sep = ''), row.names = FALSE)


#Import mothers HES records
mHES <- read.csv("//filepor10.phe.gov.uk/pid_hes$/Colindale/HCAI/Pfizer Infant Gram-negative/R/BR_mother_data/births_klebsiella_mothers_traced_HES_extract_20210301.csv")
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

#mHES2 <- read.csv("//filepor10.phe.gov.uk/pid_hes$/Colindale/HCAI/Pfizer Infant Gram-negative/R/BR_mother_data/Ecoli_DCS_BR_mother_data_2_traced_HES_extract_2.csv")
#mHES2 <- mHES2 %>% clean_names()

#Filter for HES records for the delivery episode
#mHES2_delivery <- mHES2 %>% filter(epitype==2)

#Select relevant variables & basic formatting
#mHES2_delivery <- mHES2_delivery %>% select(newnhsno, dob, homeadd, admidate, epiend, epistart, epidur, 
#                                          ethnos, dobbaby_1_dv, dobbaby_2_dv, dobbaby_3_dv, dobbaby_4_dv, 
#                                          dobbaby_5_dv, dobbaby_6_dv, dobbaby_7_dv, dobbaby_8_dv, dobbaby_9_dv,
#                                          antedur, biresus_1, biresus_2, biresus_3, biresus_4, biresus_5, 
#                                          biresus_6, biresus_7, biresus_8, biresus_9, birordr_1, birordr_2,
#                                          birordr_3, birordr_4, birordr_5, birordr_6, birordr_7, birordr_8,
#                                          birordr_9, birstat_1, birstat_2, birstat_3, birstat_4, birstat_5,
#                                          birstat_6, birstat_7, birstat_8, birstat_9, birweit_1, birweit_2, 
#                                          birweit_3, birweit_4, birweit_5, birweit_6, birweit_7, birweit_8,
#                                          birweit_8, delmeth_1, delmeth_2, delmeth_3, delmeth_4, delmeth_5,
#                                          delmeth_6, delmeth_7, delmeth_8, delmeth_9, delplac_1, delplac_2,
#                                          delplac_3, delplac_4, delplac_5, delplac_6, delplac_7, delplac_8,
#                                          delplac_9, delstat_1, delstat_2, delstat_3, delstat_4, delstat_5,
#                                          delstat_6, delstat_7, delstat_8, delstat_9, gestat_1, gestat_2, 
#                                          gestat_3, gestat_4, gestat_5, gestat_6, gestat_7, gestat_8, gestat_9,
#                                          well_baby_ind, matage, imd04, imd04rk) %>% 
#                                          rename(mother_nhs=newnhsno, mother_dob=dob) %>% 
#                                          mutate_at(vars(dobbaby_1_dv), ~ymd_hms(.)) %>% 
#                                          mutate_at(vars(dobbaby_2_dv), ~ymd_hms(.)) %>%
#                                          mutate_at(vars(dobbaby_3_dv), ~ymd_hms(.)) %>%
#                                          mutate(dobbaby_1_dv=as.Date(dobbaby_1_dv)) %>% 
#                                          mutate(dobbaby_2_dv=as.Date(dobbaby_2_dv)) %>% 
#                                          mutate(dobbaby_3_dv=as.Date(dobbaby_3_dv)) %>% 
#                                          mutate(birordr_1=as.numeric(birordr_1)) %>% 
#                                          mutate(birordr_2=as.numeric(birordr_2)) %>% 
#                                          mutate(birordr_3=as.numeric(birordr_3))  
                                          

#mHES_delivery <- bind_rows(mHES_delivery, mHES2_delivery)

                                                                  
#LINKAGE of DCS records to mothers delivery records
#Link Based on Mother NHS number and infants DOB
#Multiple iterations of the linkage nessesary in case of twins/triplets born on different days

KS1_BR_mHES_1 <- inner_join(KS1_BR_mTrace, mHES_delivery, by=c("mother_nhs"="mother_nhs","dob_br"="dobbaby_1_dv"))

KS1_BR_mHES_0 <- anti_join(KS1_BR_mTrace, mHES_delivery, by=c("mother_nhs"="mother_nhs","dob_br"="dobbaby_1_dv"))

KS1_BR_mHES_2 <- inner_join(KS1_BR_mHES_0, mHES_delivery, by=c("mother_nhs"="mother_nhs","dob_br"="dobbaby_2_dv"))

KS1_BR_mHES_0 <- anti_join(KS1_BR_mHES_0, mHES_delivery, by=c("mother_nhs"="mother_nhs","dob_br"="dobbaby_2_dv"))

KS1_BR_mHES_3 <- inner_join(KS1_BR_mHES_0, mHES_delivery, by=c("mother_nhs"="mother_nhs","dob_br"="dobbaby_3_dv"))

KS1_BR_mHES_0 <- anti_join(KS1_BR_mHES_0, mHES_delivery, by=c("mother_nhs"="mother_nhs","dob_br"="dobbaby_2_dv"))

KS1_BR_mHES <- bind_rows(KS1_BR_mHES_1, KS1_BR_mHES_2, KS1_BR_mHES_3)

KS1_BR_mHES <- KS1_BR_mHES %>% distinct(id, .keep_all = TRUE)

KS1_BR_mHES_0 <- KS1_BR_mHES_0 %>% distinct(id, .keep_all = TRUE)

path_out = "//filepor10.phe.gov.uk/pid_hes$/Colindale/HCAI/Pfizer Infant Gram-negative/R/Linkage (DCS-BR-mHES)/"
write.csv(KS1_BR_mHES,paste(path_out,'Linkage_Klebsiella_DCS_BR_mHES_Success.csv',sep = ''), row.names = FALSE)

#Create output for unlinked records
#1. Records with mHES from mother but no infant DOB match
#2. Records with no mHES match for infant


KS1_BR_mHES_unliked_HESrecord <- inner_join(KS1_BR_mHES_0, mHES_delivery, by=c("mother_nhs"="mother_nhs"))

KS1_BR_mHES_unliked_HESrecord <- KS1_BR_mHES_unliked_HESrecord %>% select(id, nhs, infant_surname_br, mother_nhs, mother_forname_br,
                                                                          mother_dob_br, mother_surname_br, mother_maidenname_br,
                                                                          infant_forname1_br, infant_forname2_br, 
                                                                          infant_forname3_br, date, dob_br, imd04) %>% 
                                                                          distinct(id, .keep_all = TRUE)

KS1_BR_mHES_unliked_noHESrecord <- anti_join(KS1_BR_mHES_0, mHES_delivery, by=c("mother_nhs"="mother_nhs"))

write.csv(KS1_BR_mHES_unliked_HESrecord,paste(path_out,'Linkage_Klebsiella_DCS_BR_mHES_Unlinked_HESrecords.csv',sep = ''), row.names = FALSE)
write.csv(KS1_BR_mHES_unliked_noHESrecord,paste(path_out,'Linkage_Klebsiella_DCS_BR_mHES_Unlinked_NO-HESrecords.csv',sep = ''), row.names = FALSE)

#Summary Records
S1 <- KS %>% summarise(n=length(id)) %>% mutate(step="cohort") %>% mutate(Rdate="KS")
S2 <- KS1 %>% summarise(n=length(id)) %>% mutate(step="cohort (removed missing NHS)") %>% mutate(Rdate="KS1")
S3 <- KS1_BR %>% summarise(n=length(id)) %>% mutate(step="cohort - BR") %>% mutate(Rdate="KS1_BR")
S4 <- KS1_BR_mTrace %>% summarise(n=length(id)) %>% mutate(step="cohort - BR - Traced Records") %>% mutate(Rdate="KS1_BR_mTrace")
S5 <- KS1_BR_mHES %>% summarise(n=length(id)) %>% mutate(step="cohort - BR - Traced Records - mHES (delivery records)") %>%  mutate(Rdate="KS1_BR_mHES")

linkage_summary <- bind_rows(S1,S2,S3,S4,S5)

linkage_summary<- linkage_summary %>% mutate(percentage_link=(n/length(KS$id))*100)

#Clean-up
remove(S1,S2,S3,S4,S5)
remove(KS1_BR_mHES_1, KS1_BR_mHES_2, KS1_BR_mHES_3)
remove(mHES, mHES2, mHES_delivery, mHES2_delivery, BR, mTraced, nhsvalid)
remove(KS1_BR_mTrace_nomatch, KS1_BR_unlink, KS1_BR_mHES_0, KS1_BR_mHES_unliked_HESrecord, KS1_BR_mHES_unliked_noHESrecord)

