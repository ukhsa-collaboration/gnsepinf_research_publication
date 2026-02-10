##PIPELINE FOR WORKPACKAGE 3
##LINKAGE OF DCS to ONS birth registrations
##Created by: Luisa Hallmaier-Wacker

#Import birth registration
source("path/path.R")

#Rename for easier handeling
BR <- BR %>% 
  rename(patient_nhs_number=NHSNO,
         patient_date_of_birth=DOB) %>% 
  mutate_at(vars(contains("date")),~ymd(.))

EC1 <- EC1 %>% 
  rename(patient_nhs_number=nhs,
         patient_date_of_birth=dob)


## STAGE 1: link BR DATA TO EC1 ####################################
## inner join to do first set of matches NHS NUMBER and DOB
## inner join to do first set of matches NHS NUMBER ONLY
## anti-join to return unmatched EC1 results
## export unmatched EC1 results and send to Nick for futher analysis

## na_matches may be deprecated in v1.0
link1 <- inner_join(EC1,BR,
                    by=c("patient_nhs_number",
                         "patient_date_of_birth"),
                    suffix=c("_dcs","_br"),
                    na_matches="never")

nomatch1 <- anti_join(EC1,BR,
                    by=c("patient_nhs_number",
                         "patient_date_of_birth"),
                    suffix=c("_dcs","_br"),
                    na_matches="never")

link2 <- inner_join(nomatch1,BR,
                    by=c("patient_nhs_number"),
                    suffix=c("_dcs","_br"),
                    na_matches="never")


nomatch2 <- anti_join(EC1,BR,
                      by=c("patient_nhs_number"),
                      suffix = c("_cov", "_sus"),
                      na_matches = "never")

# DCS records with a invalid NHS number (need to be traced with SGSS dataset)
EC1_invalid_NHS <- nomatch2 %>% filter(!valid_nhs)

# DCS records with a valid NHS number but not match in birth registrations
EC1_valid_NHS <- nomatch2 %>% filter(valid_nhs) %>% distinct(patient_nhs_number)



