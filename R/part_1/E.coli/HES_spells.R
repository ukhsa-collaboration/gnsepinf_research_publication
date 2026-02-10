## Group HES Data into Spells
## Adapted from SUS script from PEACH project
##Pathogen: Ecoli
##Project: WP3 infant spesis
##Created by: Luisa Hallmaier-Wacker
source("path/path.R")


EC1_hes <- EC1_hes %>% clean_names()

date_fields <- EC1_hes %>% select(contains("date")) %>% names()
diag_fields <- EC1_hes %>% select(contains("diag")) %>% names()
pro_fields <- EC1_hes %>% select(contains("procode")) %>% names()
pii_fields <- c("newnhsno","valid_nhs","lopatid","dob",
                "sex","ethnos","rescty","homeadd", "imd04rk")

## FIELD MAINTENENCE ############################################################
## clean fields
# regular attenders have  elective day/night admissions, as part of regular ongoing care

EC1_hes <- EC1_hes %>%
  mutate_at(date_fields,~dmy(.))%>%
  mutate(dob=parse_date_time(dob, orders = c('ymd', "dmy"))) %>% 
  mutate(epistart=parse_date_time(epistart, orders = c('ymd', "dmy"))) %>% 
  mutate(epiend=parse_date_time(epiend, orders = c('ymd', "dmy"))) %>% 
  mutate(ethnos=if_else(ethnos %in% c("99","X","x",NA),"99", toupper(substring(ethnos,1,1)))) %>%
  mutate(sex=ifelse(sex %in% c(1,2),sex,9), sex=factor(sex,levels=c(1,2,9), labels=c("Male","Female","Not specified"))) %>%
  mutate(regular_attender=classpat %in% c("3","4")) %>% 
  mutate(dob=as.Date(dob)) %>% 
  mutate(epistart=as.Date(epistart)) %>%
  mutate(epiend=as.Date(epiend))

## GROUP PATIENT RECORDS ########################################################
EC1_hes <- patient_id(data=EC1_hes,
                  nhs_number = "newnhsno",
                  hospital_number = "lopatid",
                  sex = "sex",
                  date_of_birth = "dob",
                  sort_date = "epistart")

sum(EC1_hes$valid_nhs)/nrow(EC1_hes)

## DIAGNOSTIC ICD10 CODES [PROVIDER SPELLS] #####################################
## take these out first so we can collapse the dataset by spells

diagnostic_icd10 <- EC1_hes %>%
  ungroup() %>%
  select(id,
         procode3,
         provspnops,
         diag_fields,
         nhsnoind)%>%
  mutate_at(vars(diag_code4),~substr(.,1,4)) %>%
  pivot_longer(
    cols = diag_code4,
    names_to = "diag_n",
    values_to = "icd10_diag",
    values_drop_na = T
  ) %>%
  rename(provider_code = procode3,
         hospital_provider_spell_no = provspnops,
         nhs=nhsnoind) %>% 
  distinct()


## DIAGNOSTIC ICD10 CODES [PROVIDER SPELLS] #####################################
## take these out first so we can collapse the dataset by spells

proceedure_icd10 <- EC1_hes %>%
  ungroup() %>%
  select(id,
         procode3,
         provspnops,
         opertn) %>%
  mutate_at(vars(opertn),~substr(.,1,4)) %>%
  pivot_longer(
    cols = opertn,
    names_to = "op_n",
    values_to = "icd10_proc",
    values_drop_na = T
  ) %>%
  rename(provider_code = procode3,
         hospital_provider_spell_no = provspnops) %>% 
  distinct()

### SPELLS [REQUIES SENSE CHECK] ################################################
##  2+ episodes or spells overlapping in same site = group together [not done yet]
##  keep open spells first.

EC1_hes2 <- EC1_hes %>%
  select(-c(diag_fields, opertn, opertn_idx)) %>%
  arrange(
    id,
    procode3,
    admidate,
    epistart,
    epiend,
    epiorder
  ) %>%
  distinct(id, 
           procode3, 
           provspnops, 
           epistart, 
           .keep_all = T)

## WHERE THE START AND END DATES ARE FLIPPED, SWTICH REVESE THEM: DATA QUALITY EDGE CASE

EC1_hes2 <- EC1_hes2 %>%
  ungroup() %>%
  arrange(epistart,epiend) %>% 
  mutate(spell_id = 1:n()) %>% 
  group_by(id, procode3, provspnops) %>%
  mutate(spell_id = spell_id[1]) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(spell_count = n(),
         spell_counter=1:n()) %>%
  ungroup() %>% 
  mutate(proxy_missing = case_when(
    is.na(epiend) & spell_counter==spell_count ~ 1,
    !is.na(epiend) & epiend<epistart ~ 4,
    TRUE ~ 0)) 

EC1_hes2 <- EC1_hes2 %>% 
  mutate(spell_end_date = case_when(
      proxy_missing=="0" ~ epiend,
      proxy_missing=="1" ~ epiend,
      proxy_missing=="4" ~ epistart,
      TRUE ~ epiend)
  ) %>% 
  mutate(
    spell_start_date = if_else(
      proxy_missing==4,
      epiend,
      epistart
    )
  ) %>% 
  ungroup() 

## CIP spells [PHE PROVIDER ONLY VARIANT] #######################################
## group SUS into CIPs [requires sense check]
## NHS D confirmed HSCIC CIP methodology (published 2014) is valid on 14/05/2020:
#   http://content.digital.nhs.uk/media/11859/Provider-Spells-Methodology/pdf/Spells_Methodology.pdf
#   rule 1 and 2 are criteria
#   rule 3 is exclusion
#   Regular attender episodes are considered as separate units of care that
#     should not be linked to other episodes or spells as they form
#     single episode-single provider CIP spells.
## Due to COVID19 HCAI attribution methods, it was decided that CIPs where a
#   patient transfers trust should be considered as seperate spells
#   to ensure more accurate reporting per trust

# belong to same patient
EC1_hes2 <- EC1_hes2 %>%
  ungroup() %>% 
  group_by(id) %>%
  arrange(spell_start_date,spell_end_date,.by_groups=TRUE) 

# difference between admission and discharge is <0 days
#Simon: I altered the time difference allowed for CIP spells here. If you want to include it again
#add 1,2 in the last row.


EC1_hes2 <- EC1_hes2 %>%
  mutate(cip_2daydiff =
           as.numeric(
             difftime(
               lead(spell_start_date),
               spell_end_date,
               units="days")
           ) %in% c(0)
  ) 

##) %in% c(0,1,2)
##) 

# a transfer has taken place (based on these criteria)
# used the simple criteria, as we dont need to determine transfer type (1,2,3)
EC1_hes2 <- EC1_hes2 %>%
  mutate(cip_transfer = 
           disdest %in%
           c("49", "50", "51", "52", "53", "84") |
           lead(admisorc) %in%
           c("49", "50", "51", "52", "53", "87") |
           lead(admimeth) %in%
           c("2B", "81")
  )

# exclusion criteria
EC1_hes2 <- EC1_hes2 %>%
  mutate(cip_exclude =
           disdest %in% c("19") &&
           lead(admisorc) %in% c("51") &&
           lead(admimeth) %in% c("21")
  )

# group records using cip_valid
EC1_hes2 <- EC1_hes2 %>%
  mutate(cip_valid=cip_2daydiff & cip_transfer & !cip_exclude & !regular_attender) %>% 
  mutate(cip_valid=if_else(is.na(cip_valid),FALSE,cip_valid)) %>% 
  mutate(cip_valid = if_else(
    !cip_valid &
      lag(cip_valid) &
      lag(spell_counter) == spell_counter - 1 &
      spell_count > 1,
    TRUE,
    cip_valid
  )) %>% 
  mutate(cip_valid=if_else(is.na(cip_valid),FALSE,cip_valid))  

## translate across spell
## cant just use group_by as individuals may have multiple CIPs
## PHE HCAI REPORTING VARIANT: MUST INCLUDE PROVIDER TRUST AS WE ARE NOT ALLOWING TRUST TRANSFERS
## SENSITIVITY TEST 15/05/2020 there were 8 CIPs involving a change of trust

EC1_hes2 <- EC1_hes2 %>%
  group_by(id,cip_valid) %>%
  arrange(spell_start_date,spell_end_date,.by_group=TRUE) %>% 
  mutate(cip_counter=ifelse(cip_valid,n(),0))

EC1_hes2 <- EC1_hes2 %>%
  ungroup() %>% 
  group_by(id) %>%
  arrange(spell_start_date,spell_end_date,.by_group=TRUE)

## to allow iteration within the groups
## slow but works
for(i in 1:max(EC1_hes2$cip_counter)){
  print(paste("single provider Continuous Inpatient Spells:",i))
  EC1_hes2 <- EC1_hes2 %>%
    mutate(cip_id = ifelse(
      cip_valid & lag(cip_valid,default=F) & 
        spell_counter == lag(spell_counter,default=0)+1,
      lag(spell_id),
      spell_id
    )
    )
}

## transfer across the CIP spells.
EC1_hes2 <- EC1_hes2 %>% 
  group_by(id,cip_id) %>%
  mutate(cip_start_date = if_else(cip_valid,min(spell_start_date),spell_start_date),
         cip_end_date = if_else(cip_valid,max(spell_end_date),spell_end_date)
  ) %>%
  ungroup()

## MEGA SPELL [needs check] #####################################################
## This needs to be done after the CIPs as it cleans up transfers and overlaps
## NHSD methods allow for these immpossible scenarios eg. patient in 2 locations

# Within an NHS provider, when there is overlap between the admission and
# discharge dates between two separate spells, these will be joined into a
# single spell. This may occur where one spell is completely encapsulated
# within the other, or where the end of one spell and the start of another overlap.

## indx solution from stackoverflow
## uses vectorisation so its fast
## [-n()] removes the last item from the vector, due to the introduction of 0 at start
## groups two events if they overlap
EC1_hes2 <- EC1_hes2 %>% 
  group_by(id) %>% 
  arrange(cip_start_date,cip_end_date,.by_group=T) %>%
  mutate(indx = c(0, 
                  cumsum(as.numeric(lead(cip_start_date)) >
                           cummax(as.numeric(cip_end_date)))[-n()])
  ) %>%  
  group_by(id,indx) %>% 
  mutate(mega_spell_start_date=min(cip_start_date),
         mega_spell_end_date=max(cip_end_date)) %>% 
  mutate(mega_spell_id=first(cip_id))

## REJOIN AND COLLAPSE SPELLS ###################################################

## collect PID
pid <- EC1_hes2 %>%
  ungroup() %>% 
  collect() %>%
  select(pii_fields,
         procode3,
        mega_spell_id,
        id
  ) %>%
  rename(local_patient_identifier = lopatid,
         county_of_residence = rescty,
         provider_code = procode3,
         patient_postcode = homeadd) %>% 
  distinct()


## collect spell data
spell_data <- EC1_hes2 %>%
  ungroup() %>% 
  select(
    id,
    procode3,
    mega_spell_id,
    provspnops,
    admisorc,
    admimeth,
    dismeth,
    disdest,
    spell_start_date,spell_end_date,proxy_missing,
    cip_start_date,cip_end_date,
    mega_spell_start_date,mega_spell_end_date,epitype, imd04rk
  ) %>%
  collect() %>%
  rename(admit_source = admisorc,
         admit_method = admimeth,
         discharge_method = dismeth,
         discharge_destination = disdest,
         provider_code = procode3,
         hospital_provider_spell_no = provspnops) %>%
  mutate_at(vars(c(admit_source,
                   admit_method,
                   discharge_method,
                   discharge_destination
  )),
  ~str_replace(.,"99",NA_character_)) %>%
  mutate_at(vars(c(admit_source,
                   admit_method,
                   discharge_method,
                   discharge_destination
  )),
  ~factor(.,ordered=T,exclude=NA)) %>%
  distinct()

## join all the spell data to icd10
EC1_hes3 <- left_join(spell_data, diagnostic_icd10,
                 by = c("id",
                        "provider_code",
                        "hospital_provider_spell_no")
)



## take the full data and collapse on the new mega_spell_id
EC1_hes3 <- EC1_hes3 %>%
  distinct(id, mega_spell_id, icd10_diag, .keep_all = T) %>%
  group_by(id, mega_spell_id, provider_code) %>%
  arrange(spell_start_date, spell_end_date, diag_n, .by_group=T) %>%
  mutate(diag_order = seq(1:n())) %>%
  summarise(
    diag_count = max(diag_order),
    diag_concat = paste(icd10_diag, collapse = ";"),
    admit_source = first(admit_source),
    admit_method = first(admit_method),
    discharge_method = last(discharge_method),
    discharge_destination = last(discharge_destination),
    spell_start_date = min(mega_spell_start_date, na.rm = T),
    spell_end_date = last(mega_spell_end_date),
    proxy_missing = last(proxy_missing),
    spell_id_concat = paste(hospital_provider_spell_no, collapse = ";"),
    .groups="drop"
  ) %>%
  ungroup() %>% 
  distinct(id, mega_spell_id, .keep_all = T)

#Procedural codes will be added seperatly for better concantination. 
EC1_hes4 <- left_join(spell_data, proceedure_icd10,
                      by = c("id",
                             "provider_code",
                             "hospital_provider_spell_no")
)

EC1_hes4 <- EC1_hes4 %>%
  distinct(id, mega_spell_id, icd10_proc, .keep_all = T) %>%
  group_by(id, mega_spell_id, provider_code) %>%
  arrange(spell_start_date, spell_end_date, .by_group=T) %>%
  summarise(proc_concat = paste(icd10_proc, collapse = ";")) %>%
  ungroup() %>% 
  distinct(id, mega_spell_id, .keep_all = T)

EC1_hes4 <- EC1_hes4 %>% 
  select(mega_spell_id,id,proc_concat)

EC1_hes5 <- left_join(EC1_hes3, EC1_hes4,
                      by = c("id",
                             "mega_spell_id")
)



##add length of stay variable
EC1_hes5 <- EC1_hes5 %>%
  mutate(
    length_of_stay = as.integer(difftime(spell_end_date,
                                         spell_start_date,
                                         unit = "days"))
  ) 

print("STAGE 8: SUS spells collapsed")
print(difftime(Sys.time(),timer))

## join PID with collapsed spells
EC1_hes_collapsed <- right_join(pid,EC1_hes5,
                  by=c("id",
                       "provider_code",
                       "mega_spell_id")) %>% 
  distinct(id, mega_spell_id, .keep_all = T)

print("STAGE 9: joined PID to SUS Spell data")
print(difftime(Sys.time(),timer))

### SAVE Collapsed HES Data to the Porton Drive #################################################

write.csv2(EC1_hes_collapsed,paste(path_out,'DSC_Ecoli_HES_spells_collapsed_concat_proceedures_diagnostic_codes.csv',sep = ''), row.names=FALSE)

##################################################################################################

##Produce output for HES records (non-collapsed spells with individual episodes per row)
#Multiple rows can fall within one spell.The mega-spell id identifies each spell
#Mega-spells are assigned (represent finial spell-admit/discharge date)
#Note that there are HES records from NHS numbers that we have identified as non-infants 
#

## collect spell data
EC1_hes_full <- EC1_hes2 %>%
  ungroup() %>% 
  select(
    id,
    procode3,
    mega_spell_id,
    provspnops,
    admisorc,
    admimeth,
    dismeth,
    disdest,
    spell_start_date,spell_end_date,proxy_missing,
    cip_start_date,cip_end_date,
    mega_spell_start_date,mega_spell_end_date,epitype, imd04rk,newnhsno, valid_nhs, 
    lopatid, dob, sex, ethnos, rescty, homeadd
  ) %>%
  rename(local_patient_identifier = lopatid,
         county_of_residence = rescty,
         patient_postcode = homeadd) %>% 
  collect() %>%
  rename(admit_source = admisorc,
         admit_method = admimeth,
         discharge_method = dismeth,
         discharge_destination = disdest,
         provider_code = procode3,
         hospital_provider_spell_no = provspnops) %>%
  mutate_at(vars(c(admit_source,
                   admit_method,
                   discharge_method,
                   discharge_destination
  )),
  ~str_replace(.,"99",NA_character_)) %>%
  mutate_at(vars(c(admit_source,
                   admit_method,
                   discharge_method,
                   discharge_destination
  )),
  ~factor(.,ordered=T,exclude=NA)) %>%
  distinct()



## join all the spell data to diagnostic icd10
EC1_hes_diag <- left_join(EC1_hes_full, diagnostic_icd10,
                      by = c("id",
                             "provider_code",
                             "hospital_provider_spell_no"
                             )
)

## join all the spell data to procedure icd10
EC1_hes_proc <- left_join(EC1_hes_full, proceedure_icd10,
                          by = c("id",
                                 "provider_code",
                                 "hospital_provider_spell_no")
)

### SAVE NON-collapsed HES Data to the Porton Drive #################################################

write.csv2(EC1_hes_diag,paste(path_out,'DSC_Ecoli_HES_spells_NOTcollapsed_diagnostic_codes_for_episodes.csv',sep = ''), row.names=FALSE)
write.csv2(EC1_hes_proc,paste(path_out,'DSC_Ecoli_HES_spells_NOTcollapsed_proceedures_codes_for_episodes.csv',sep = ''), row.names=FALSE)


##Clean-up
rm(EC1_hes1, EC1_hes2, EC1_hes3, EC1_hes4, EC1_hes5,pid, proceedure_icd10, spell_data, EC1_hes, diagnostic_icd10)

