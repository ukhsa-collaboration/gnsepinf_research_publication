## Link DCS-BR data to HES
##Pathogen: Paeruginosa
##Project: WP3 infant spesis
##Created by: Luisa Hallmaier-Wacker

source("path/path.R")


#Import nessesary data

PA1_hes_collapsed <- PA1_hes_collapsed %>%  mutate_at(vars(spell_end_date),~ymd(.)) %>% 
                                            mutate_at(vars(dob),~ymd(.)) %>% 
                                            mutate_at(vars(spell_start_date),~ymd(.)) %>% 
                                            rename(patient_id=id)




PA1_BR <- PA1_BR %>% select(nhs, dob_br, data_collection_date,id) %>% 
                      mutate_at(vars(dob_br),~ymd(.)) %>% 
                      mutate_at(vars(data_collection_date),~ymd(.)) 

## STAGE 1: link all HES records to DCS DATA ####################################
## join up hospital spell data with mandetory dataset for E.coli
## inner join to do first set of matches NHS DoB
## anti-join to return mismatch results

link1 <- inner_join(PA1_BR,PA1_hes_collapsed,
                    by=c("nhs"="newnhsno",
                         "dob_br"="dob"),
                    na_matches="never")

link1_nhs <- link1 %>% distinct(id, .keep_all = TRUE)

#Create a list of records that have no documented HES records

nomatch1 <- anti_join(PA1_BR,PA1_hes_collapsed,
                    by=c("nhs"="newnhsno",
                         "dob_br"="dob"),
                    na_matches="never")


## STAGE 2: Filter linked output HES record during DCS specimen date ###########

link1<- link1 %>% mutate(
  diff_pos_admit = difftime(
    data_collection_date,
    spell_start_date,
    units = "days")
) %>%
  mutate_if(is.difftime,as.integer) %>%
  mutate(
    pos_in_hospital = data_collection_date %within% interval(spell_start_date,spell_end_date),
    test_pre_admit = diff_pos_admit < 0,
    birth_episode=dob_br %within% interval(spell_start_date,spell_end_date))

#Infants in hospital during positive bact. 

inhos <- link1 %>% filter(pos_in_hospital)

#Generate Output Tables (3.2J-L)

data3.2J <- inhos %>% summarise_at(vars(diff_pos_admit),
                                      list(min=min, Q1=~quantile(., probs = 0.25),
                                           median=median, Q3=~quantile(., probs = 0.75),
                                           max=max))
inhos <- inhos %>% mutate(diff_pos_admit_grp=case_when(diff_pos_admit %in% c(0) ~ "0",
                                                   diff_pos_admit %in% c(1) ~"1",
                                                   diff_pos_admit %in% c(2,3,4,5,6) ~"2-6",
                                                   diff_pos_admit>=7 & diff_pos_admit<90 ~"7-89",
                                                   diff_pos_admit>=90  ~"90+",
                                                            TRUE ~ "NA"))


data3.2K <- inhos %>% group_by(diff_pos_admit_grp) %>% tally()

data3.2L <- inhos %>% group_by(diff_pos_admit_grp, birth_episode) %>% tally()

data3.2L <- data3.2L %>% pivot_wider(names_from = birth_episode, values_from =n)

#Remove created vriable for downstream analysis

inhos <- inhos %>% select(-diff_pos_admit_grp)

                               
#Infants NOT in hospital during positive bact. 
#Note these are all HES records for these infants

notinhos <- anti_join(link1,inhos,
                      by=c("id"="id"),
                      na_matches="never")

#Examining when the infants not in hospital are admitted
#Using diff_pos_admit variable

notinhos_1 <-notinhos %>% filter(diff_pos_admit==-1)

notinhos <- anti_join(notinhos,notinhos_1,
                      by=c("id"="id"),
                      na_matches="never")

notinhos_2 <-notinhos %>% filter(diff_pos_admit==-2) 

notinhos <- anti_join(notinhos,notinhos_2,
                      by=c("id"="id"),
                      na_matches="never")

notinhos_3 <-notinhos %>% filter(diff_pos_admit==-3) 

notinhos <- anti_join(notinhos,notinhos_3,
                      by=c("id"="id"),
                      na_matches="never")

notinhos_4 <-notinhos %>% filter(diff_pos_admit==-4)

notinhos <- anti_join(notinhos,notinhos_4,
                      by=c("id"="id"),
                      na_matches="never")

notinhos_5 <-notinhos %>% filter(diff_pos_admit==-5)

notinhos <- anti_join(notinhos,notinhos_5,
                      by=c("id"="id"),
                      na_matches="never")

notinhos_6 <-notinhos %>% filter(diff_pos_admit==-6)

notinhos <- anti_join(notinhos,notinhos_6,
                      by=c("id"="id"),
                      na_matches="never")

notinhos_7 <-notinhos %>% filter(diff_pos_admit==-7)

notinhos_8plus <- notinhos  %>% distinct(id, .keep_all = TRUE)


#Generate summary records for when infants were admitted if not in hospital during bact positive
S1 <- notinhos_1 %>% summarise(n=length(id)) %>% mutate(days_post_bact="1 day")
S2 <- notinhos_2 %>% summarise(n=length(id)) %>% mutate(days_post_bact="2 day")
S3 <- notinhos_3 %>% summarise(n=length(id)) %>% mutate(days_post_bact="3 day")
S4 <- notinhos_4 %>% summarise(n=length(id)) %>% mutate(days_post_bact="4 day")
S5 <- notinhos_5 %>% summarise(n=length(id)) %>% mutate(days_post_bact="5 day")
S6 <- notinhos_6 %>% summarise(n=length(id)) %>% mutate(days_post_bact="6 day")
S7 <- notinhos_7 %>% summarise(n=length(id)) %>% mutate(days_post_bact="7 day")
S8 <- notinhos_8plus %>% summarise(n=length(id)) %>% mutate(days_post_bact="8+ day")

notinhos_summary <- bind_rows(S1,S2,S3,S4,S5, S6,S7,S8)

rm(S1,S2,S3,S4,S5, S6,S7,S8)

#Generate new cohort for:
#Infants in hospital or admitted 1 day post bacteremia
#To generate output 3.3A

inhos2 <- bind_rows(inhos, notinhos_1, notinhos_2, notinhos_3, notinhos_4, notinhos_5,
                    notinhos_6, notinhos_7)

inhos2 <- inhos2 %>% mutate(
                    length_stay_post_bac = difftime(
                    spell_end_date,
                    data_collection_date,
                    units = "days"))

data3.3A <- inhos2 %>% summarise_at(vars(length_stay_post_bac),
                                   list(min=min, Q1=~quantile(., probs = 0.25),
                                        median=median, Q3=~quantile(., probs = 0.75),
                                        max=max))
inhos2 <- inhos2 %>% mutate(length_stay_post_bac_grp=case_when(length_stay_post_bac %in% c(0) ~ "0",
                                                         length_stay_post_bac %in% c(1) ~"1",
                                                         length_stay_post_bac %in% c(2,3,4,5,6) ~"2-6",
                                                         length_stay_post_bac>=7 & length_stay_post_bac<90 ~"7-89",
                                                         length_stay_post_bac>=90  ~"90+",
                                                          TRUE ~ "NA"))


data3.3A_2 <- inhos2 %>% group_by(length_stay_post_bac_grp) %>% tally()

#Clean-up and save files

remove(link1, link1_nhs, nomatch1, notinhos, notinhos_1, notinhos_2, notinhos_3, notinhos_4, notinhos_5,
       notinhos_6, notinhos_7, notinhos_8plus)

#write.csv(inhos,paste(path_out,'Linkage_Paeruginosa_DCS_infantHES_Success_inhospital.csv',sep = ''), row.names = FALSE)
#write.csv(inhos2,paste(path_out,'Linkage_Paeruginosa_DCS_infantHES_Success_inhospital_OR_admitted_1day_postbac.csv',sep = ''), row.names = FALSE)
#write.csv(notinhos_summary,paste(path_out,'Output_table_NotinHospital_Paeruginosa_DCS_infantHES.csv',sep = ''), row.names = FALSE)
