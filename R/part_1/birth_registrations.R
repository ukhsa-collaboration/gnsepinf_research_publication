## Pfizer infant spesis Work Package 3
## DCS to birth registration linkage
## Original Author: Luisa Hallmaier-Wacker

##Import birth registration data
source("path/path.R")

HCAI_births_2010 <- HCAI_births_2010 %>% select(NHSIND, NHSNO, SNAMCH, FNAMCHX_1, FNAMCH1, FNAMCH2, FNAMCH3, DOB, 
                                                FNAMMX_1, FNAMM_1, SNAMM, NAMEMAID, DOBM, BIRTHWGT, MULTTYPE, MULTBTH) %>% 
                                                mutate_at(vars(DOBM),~dmy(.)) %>% 
                                                mutate_at(vars(DOB),~ymd(.)) 

HCAI_births_2011 <- HCAI_births_2011 %>% select(NHSIND, NHSNO, SNAMCH, FNAMCHX_1, FNAMCH1, FNAMCH2, FNAMCH3, DOB, 
                                                FNAMMX_1, FNAMM_1, SNAMM, NAMEMAID, DOBM, BIRTHWGT, MULTTYPE, MULTBTH) %>% 
                                         tibble() %>%  
                                         mutate_at(vars(DOBM),~dmy(.)) %>% 
                                         mutate_at(vars(DOB),~ymd(.)) 


HCAI_births_2012 <- HCAI_births_2012 %>% select(NHSIND, NHSNO, SNAMCH, FNAMCHX_1, FNAMCH1, FNAMCH2, FNAMCH3, DOB, 
                                                FNAMMX_1, FNAMM_1, SNAMM, NAMEMAID, DOBM, BIRTHWGT, MULTTYPE, MULTBTH) %>% 
                                          tibble() %>%                                
                                          mutate_at(vars(DOBM),~dmy(.)) %>% 
                                          mutate_at(vars(DOB),~ymd(.)) 


HCAI_births_2013 <- HCAI_births_2013 %>% select(NHSIND, NHSNO, SNAMCH, FNAMCHX_1, FNAMCH1, FNAMCH2, FNAMCH3, DOB, 
                                                FNAMMX_1, FNAMM_1, SNAMM, NAMEMAID, DOBM, BIRTHWGT, MULTTYPE, MULTBTH) %>% 
                                          tibble() %>%  
                                          mutate_at(vars(DOBM),~dmy(.)) %>% 
                                          mutate_at(vars(DOB),~ymd(.)) 

HCAI_births_2014 <- HCAI_births_2014 %>% select(NHSIND, NHSNO, SNAMCH, FNAMCHX_1, FNAMCH1, FNAMCH2, FNAMCH3, DOB, 
                                                FNAMMX_1, FNAMM_1, SNAMM, NAMEMAID, DOBM, BIRTHWGT, MULTTYPE, MULTBTH) %>% 
                                          tibble() %>%  
                                          mutate_at(vars(DOBM),~dmy(.)) %>% 
                                          mutate_at(vars(DOB),~ymd(.)) 


HCAI_births_2015 <- HCAI_births_2015 %>% select(NHSIND, NHSNO, SNAMCH, FNAMCHX_1, FNAMCH1, FNAMCH2, FNAMCH3, DOB, 
                                                FNAMMX_1, FNAMM_1, SNAMM, NAMEMAID, DOBM, BIRTHWGT, MULTTYPE, MULTBTH) %>% 
                                         tibble() %>%                                           
                                         mutate_at(vars(DOBM),~dmy(.)) %>% 
                                         mutate_at(vars(DOB),~ymd(.)) 


HCAI_births_2016 <- HCAI_births_2016 %>% select(NHSIND, NHSNO, SNAMCH, FNAMCHX_1, FNAMCH1, FNAMCH2, FNAMCH3, DOB, 
                                                FNAMMX_1, FNAMM_1, SNAMM, NAMEMAID, DOBM, BIRTHWGT, MULTTYPE, MULTBTH) %>% 
                                         tibble() %>%    
                                         mutate_at(vars(DOBM),~dmy(.)) %>% 
                                         mutate_at(vars(DOB),~ymd(.)) 


HCAI_births_2017 <- HCAI_births_2017 %>% select(NHSIND, NHSNO, SNAMCH, FNAMCHX_1, FNAMCH1, FNAMCH2, FNAMCH3, DOB, 
                                                FNAMMX_1, FNAMM_1, SNAMM, NAMEMAID, DOBM, BIRTHWGT, MULTTYPE, MULTBTH) %>% 
                                         tibble() %>%    
                                         mutate_at(vars(DOBM),~dmy(.)) %>% 
                                         mutate_at(vars(DOB),~ymd(.)) 


HCAI_births_2018 <- HCAI_births_2018 %>% select(NHSIND, NHSNO, SNAMCH, FNAMCHX_1, FNAMCH1, FNAMCH2, FNAMCH3, DOB, 
                                                FNAMMX_1, FNAMM_1, SNAMM, NAMEMAID, DOBM, BIRTHWGT, MULTTYPE, MULTBTH) %>% 
                                                tibble() %>%                                                 
                                                mutate_at(vars(DOBM),~ymd(.)) %>% 
                                                mutate_at(vars(DOB),~ymd(.)) 


HCAI_births_2019 <- HCAI_births_2019 %>% select(NHSIND, NHSNO, SNAMCH, FNAMCHX_1, FNAMCH1, FNAMCH2, FNAMCH3, DOB, 
                                                FNAMMX_1, FNAMM_1, SNAMM, NAMEMAID, DOBM, BIRTHWGT, MULTTYPE, MULTBTH) %>% 
                                                tibble() %>%    
                                                mutate_at(vars(DOBM),~ymd(.)) %>% 
                                                mutate_at(vars(DOB),~ymd(.)) 

BirthR <- bind_rows(HCAI_births_2010,HCAI_births_2011,HCAI_births_2012,HCAI_births_2013, HCAI_births_2014,
                    HCAI_births_2015, HCAI_births_2016, HCAI_births_2017,HCAI_births_2018,HCAI_births_2019)

##Save file

write.csv(BirthR,'birth_registration_2010_2019.csv',sep = '')

write.csv(BirthR ,paste(path_out,'DSC_Ecoli_Output.csv',sep = ''))
