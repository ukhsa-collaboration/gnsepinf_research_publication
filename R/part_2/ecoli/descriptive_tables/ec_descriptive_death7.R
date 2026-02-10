##New descriptive tables with merged datasets
pacman::p_load(
  rio,            # import/export
  here,           # file pathways
  flextable,      # make HTML tables 
  officer,        # helper functions for tables
  tidyverse,
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  gt,
  rstatix,      # statistics
  corrr,        # correlation analayis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable     # converting tables to HTML
)



#########################################
##TABLES WITH EC_FINAL DATASET
##TABLE 1A. will include descriptive information on sociodemographics 
##I will relevel the variables here as well
ec_final <- ec_final %>% 
  mutate(
    imd_quintile = fct_relevel(
      imd_quintile,
      "1st",
      after = 0
    )
  ) %>% 
  mutate(
    multibirth_bi = fct_relevel(
      multibirth_bi,
      "Single",
      after = 0
    )
  ) %>% 
  mutate(
    infant_age_cat = fct_relevel(
      infant_age_cat,
      "0-3 days",
      after = 0
    ) %>% 
      factor(levels = c(
        "0-3 days",
        "4-28 days",
        "29-90 days",
        "90-365 days"
      ))
  ) %>% 
  mutate(
    gestage_grp = fct_relevel(
      gestage_grp,
      "Term",
      after = 0
    ) %>% 
      factor(levels = c(
        "Term",
        "Moderate Preterm",
        "Very Preterm",
        "Extremely Preterm"
      )
      )
  ) %>% 
  mutate(
    birthwgt_grp = fct_relevel(
      birthwgt_grp,
      "Normal",
      after = 0
    ) %>% 
      factor(levels = c(
        "Normal",
        "Low",
        "Very Low",
        "Extremely Low"
      )
      )
  ) %>% 
  mutate(
    antenatal_stay = fct_relevel(
      antenatal_stay,
      "0 days",
      after = 0
    )
  ) %>% 
  mutate(
    delivery_cat = fct_relevel(
      delivery_cat,
      "vaginal",
      after = 0
    )
  ) 

#############################################
##TABLE 1A. will include descriptive information on sociodemographics 
#########################################################
###run TABLE1a
ectable1a_7 <- ec_final %>% 
  mutate(death_7 = ifelse(death_7 == 1,
                           "Death",
                           "Survive")) %>% 
  select(sex,
         infant_age_cat,
         matage_grp,
         ethnicity_des2,
         imd_quintile,
         address,
         urban_rural,
         year_sample,
         death_7
  ) %>% 
  tbl_summary(by= death_7,
              missing = "no",
              percent = "column",
              label = list(
                sex ~ "Sex (infant)",
                infant_age_cat ~ "Infant age group",
                matage_grp ~ "Maternal age group",
                ethnicity_des2 ~ "Ethnicity group (mother)",
                imd_quintile ~ "Multiple deprivation quintile (mother)",
                address ~ "Region of origin",
                urban_rural ~ "Geographic area",
                year_sample ~ "Year"
              ),
              digits = list(
                all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
                all_categorical() ~ 0 
              )) %>%
  add_p()%>% 
  add_overall() %>% 
  add_n() %>%
  modify_spanning_header(c("stat_1", 
                           "stat_2") ~ "**Episode outcome**") %>% 
  bold_labels() #%>% 
  #as_gt() #%>% 
# tab_header(
#   title = "Table 1a. Sociodemographic characteristics of mothers and infants with E. coli bacteremia in England 2011-2019"
# ) 


####################################################
#### TABLE 2A. with the birth details
###################################################
ectable2a_7 <- 
  ec_final %>% 
  mutate(death_7 = ifelse(death_7 == 1,
                           "Death",
                           "Survive")) %>% 
  select(gestage_grp,
         birthwgt_grp,
         multibirth_bi,
         delivery_cat,
         delivery_location,
         mother_epidur,
         antenatal_stay,
         death_7
  ) %>% 
  tbl_summary(by= death_7,
              missing = "no",
              percent = "column",
              label = list(
                gestage_grp ~ "Gestational age group",
                birthwgt_grp ~ "Birth weight group",
                multibirth_bi ~ "Single or multiple birth",
                delivery_cat ~ "Delivery type",
                delivery_location ~ "Delivery location",
                mother_epidur ~ "Maternal hospital stay lenght",
                antenatal_stay ~ "Antenatal stay lenght"
              ),
              digits = list(
                all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
                all_categorical() ~ 0 
              )) %>%
  add_p()%>% 
  add_overall() %>% 
  add_n() %>%
  modify_spanning_header(c("stat_1", 
                           "stat_2") ~ "**Episode outcome**") %>% 
  bold_labels() #%>% 
 # as_gt() #%>% 
# tab_header(
#   title = "Table 2a. Birth characteristics by outcome of mothers and infants with an E. coli episode in England 2011-2019"
# ) 

##########################################
##TABLE 3a. includes comorbidities

#######################################################

ectable3a_7 <- 
  ec_final %>% 
  mutate(death_7 = ifelse(death_7 == 1,
                           "Death",
                           "Survive")) %>% 
  select(death_7,
         jaundice,
         hemato,
         neoplasm_carcinom,
         endo_nutri_meta,
         accident_injury,
         congenital,
         perinatal_other,
         perinat_resp_cardio,
         pregnancy_complication_trauma,
         genitourinary,
         musculo,
         skin,
         ear,
         eye,
         digestive,
         circulatory,
         other_infectious,
         neuro_nervo,
         temp_reg
  ) %>% 
  tbl_summary(by= death_7,
              missing = "no",
              percent = "column",
              label = list(
                jaundice ~ "Jaundice",
                hemato ~ "Hematological & hemorrhagic",
                neoplasm_carcinom ~ "Neoplasm & carcinoma",
                endo_nutri_meta ~ "Endocrine, nutritional & metabolic",
                accident_injury ~ "Accident & injury",
                congenital ~ "Congenital",
                perinatal_other ~ "Perinatal other disorders",
                perinat_resp_cardio ~ "Perinatal respiratory & cardiovascular",
                pregnancy_complication_trauma ~ "Pregnancy complication & birth trauma",
                genitourinary ~ "Genitourinary",
                musculo ~ "Musculoskeletal",
                skin ~ "Skin",
                ear ~ "Ear",
                eye ~ "Eye",
                digestive ~ "Digestive",
                circulatory ~ "Circulatory",
                other_infectious ~ "Other infectious",
                neuro_nervo ~ "Neurological & nervous system",
                temp_reg ~ "Temperature regulation"
              ),
              digits = list(
                all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
                all_categorical() ~ 0 
              )) %>% 
  add_p()%>% 
  add_overall() %>% 
  add_n() %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Episode outcome**") %>% 
  bold_labels() #%>% 
 # as_gt() #%>% 
# tab_header(
#   title = "Table 3a. Comorbidity characteristics of infants with an E. coli bacteremia episode in England 2011-2019"
# ) 

#######################
###TABLE 4a includes pre-bactaeremia procedures

########################
ectable4a_7 <- 
  ec_final %>% 
  mutate(death_7 = ifelse(death_7 == 1,
                           "Death",
                           "Survive")) %>% 
  select(death_7,
         cardiac_surgery_30d_prior,
         bowel_gastric_surgery_30d_prior,
         urinary_catheter_28d_prior,
         umbilical_central_catheter_28d_prior,
         ventilation_28d_prior,
         intubation_28d_prior
  ) %>% 
  tbl_summary(by= death_7,
              missing = "no",
              percent = "column",
              label = list(
                cardiac_surgery_30d_prior ~ "Cardiac surgery",
                bowel_gastric_surgery_30d_prior ~ "Bowel/gastric surgery",
                urinary_catheter_28d_prior ~ "Urinary catheter",
                umbilical_central_catheter_28d_prior ~ "Umbilical/central catheter",
                ventilation_28d_prior ~ "Invasive ventilation",
                intubation_28d_prior ~ "Intubation"
              ),
              digits = list(
                all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
                all_categorical() ~ 0 
              )) %>% 
  add_p()%>% 
  add_overall() %>% 
  add_n() %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Episode outcome**") %>% 
  bold_labels() #%>% 
  #as_gt() #%>% 
# tab_header(
#   title = "Table 4a. Procedures performed on infants 28 days before their first E. coli bacteremia episode in England 2011-2019",
#   subtitle = "(30 days for surgeries)"
# ) 

#########################
###TABLE 5 includes post-bacteriaemia procedures
###############################################

ectable5a_7 <- 
  ec_final %>% 
  mutate(death_7 = ifelse(death_7 == 1,
                           "Death",
                           "Survive")) %>% 
  select(death_7,
         cardiac_surgery_90d_post,
         bowel_gastric_surgery_90d_post,
         urinary_catheter_90d_post,
         umbilical_central_catheter_90d_post,
         ventilation_90d_post,
         intubation_90d_post
  ) %>% 
  tbl_summary(by= death_7,
              missing = "no",
              percent = "column",
              label = list(
                cardiac_surgery_90d_post ~ "Cardiac surgery",
                bowel_gastric_surgery_90d_post ~ "Bowel/gastric surgery",
                urinary_catheter_90d_post ~ "Urinary Catheter",
                umbilical_central_catheter_90d_post ~ "Umbilical/central catheter",
                ventilation_90d_post ~ "Invasive ventilation",
                intubation_90d_post ~ "Intubation"
              ),
              digits = list(
                all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
                all_categorical() ~ 0 
              )) %>% 
  add_p()%>% 
  add_overall() %>% 
  add_n() %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Episode outcome**") %>% 
  bold_labels() #%>% 
  #as_gt() #%>% 
# tab_header(
#   title = "Table 5a. Procedures performed on infants 1-28 days after their first E. coli bacteremia episode in England 2011-2019",
# ) 


#############

##TABLE 6. General hospital characteristics
################# 
ectable6a_7 <- 
  ec_final %>% 
  mutate(death_7 = ifelse(death_7 == 1,
                           "Death",
                           "Survive")) %>% 
  select(death_7,
         apportionment_rule_category,
         sample_onset,
         antenatal_stay,
         number_episodes,
         mother_epidur   
  ) %>% 
  tbl_summary(by= death_7,
              missing = "no",
              percent = "column",
              label = list(
                apportionment_rule_category ~ "Context of acquisition",
                sample_onset ~ "Onset type",
                antenatal_stay ~ "Antenatal stay",
                number_episodes ~ "Number of episodes/infant",
                mother_epidur ~ "Lenght of stay - mother"
              ),
              digits = list(
                all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
                all_categorical() ~ 0 
              )) %>% 
  add_p()%>% 
  add_overall() %>% 
  add_n() %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Episode outcome**") %>% 
  bold_labels() #%>% 
  
  #as_gt() #%>% 
# tab_header(
#   title = "Table 6a. Other information from mothers and infants with an E.coli bacteremia in England 2011-2019"
# ) 
