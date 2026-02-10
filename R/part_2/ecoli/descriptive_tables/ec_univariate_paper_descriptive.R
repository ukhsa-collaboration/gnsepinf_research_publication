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


#############################################
##TABLE 1b. will include descriptive information on sociodemographics 
#########################################################
###run TABLE1b
ectable1b_paper <- ec_univariate_paper %>% 
  mutate(death_30 = ifelse(death_30 == 1,
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
         death_30
  ) %>% 
  tbl_summary(by= death_30,
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
  bold_labels() %>% 
  as_gt() %>% 
  tab_header(
    title = "Table 1b. Sociodemographic characteristics of mothers and infants with E. coli bacteremia in England 2011-2019"
  ) 


####################################################
#### TABLE 2b. with the birth details
###################################################
ectable2b <- 
  ec_univariate_paper %>% 
  mutate(death_30 = ifelse(death_30 == 1,
                           "Death",
                           "Survive")) %>% 
  select(gestage_grp,
         birthwgt_grp,
         multibirth_bi,
         delivery_cat,
         delivery_location,
         mother_epidur,
         antenatal_stay,
         death_30
  ) %>% 
  tbl_summary(by= death_30,
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
  bold_labels() %>% 
  as_gt() %>% 
  tab_header(
    title = "Table 2b. Birth characteristics by outcome of mothers and infants with an E. coli episode in England 2011-2019"
  ) 

##########################################
##TABLE 3b. includes comorbidities

#######################################################

ectable3b <- 
  ec_univariate_paper %>% 
  mutate(death_30 = ifelse(death_30 == 1,
                           "Death",
                           "Survive")) %>% 
  select(death_30,
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
  tbl_summary(by= death_30,
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
                temp_reg = "Temperature regulation"
              ),
              digits = list(
                all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
                all_categorical() ~ 0 
              )) %>% 
  add_p()%>% 
  add_overall() %>% 
  add_n() %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Episode outcome**") %>% 
  bold_labels() %>% 
  as_gt() %>% 
  tab_header(
    title = "Table 3b. Comorbidity characteristics of infants recorded before their first E. coli bacteremia episode in England 2011-2019"
  ) 

#######################
###TABLE 4b includes pre-bactaeremia procedures

########################
ectable4b <- 
  ec_univariate_paper %>% 
  mutate(death_30 = ifelse(death_30 == 1,
                           "Death",
                           "Survive")) %>% 
  select(death_30,
         cardiac_surgery_30d_prior,
         bowel_gastric_surgery_30d_prior,
         urinary_catheter_28d_prior,
         umbilical_central_catheter_28d_prior,
         ventilation_28d_prior,
         intubation_28d_prior
  ) %>% 
  tbl_summary(by= death_30,
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
  bold_labels() %>% 
  as_gt() %>% 
  tab_header(
    title = "Table 4b. Procedures performed on infants 28 days before their first E. coli bacteremia episode in England 2011-2019",
    subtitle = "(30 days for surgeries)"
  ) 

#########################
###TABLE 5 includes post-bacteriaemia procedures
###############################################

ectable5b <- 
  ec_univariate_paper %>% 
  mutate(death_30 = ifelse(death_30 == 1,
                           "Death",
                           "Survive")) %>% 
  select(death_30,
         cardiac_surgery_90d_post,
         bowel_gastric_surgery_90d_post,
         urinary_catheter_90d_post,
         umbilical_central_catheter_90d_post,
         ventilation_90d_post,
         intubation_90d_post
  ) %>% 
  tbl_summary(by= death_30,
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
  bold_labels() %>% 
  as_gt() %>% 
  tab_header(
    title = "Table 5b. Procedures performed on infants 1-28 days after their first E. coli bacteremia episode in England 2011-2019",
  ) 


#############

##TABLE 6. General hospital characteristics
################# 
ectable6b <- 
  ec_univariate_paper %>% 
  mutate(death_30 = ifelse(death_30 == 1,
                           "Death",
                           "Survive")) %>% 
  select(death_30,
         apportionment_rule_category,
         sample_onset,
         antenatal_stay,
         number_episodes,
         mother_epidur   
  ) %>% 
  tbl_summary(by= death_30,
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
  bold_labels() %>% 
  as_gt() %>% 
  tab_header(
    title = "Table 6b. Other information from mothers and infants with an E.coli bacteremia in England 2011-2019"
  ) 



###################

