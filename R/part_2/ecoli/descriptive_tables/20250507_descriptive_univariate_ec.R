ec_descriptive_univariate <- 
  ec_univariate_paper %>% 
  mutate(death_90 = ifelse(death_90 == 1,
                           "Death",
                           "Survive")) %>% 
  select(sex,
         ethnicity_des2,
         imd_quintile,
         address,
         urban_rural,
         matage_grp,
         gestage_grp,
         birthwgt_grp,
         multibirth_bi,
         apportionment_rule_category,
         left_hospital,
         mother_epidur,
         #antenatal_stay,
         delivery_cat,
         #delivery_location,
         death_30,
         infant_age_cat,
         delivery_cat,
         umbilical_central_catheter_28d_prior,
         ventilation_28d_prior,
         jaundice,
         hemato,
         endo_nutri_meta,
         accident_injury,
         congenital,
         perinatal_other,
         perinat_resp_cardio,
         pregnancy_complication_trauma,
         genitourinary,
         skin,
         eye,
         digestive,
         circulatory,
         other_infectious,
         temp_reg
         ) %>% 
  tbl_summary(by= death_30,
              missing = "no",
              percent = "row",
              label = list(
                sex ~ "Sex (infant)",
                ethnicity_des2 ~ "Ethnicity group (mother)",
                imd_quintile ~ "Multiple deprivation quintile (mother)",
                #address ~ "Region of origin",
                urban_rural ~ "Geographic area",
                matage_grp ~ "Maternal age group",
                gestage_grp ~ "Gestational age group",
                birthwgt_grp ~ "Birth weight group",
                #multibirth_grp ~ "Type of birth",
                multibirth_bi ~ "Single or multiple birth",
                #age_description ~ "Infant age group",
                #apportionment_rule_category ~ "Onset (apportionment)",
                #number_episodes ~ "Number of episodes",
                #left_hospital ~ "left hospital since DOB",
                #left_hospital_period ~ "lenght dob-admission",
                #admi_onset ~ "onset - admission",
                #sample_onset ~ "Episode onset",
                #mother_epidur ~ "Mother hospital stay lenght",
                #antenatal_stay ~ "Antenatal stay lenght",
                #resus ~ "Resucitation methods",
                #delivery ~ "Delivery type",
                delivery_cat ~ "Delivery type"
                #delivery_location ~ "Location of delivery"#,
                #status ~ "Person conducting delivery",
                #risk_num_antibiotic_courses_28d ~ "# antibiotic courses 28d pre-episode",
                #antibiotic_1_antibiotic_name ~ "antibiotic name",
                
              ),
              digits = list(
                all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
                all_categorical() ~ 0 
              )) %>% 
  #add_p()%>% 
  add_overall() %>% 
  add_n() %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Episode outcome**") %>% 
  bold_labels() %>% 
  as_gt() 
