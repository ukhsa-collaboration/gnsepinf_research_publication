## descriptive tables for E coli

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

#Make one table per group of caracteristics
##DEMOGRAPHICS: deprivation, ethnicity, sex, region, year
ec_demotbl <- ec_infants %>% 
  mutate(death_90 = ifelse(death_90 == 1,
                           "Death",
                           "Survive")) %>% 
  select(sex,
         ethnicity_des,
         ethnicity_des2,
         ethnicity_grp,
         imd04_grp,
         phe_region_reporting,
         phe_centre_reporting,
         year_sample,
         address,
         urban_rural,
         death_90) %>% 
  tbl_summary(by= death_90,
              missing = "no",
              percent = "column",
              label = list(
                sex ~ "Sex (infant)",
                ethnicity_des ~ "Ethnicity group NEW (mother)",
                ethnicity_des2 ~ "Ethnicity group NEW (2) (mother)",
                ethnicity_grp ~ "Ethnicity group ORIGINAL (mother)",
                imd04_grp ~ "Multiple deprivation quintile (mother)",
                phe_region_reporting ~ "PHE Region reporting",
                phe_centre_reporting ~ "PHE Centre reporting",
                address ~ "Region of origin",
                urban_rural ~ "Geographic area",
                year_sample ~ "Year"
              ),
              digits = list(
                all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
                all_categorical() ~ 0 
              )) %>% 
  add_p(
     #   test = list(
     #   phe_centre_reporting ~ "fisher.test"
     # ),
     # test.args = list(
     #   phe_centre_reporting ~ list(workspace = 2e9))
  )%>% 
  add_overall() %>% 
  add_n() %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Episode outcome**") %>% 
  bold_labels() %>% 
  as_gt() %>% 
  tab_style(
    style = list(
      cell_fill(color = "red"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = stat_1,
      rows = grepl("^([0-9]|10)\\b", stat_1)))

##descriptive for birth factors
ec_birth_tbl <- ec_infants %>% 
  mutate(death_90 = ifelse(death_90 == 1,
                           "Death",
                           "Survive")) %>% 
  select(matage_grp,
         gestage_grp,
         birthwgt_grp,
         multibirth_grp,
         multibirth_bi,
         age_description,
         death_90) %>% 
  tbl_summary(by= death_90,
              missing = "no",
              percent = "column",
              label = list(
                matage_grp ~ "Maternal age group",
                gestage_grp ~ "Gestational age group",
                birthwgt_grp ~ "Birth weight group",
                multibirth_grp ~ "Type of birth",
                multibirth_bi ~ "Single or multiple birth",
                age_description ~ "Infant age group"
              ),
              digits = list(
                all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
                all_categorical() ~ 0 
              )) %>% 
  add_p(
    test = list(
      gestage_grp ~ "chisq.test",
      matage_grp ~ "fisher.test",
      birthwgt_grp ~ "fisher.test",
      multibirth_grp ~ "fisher.test",
      age_description ~ "fisher.test"
    ),
    test.args = list(
      matage_grp ~ list(workspace = 2e9),
      birthwgt_grp ~ list(workspace = 2e9),
      multibirth_grp ~ list(workspace = 2e9),
      age_description ~ list(workspace = 2e9))
  ) %>% 
  add_overall() %>% 
  add_n() %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Episode outcome**") %>% 
  bold_labels() %>% 
  as_gt() %>% 
  tab_style(
    style = list(
      cell_fill(color = "red"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = stat_1,
      rows = grepl("^([0-9]|10)\\b", stat_1)))
ec_birth_tbl

##risk factors that have to do with medical procedures first slot of the dataset

ec_risk_tbl1 <- ec_infants %>% 
  mutate(death_90 = ifelse(death_90 == 1,
                           "Death",
                           "Survive")) %>% 
  select(risk_noutrophil_count500,
         risk_diabetic_foot_28d,
         risk_hepatobiliary_28d,
         risk_intubated_extubated_28d,
         risk_open_wound_ulcer_28d,
         risk_surgery_prosthetic_30d_12m,
         risk_urinary_catheter_28d,
         risk_urinary_catheter_manipulated_28d,
         risk_uti_treatment_28d,
         risk_vascular_device_28d,
         risk_prostate_biopsy_28d,
         risk_surgical_other,
         risk_cancer_chemotherapy_28d,
         risk_num_antibiotic_courses_28d,
         risk_know_primary_focus,
         risk_likely_primary_focus,
         risk_predisposing_factors,
         risk_hcai_likely,
         risk_infection_acquired_location,
         risk_discharged_28d,
         death_90) %>% 
  tbl_summary(by= death_90,
              missing = "ifany",
              missing_text = "(Missing)",
              percent = "row",
              label = list(
                risk_noutrophil_count500 ~ "Neutrophil count >=500",
                risk_diabetic_foot_28d ~ "Diabetic foot ulcer/infection",
                risk_hepatobiliary_28d ~ "Hepatobiliary procedure ERCP/MRCP & sim",
                risk_intubated_extubated_28d ~ "Intubated/extubated",
                risk_open_wound_ulcer_28d ~ "Open wound/ulcer (non-diabetic foot)",
                risk_surgery_prosthetic_30d_12m ~ "Surgery/prosthetic material 30d-12month prios",
                risk_urinary_catheter_28d ~ "Urinary catheter: intermittent/temporary",
                risk_urinary_catheter_manipulated_28d ~ "Urinary catheter manipulated",
                risk_uti_treatment_28d ~ "UTI treatment",
                risk_vascular_device_28d ~ "Vascular device (PPM, ICD, CVC) inserted/removed/manipulated",
                risk_prostate_biopsy_28d ~ "Prostate biopsy",
                risk_surgical_other ~ "Surgical methods (all apply)",
                risk_cancer_chemotherapy_28d ~ "Anti-cancer chemotherapy",
                risk_num_antibiotic_courses_28d ~ "Antibiotics prescribed",
                risk_know_primary_focus ~ "Primary focus known?",
                risk_likely_primary_focus ~ "Most likely primary focus",
                risk_predisposing_factors ~ "Episode predisposing factors",
                risk_hcai_likely ~ "HCAI likely?",
                risk_infection_acquired_location ~ "Likely location of acquisition",
                risk_discharged_28d ~ "Patient discharged?"
              ),
              digits = list(
                all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
                all_categorical() ~ 0 
              )
  )%>% 
  add_p(
    # test = list(
    # risk_likely_primary_focus ~ "fisher.test"), # Specify Fisher's test for this variable
    #     test.args = list(
    #       risk_likely_primary_focus ~ list(workspace = 10e7)) # Increase workspace size
  ) %>% 
  add_overall() %>% 
  add_n() %>% 
  modify_header(label ~ "**Risk factors (28 days prior to episode)**") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Episode outcome**") %>%
  bold_labels() %>% 
  as_gt() %>% 
  tab_style(
    style = list(
      cell_fill(color = "red"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = stat_1,
      rows = grepl("^([0-9]|10)\\b", stat_1)))

#####
#####risk factors of the second slot of the dataset

ec_risk_tbl2 <- 
  ec_infants %>% 
  mutate(death_90 = ifelse(death_90 == 1,
                           "Death",
                           "Survive")) %>% 
  select(risk_factors_urinary_catherisation,
         risk_factors_vascular_device,
         risk_factors_other_invasive_indwelling_device,
         risk_factors_surgical_or_other_invasive_procedure,
         risk_factors_neutropaenia,
         risk_factors_wound_ulcer,
         apportionment_rule_category,
         left_hospital,
         left_hospital_period,
         death_90) %>% 
  tbl_summary(by= death_90,
              missing = "no",
              missing_text = "(Missing)",
              percent = "row",
              label = list(
                risk_factors_urinary_catherisation ~ "Urinary catherisation",
                risk_factors_vascular_device ~ "Vascular device",
                risk_factors_other_invasive_indwelling_device ~ "Other invasive indwelling device",
                risk_factors_surgical_or_other_invasive_procedure ~ "Surgical or other invasive procedure",
                risk_factors_neutropaenia ~  "Neutropenia",
                risk_factors_wound_ulcer ~ "Wounded ulcer",
                apportionment_rule_category ~ "Onset (apportionment)",
                left_hospital ~ "left hospital since DOB",
                left_hospital_period ~ "lenght dob-admission"
              ),
              digits = list(
                all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
                all_categorical() ~ 0       # Apply no decimals to categorical counts
              )) %>% 
  add_p() %>% 
  add_overall() %>% 
  add_n() %>% 
  modify_header(label ~ "**Risk factors 2**") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Episode outcome**") %>% 
  bold_labels() %>% 
  as_gt() %>% 
  tab_style(
    style = list(
      cell_fill(color = "red"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = stat_1,
      rows = grepl("^([0-9]|10)\\b", stat_1)))


## generate one table for the variables that are of interest for our analysis
ec_varinterest_tbl <- ec_infants %>% 
  mutate(death_90 = ifelse(death_90 == 1,
                           "Death",
                           "Survive")) %>% 
  select(sex,
         ethnicity_des,
         imd_quintile,
         address,
         urban_rural,
         matage_grp,
         gestage_grp,
         birthwgt_grp,
         #multibirth_grp,
         multibirth_bi,
         age_description,
         apportionment_rule_category,
         number_episodes,
         left_hospital,
         left_hospital_period,
         admi_onset,
         sample_onset,
         mother_epidur,
         antenatal_stay,
         resus,
         #delivery,
         delivery_cat,
         delivery_location,
         status,
         risk_num_antibiotic_courses_28d,
         antibiotic_1_antibiotic_name,
         death_90) %>% 
  tbl_summary(by= death_90,
              missing = "no",
              percent = "row",
              label = list(
                sex ~ "Sex (infant)",
                ethnicity_des ~ "Ethnicity group NEW (mother)",
                imd_quintile ~ "Multiple deprivation quintile (mother)",
                address ~ "Region of origin",
                urban_rural ~ "Geographic area",
                matage_grp ~ "Maternal age group",
                gestage_grp ~ "Gestational age group",
                birthwgt_grp ~ "Birth weight group",
                #multibirth_grp ~ "Type of birth",
                multibirth_bi ~ "Single or multiple birth",
                age_description ~ "Infant age group",
                apportionment_rule_category ~ "Onset (apportionment)",
                number_episodes ~ "Number of episodes",
                left_hospital ~ "left hospital since DOB",
                left_hospital_period ~ "lenght dob-admission",
                admi_onset ~ "onset - admission",
                sample_onset ~ "onset - sample",
                mother_epidur ~ "Mother hospital stay lenght",
                antenatal_stay ~ "Antenatal stay lenght",
                resus ~ "Resucitation methods",
                #delivery ~ "Delivery type",
                delivery_cat ~ "Delivery type",
                delivery_location ~ "Location of delivery",
                status ~ "Person conducting delivery",
                risk_num_antibiotic_courses_28d ~ "# antibiotic courses 28d pre-episode",
                antibiotic_1_antibiotic_name ~ "antibiotic name"
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
  tab_style(
    style = list(
      cell_fill(color = "red"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = stat_1,
      rows = grepl("^([0-9]|10)\\b", stat_1)))


###UPDATED DESCRIPTIVE TABLE 4/3/2025

ec_descriptive_final <- 
  ec_infants %>% 
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
         #multibirth_grp,
         multibirth_bi,
         age_description,
         apportionment_rule_category,
         number_episodes,
         left_hospital,
         left_hospital_period,
         #admi_onset,
         sample_onset,
         mother_epidur,
         antenatal_stay,
         #resus,
         #delivery,
         delivery_cat,
         delivery_location,
         #status,
         #risk_num_antibiotic_courses_28d,
         #antibiotic_1_antibiotic_name,
         death_90) %>% 
  tbl_summary(by= death_90,
              missing = "no",
              percent = "row",
              label = list(
                sex ~ "Sex (infant)",
                ethnicity_des2 ~ "Ethnicity group (mother)",
                imd_quintile ~ "Multiple deprivation quintile (mother)",
                address ~ "Region of origin",
                urban_rural ~ "Geographic area",
                matage_grp ~ "Maternal age group",
                gestage_grp ~ "Gestational age group",
                birthwgt_grp ~ "Birth weight group",
                #multibirth_grp ~ "Type of birth",
                multibirth_bi ~ "Single or multiple birth",
                age_description ~ "Infant age group",
                apportionment_rule_category ~ "Onset (apportionment)",
                number_episodes ~ "Number of episodes",
                left_hospital ~ "left hospital since DOB",
                left_hospital_period ~ "lenght dob-admission",
                #admi_onset ~ "onset - admission",
                sample_onset ~ "Episode onset",
                mother_epidur ~ "Mother hospital stay lenght",
                antenatal_stay ~ "Antenatal stay lenght",
                #resus ~ "Resucitation methods",
                #delivery ~ "Delivery type",
                delivery_cat ~ "Delivery type",
                delivery_location ~ "Location of delivery"#,
                #status ~ "Person conducting delivery",
                #risk_num_antibiotic_courses_28d ~ "# antibiotic courses 28d pre-episode",
                #antibiotic_1_antibiotic_name ~ "antibiotic name"
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
  tab_style(
    style = list(
      cell_fill(color = "red"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = stat_1,
      rows = grepl("^([0-9]|10)\\b", stat_1)))
