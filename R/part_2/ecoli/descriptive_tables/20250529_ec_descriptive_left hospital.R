
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
  flextable,     # converting tables to HTML
  dplyr
)

ec_supplementary_discharged <- ec_final %>% 
  mutate(left_hospital = case_when(
    left_hospital == "no" ~ "Never discharged",
    left_hospital == "yes" ~  "Discharged",
    TRUE ~ NA_character_)) %>%
  dplyr::select(death_30,
                sex,
                ethnicity_des2,
                imd_quintile,
                address,
                urban_rural,
                left_hospital,
                matage_grp,
                gestage_grp,
                birthwgt_grp,
                multibirth_bi,
                infant_age_cat,
                #age_description,
                #apportionment_rule_category,
                #sample_onset,
                delivery_cat,
                #hospital_site_code,
                #trust_provider_code,
                mother_epidur,
                #antenatal_stay,
                #cardiac_surgery_30d_prior,
                #bowel_gastric_surgery_30d_prior,
                #urinary_catheter_28d_prior,
                umbilical_central_catheter_28d_prior,
                ventilation_28d_prior,
                #intubation_28d_prior,
                #cardiac_surgery_90d_post,
                #bowel_gastric_surgery_90d_post,
                #urinary_catheter_90d_post,
                #umbilical_central_catheter_90d_post,
                #ventilation_90d_post,
                #intubation_90d_post,
                jaundice,
                hemato,
                #neoplasm_carcinom,
                endo_nutri_meta,
                accident_injury,
                congenital,
                perinatal_other,
                perinat_resp_cardio,
                pregnancy_complication_trauma,
                genitourinary,
                #musculo,
                skin,
                #ear,
                eye,
                digestive,
                circulatory,
                other_infectious,
                #neuro_nervo,
                temp_reg
  ) %>% 
  tbl_summary(by= left_hospital,
              missing = "no",
              percent = "column",
              label = list(
                sex ~ "Sex (infant)",
                #age_description ~ "Infant age group",
                matage_grp ~ "Maternal age group",
                ethnicity_des2 ~ "Ethnicity",
                infant_age_cat ~ "Infant age group",
                imd_quintile ~ "Multiple deprivation quintile",
                address ~ "Region of origin",
                #left_hospital ~ "Discharged?",
                urban_rural ~ "Population density",
                #year_sample ~ "Year",
                gestage_grp ~ "Gestational age group",
                birthwgt_grp ~ "Birth weight group",
                multibirth_bi ~ "Single or multiple birth",
                delivery_cat ~ "Delivery type",
                # delivery_location ~ "Delivery location",
                mother_epidur ~ "Maternal hospital stay length",
                #antenatal_stay ~ "Antenatal stay length",
                jaundice ~ "Jaundice",
                hemato ~ "Hematological & hemorrhagic",
                endo_nutri_meta ~ "Endocrine, nutritional & metabolic",
                accident_injury ~ "Accident & injury",
                congenital ~ "Congenital",
                perinatal_other ~ "Other perinatal disorders",
                perinat_resp_cardio ~ "Perinatal respiratory & cardiovascular",
                pregnancy_complication_trauma ~ "Pregnancy complication & birth trauma",
                genitourinary ~ "Genitourinary",
                skin ~ "Skin",
                eye ~ "Eye",
                digestive ~ "Digestive",
                circulatory ~ "Circulatory",
                other_infectious ~ "Other infectious",
                #neuro_nervo ~ "Neurological & nervous system",
                umbilical_central_catheter_28d_prior ~ "Umbilical/central catheter",
                ventilation_28d_prior ~ "Invasive ventilation",
                #umbilical_central_catheter_90d_post ~ "Umbilical/central catheter",
                # ventilation_90d_post ~ "Invasive ventilation",
                #apportionment_rule_category ~ "Hospital/community onset",
                #sample_onset ~ "EOS/LOS",
                temp_reg ~ "Temperature regulation",
                death_30 ~ "Died?"
              )
              # digits ~ list(
              #   all_continuous() ~ 2,        # Apply 2 decimal places to continuous variables
              #   all_categorical() ~ 0 
              # )
  ) %>%
  add_p()%>% 
  #add_overall() %>% 
  add_n() %>%
  # modify_spanning_header(c("stat_1", 
  #                          "stat_2") ~ "**Episode outcome**") %>% 
  bold_labels()
