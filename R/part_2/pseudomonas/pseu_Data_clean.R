###Data cleaning of the pseudomonas dataset

pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  data.table,
  skimr
)

source("paths/import_datasets.R")


pseud_clean <- pseud %>%
  mutate(imd04_grp=case_when(imd04rk<=6568 ~ "1st",
                             imd04rk<=13137 & imd04rk>=6569 ~"2nd",
                             imd04rk<=19706 & imd04rk>=13138 ~"3rd",
                             imd04rk<=26275 & imd04rk>=19707 ~"4th",
                             imd04rk<= 32844 & imd04rk>=26276 ~"5th",
                             is.na(imd04rk) ~ "Unknown")
  ) %>% 
  mutate(multibirth_grp = case_when(multtype_br %in% c(0,1,2,3,4,5,6,7,8,9) ~ "Twin",
                                    multtype_br>9 ~"Triplet +",
                                    is.na(multtype_br) ~"Single")
  ) %>% 
  mutate(multibirth_bi = case_when(
    multibirth_grp == "Twin" ~ "Twin +",
    multibirth_grp == "Triplet +" ~ "Twin +",
    multibirth_grp == "Single" ~ "Single")
  ) %>% 
  mutate(birth_weight_equal=case_when(birthwgt_br==birweit_1 | birthwgt_br==birweit_2 | birthwgt_br==birweit_3~1,
                                      TRUE ~ 0)
  ) %>% 
  mutate(birth_weight_equal=as.integer(birth_weight_equal)
  ) %>% 
  mutate(birthwgt_combined=case_when(birth_weight_equal=="1" ~1,
                                     birth_weight_equal=="0" & multbth_br== "1" & birthwgt_br!="0" ~ 1,
                                     birth_weight_equal=="0" & is.na(multbth_br) & birthwgt_br!="0" ~ 1,
                                     birth_weight_equal=="0" & is.na(multbth_br) & birthwgt_br=="0" ~ 2,
                                     birth_weight_equal=="0" & is.na(multbth_br) & is.na(birthwgt_br) ~ 2,
                                     birth_weight_equal=="0" & multbth_br=="1" ~ 0)
  ) %>% 
  mutate(birthwgt_fin=case_when(birthwgt_combined=="1" ~ birthwgt_br,
                                birthwgt_combined=="2" ~ birweit_1)
  ) %>% 
  mutate(birthwgt_grp=case_when(birthwgt_fin < 1000 ~ "Extremely Low",
                                birthwgt_fin >=1000 & birthwgt_fin <1500 ~"Very Low",
                                birthwgt_fin >=1500 & birthwgt_fin <2500 ~"Low",
                                birthwgt_fin >=2500 ~ "Normal",
                                TRUE ~ "Unknown")
  ) %>% 
  mutate_at(vars(mother_dob_br), ~ymd(.)
  ) %>% 
  mutate_at(vars(dob_br), ~ymd(.)
  ) %>% 
  mutate(matage=floor(decimal_date(dob_br) - decimal_date(mother_dob_br))
  ) %>% 
  mutate(matage_grp=case_when(matage<20 ~ "<20 years",
                              matage>=20 & matage<30 ~"20-29 years",
                              matage>=30 & matage<40 ~"30-39 years",
                              matage>=40 ~"40+ years")
  ) %>% 
  mutate(ethnos=as.character(ethnos)
  ) %>% 
  mutate(ethnicity_grp=case_when(ethnos %in% c("A ") ~ " White British",
                                 ethnos %in% c("B ","C ") ~"White Other",
                                 ethnos %in% c("N ") ~"Black African",
                                 ethnos %in% c("M ") ~"Black Caribbean",
                                 ethnos %in% c("H ","J ","K ","L ","R ") ~"Asian",
                                 ethnos %in% c("D ","E ","F ","G ","P ","S ") ~"Other",
                                 ethnos %in% c("Z ", "X ", "99", "9 ")  ~"Not states")
  ) %>% 
  mutate(ethnicity = case_when(
    ethnos %in% c("A ") ~ "White British",
    ethnos %in% c("B ", "C ") ~ "White other",
    ethnos %in% c("N ") ~ "Black African",
    ethnos %in% c("M ") ~ "Black Caribbean",
    ethnos %in% c ("J ") ~ "Pakistani",
    ethnos %in% c("K ") ~ "Bangladeshi",
    ethnos %in% c("H ") ~ "Indian",
    ethnos %in% c("R ") ~ "Chinese",
    ethnos %in% c("L ") ~ "Asian other",
    ethnos %in% c("D ", "E ", "F ", "G ") ~ "mixed",
    ethnos %in% c("P ") ~ "Black British",
    ethnos %in% c("S ") ~ "Other ethnicity",
    ethnos %in% c("Z ", "X ", "99", "9 ")  ~ "Not states")
  ) %>% 
  mutate(ethnicity_des = case_when(
    ethnos %in% c("A ") ~ "White British",
    ethnos %in% c("B ", "C ") ~ "White other",
    ethnos %in% c("N ") ~ "Black African",
    ethnos %in% c ("J ") ~ "Pakistani",
    ethnos %in% c("H ") ~ "Indian",
    ethnos %in% c("K ") ~ "Bangladeshi",
    ethnos %in% c("L ", "R ") ~ "Asian other",
    ethnos %in% c("D ", "E ", "F ", "G ") ~ "Mixed",
    ethnos %in% c("P ", "M ") ~ "Black other",
    ethnos %in% c("S ") ~ "Other ethnicity",
    ethnos %in% c("Z ", "X ", "99", "9 ")  ~ "Not states")
  ) %>%
  mutate(ethnicity_des2 = case_when(
    ethnos %in% c("A ", "B ", "C ") ~ "White",
    ethnos %in% c("N ", "P ", "M ") ~ "Black",
    ethnos %in% c ("J ", "H ", "K ") ~ "South Asian",
    ethnos %in% c("L ", "R ") ~ "Asian other",
    ethnos %in% c("D ", "E ", "F ", "G ") ~ "Mixed",
    ethnos %in% c("S ") ~ "Other ethnicity",
    ethnos %in% c("Z ", "X ", "99", "9 ")  ~ "Not states")
  ) %>%
  mutate(
    ethnicity_di = case_when(
      ethnicity_des %in% c("White") ~ "White",
      ethnicity_des %in% c("Black", "South Asian", "Asian other", "Mixed", "Other ethnicity") ~ "Non-white",
      TRUE ~ NA_character_
    )
  ) %>% 
  rename(risk_noutrophil_count500 = risk_factors_absolute_neutrophil_count_less_than_500_0_5_at_time,
         risk_diabetic_foot_28d = risk_factors_diabetic_foot_ulcer_or_infection_28_days_prior,
         risk_hepatobiliary_28d =  risk_factors_hepatobiliary_procedure_ercp_or_mrcp_or_similar_28_days_prior,
         risk_intubated_extubated_28d = risk_factors_intubated_e_tor_pt_or_extubated_28_days_prior,
         risk_open_wound_ulcer_28d = risk_factors_open_wounds_or_ulcer_excluding_diabetic_foot_infection_28_days_prior,
         risk_surgery_prosthetic_30d_12m = risk_factors_surgery_30_days_or_12_months_prosthetic_material_prior,
         risk_urinary_catheter_28d = risk_factors_urinary_catheter_including_intermittent_or_temporary_in_last_28_days_prior,
         risk_urinary_catheter_manipulated_28d = risk_factors_urinary_catheter_including_intermittent_or_temporary_manipulated_28_days_prior,
         risk_uti_treatment_28d = risk_factors_uti_treatment_28_days_prior_to_onset,
         risk_vascular_device_28d = risk_factors_vascular_device_ppm_or_icd_or_cvc_inserted_removed_manipulated_28_days_prior,
         risk_prostate_biopsy_28d = risk_factors_prostate_biopsy_in_28_days_prior,
         risk_surgical_other = risk_factors_surgical_methods_used_tick_all_that_apply,
         risk_cancer_chemotherapy_28d = risk_factors_has_the_patient_been_on_anti_cancer_chemotherapy_in_28_days_prior_to_specimen_date,
         risk_num_antibiotic_courses_28d = antibiotic_history_number_of_antibiotic_courses_prescribed_28_days_prior#,
         #risk_know_primary_focus = risk_factors_do_you_know_of_a_primary_focus_of_the_bacteraemia,
         #risk_likely_primary_focus = risk_factors_most_likely_primary_focus,
         #risk_other_most_likely_focus = risk_factors_other_most_likely_primary_focus,
         #risk_predisposing_factors = risk_factors_factors_directly_predisposing_to_this_episode,
         #risk_hcai_likely =  risk_factors_is_this_episode_likely_to_to_be_an_hcai,
         #risk_infection_acquired_location = risk_factors_where_was_the_infection_likely_to_have_been_acquired,
         #risk_discharged_28d = healthcare_interaction_has_the_patient_been_discharged_from_an_elective_or_emergency_hospital_admission_in_the_reporting_trust_in_the_last_28_days,
  ) %>% 
  mutate(gestage_grp=case_when(gestat_1 %in% c(22,23,24,25,26,27) ~ "Extremely Preterm",
                               gestat_1 %in% c(28,29,30,31) ~"Very Preterm",
                               gestat_1 %in% c(32,33,34,35,36) ~"Moderate Preterm",
                               gestat_1>=37 & gestat_1<50 ~"Term",
                               gestat_1==99 ~"NA",
                               TRUE ~ "NA")
  ) %>%  
  mutate(
    year_sample = lubridate::year(as.Date(data_collection_date))
  ) %>% 
  distinct() %>% 
  mutate(
    data_collection_date = as.Date(data_collection_date)
  ) %>% 
  arrange(data_collection_date
  ) %>% 
  group_by(nhs
  ) %>% 
  mutate(
    time_diff = as.numeric(data_collection_date - lag(data_collection_date,
                                                      default = first(data_collection_date))),
    new_episode = ifelse(is.na(time_diff) | time_diff >=14, 1, 0),
    episode_group = cumsum(new_episode)
  ) %>% 
  ungroup(
    
  ) %>% 
  ##number of episodes per infant
  mutate(
    number_episodes = case_when(
      episode_group == 0 ~ 1,
      episode_group == 1 ~ 2,
      episode_group == 2 ~ 3,
      episode_group == 3 ~ 4,
      episode_group == 4 ~ 5
    )
  ) %>% 
  mutate(
    date_admitted = dmy(date_admitted),
    dob = as.Date(dob)
  ) %>% 
  mutate(
    diff_admitted_collection= difftime(
      date_admitted,
      data_collection_date,
      units = "days"
    )
  ) %>% 
  mutate(
    diff_dob_admitted = difftime(
      date_admitted,
      dob,
      units = "days"
    )
  ) %>% 
  mutate(
    diff_dob_collection = difftime(
      data_collection_date,
      dob,
      units = "days"
    )
  ) %>% 
  mutate(
    left_hospital = case_when(
      diff_dob_admitted == 0 ~ "no",
      diff_dob_admitted > 0 ~ "yes"
    ),
    left_hospital_period = case_when(
      diff_dob_admitted == 0 ~ "never",
      diff_dob_admitted >0 & diff_dob_admitted <= 7 ~ "1 week",
      diff_dob_admitted >7 & diff_dob_admitted <= 31 ~ "1 month",
      diff_dob_admitted >31 ~ "+ 1 month"
    )
  ) %>% 
  mutate(
    admi_onset = case_when(
      diff_dob_admitted <=3  ~ "early onset",
      diff_dob_admitted >=4  ~ "late onset"
    )
  ) %>% 
  mutate(
    sample_onset = case_when(
      diff_dob_collection  <=3  ~ "early onset",
      diff_dob_collection >=4  ~ "late onset"
    )
  ) %>% 
  mutate(
    mother_epidur = case_when(
      epidur <= 2 ~ "<48h",
      epidur >2 & epidur <=7 ~ "2days to 1 week",
      epidur >7 ~ "+1 week"
    )
  ) %>% 
  mutate(
    antenatal_stay = case_when(
      antedur ==0 ~ "0 days",
      antedur >=1 & antedur <=2 ~ "1-2 days",
      antedur >2 ~ "3 + days"
    )
  ) %>% 
  mutate(
    resus = case_when(
      biresus_1 == 1 ~ "None",
      biresus_1 == 2 ~ "Drugs administered",
      biresus_1 == 3 ~ "Positive pressure mask",
      biresus_1 == 4 ~ "Positive pressure mask + drugs",
      biresus_1 == 5 ~ "Pos pressure endotracheal tube",
      biresus_1 == 6 ~ "Endotracheal tube + drugs",
      biresus_1 == 8 ~ "still born",
      biresus_1 == 9 ~ NA_character_
    )
  ) %>% 
  mutate(
    delivery = case_when(
      delmeth_1 == "0" ~ "Spontaneous vaginal",
      delmeth_1 == "1" ~ "Vaginal with abnormal cephalia",
      delmeth_1 == "2" ~ "Low forceps, not breech",
      delmeth_1 == "3" ~ "Other forceps, not breech - manipulation",
      delmeth_1 == "4" ~ "Ventouse",
      delmeth_1 == "5" ~ "Partial breech",
      delmeth_1 == "6" ~ "Breech",
      delmeth_1 == "7" ~ "Elective C-section",
      delmeth_1 == "8" ~ "Emergency C-section",
      delmeth_1 == "9" ~ "other",
      delmeth_1 == "X" ~ NA_character_
    )
  ) %>% 
  mutate(
    delivery_cat = case_when(
      delmeth_1 %in% c(0,1) ~ "vaginal",
      delmeth_1 %in% c(2,3,4,5,6, 9) ~ "manipulation/breech/other",
      delmeth_1 %in% c(7,8) ~ "c-section",
      delmeth_1 == "X" ~ NA_character_
    )
  ) %>% 
  mutate(
    delivery_location = case_when(
      delplac_1 %in% c(0,2,3,4,7,5,6,8) ~ "Hospital",
      delplac_1 %in% c(1) ~ "home delivery",
      #delplac_1 %in% c(7) ~ "NHS hosp - no delivery unit",
      #delplac_1 %in% c(5, 6, 8) ~ "other hospital",
      delplac_1 %in% c(9) ~ NA_character_
    )
  ) %>% 
  mutate(
    status = case_when(
      delstat_1 == 1 ~ "Hospital doctor",
      delstat_1 == 2 ~ "GP",
      delstat_1 ==3 ~ "Midwife",
      delstat_1 == 8 ~ "other",
      delstat_1 == 9 ~ NA_character_
    )
  ) %>% 
  mutate(
    homeadd = gsub("\\s+", "", homeadd)
  ) %>% 
  mutate(
    age_description = replace(
      age_description,
      age_description == "1 years",
      "0 to 28 days")
  ) %>% 
  mutate(
    ethnicity_des = case_when(
      ethnicity_des == "Not states" ~ NA_character_,
      TRUE ~ ethnicity_des
    )
  ) %>% 
  mutate(
    ethnicity_des2 = case_when(
      ethnicity_des2 == "Not states" ~ NA_character_,
      TRUE ~ ethnicity_des2
    )
  ) %>% 
  mutate(
    ethnicity = case_when(
      ethnicity == "Not states" ~ NA_character_,
      TRUE ~ ethnicity
    )
  ) %>% 
  mutate(
    imd04_grp = case_when(
      imd04_grp == "Unknown" ~ NA_character_,
      TRUE ~ imd04_grp
    )
  ) %>% 
  mutate(
    birthwgt_grp = case_when(
      birthwgt_grp == "Unknown" ~ NA_character_,
      TRUE ~ birthwgt_grp
    )
  ) %>% 
  mutate(
    gestage_grp = case_when(
      gestage_grp == "NA" ~ NA_character_,
      TRUE ~ gestage_grp
    )
  ) %>% 
  mutate(
    infant_age_grp = case_when(
      age_description %in% c("0 to 28 days") ~ "0 to 28 days",
      age_description %in% c("29 to 90 days") ~ "29 to 90 days",
      age_description %in% c("91 to 181 days",
                             "182 to 272 days",
                             "273 to 364 days") ~ "91 to 364 days"
    )
  )%>% 
  mutate(infage_days = difftime(
    data_collection_date,
    dob_br,
    unit="days"
  )
  ) %>%  # age cat is based on the age in days of the infant when the sample was taken
  mutate(infant_age_cat = case_when(
    infage_days <= as.difftime(3, units = "days") ~ "0-3 days",
    infage_days > as.difftime(3, units = "days") & infage_days <= as.difftime(28, units = "days") ~ "4-28 days",
    infage_days > as.difftime(28, units = "days") & infage_days <= as.difftime(90, units = "days") ~ "29-90 days",
    infage_days > as.difftime(90, units = "days") & infage_days <= as.difftime(365, units = "days") ~ "91-365 days",
    TRUE ~ NA_character_
  )
  # here i found that for the age_Description that the previous person used they probably
  #used dob instead of dob_br to code for the age_description variable which is diff for 2 values in my dataset
  # we trust dob_br more than dob so i am dropping them
  ) 
