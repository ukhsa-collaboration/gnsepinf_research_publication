###univariate for klebsiella

pacman::p_load(
  rio,          # File import
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics, 
  stringr,      # manipulate text strings 
  purrr,        # loop over objects in a tidy way
  gtsummary,    # summary statistics and tests 
  broom,        # tidy up results from regressions
  lmtest,       # likelihood-ratio tests
  parameters,   # alternative to tidy up results from regressions
  see,          # alternative to visualise forest plots
  labelled # for setting value labels
)
##define and select the variables of interest
source("paths/import_datasets.R")


kleb_explanvars <- c("sex",
                    #"ethnicity_des2",
                    "ethnicity_di",
                    "imd_quintile",
                    "address",
                    "urban_rural",
                    "matage_grp",
                    "gestage_grp",
                    "birthwgt_grp",
                    "multibirth_bi",
                    "infant_age_grp",
                    #"age_description",
                    "apportionment_rule_category",
                    "sample_onset",
                    "delivery_cat",
                    #"hospital_site_code",
                    #"trust_provider_code",
                    "mother_epidur",
                    "antenatal_stay",
                    #"cardiac_surgery_30d_prior",
                    #"bowel_gastric_surgery_30d_prior",
                    #"urinary_catheter_28d_prior",
                    "umbilical_central_catheter_28d_prior",
                    "ventilation_28d_prior",
                    #"intubation_28d_prior",
                    #"cardiac_surgery_90d_post",
                    #"bowel_gastric_surgery_90d_post",
                    #"urinary_catheter_90d_post",
                    "umbilical_central_catheter_90d_post",
                    "ventilation_90d_post",
                    #"intubation_90d_post",
                    "jaundice",
                    "hemato",
                    #"neoplasm_carcinom",
                    "endo_nutri_meta",
                    "accident_injury",
                    "congenital",
                    "perinatal_other",
                    "perinat_resp_cardio",
                    #"pregnancy_complication_trauma",
                    "genitourinary",
                    #"musculo",
                    "skin",
                    #"ear",
                    #"eye",
                    "digestive",
                    "circulatory",
                    "other_infectious"
                    #"neuro_nervo"
)


##convert dichotomous variables to 0/1

kleb_univariate <- kleb_final %>% 
  mutate(across(
    .cols = all_of(c("sex",
                     "urban_rural",
                     "apportionment_rule_category",
                     "sample_onset"#,
                     #"cardiac_surgery_30d_prior",
                     #"bowel_gastric_surgery_30d_prior",
                     # "urinary_catheter_28d_prior",
                     # "umbilical_central_catheter_28d_prior",
                     # "ventilation_28d_prior",
                     # "intubation_28d_prior",
                     # "jaundice",
                     # "hemato",
                     # "endo_nutri_meta",
                     # "accident_injury",
                     # "congenital",
                     # "perinatal_other",
                     # "perinat_resp_cardio",
                     # "pregnancy_complication_trauma",
                     # "genitourinary",
                     # "skin",
                     # "eye",
                     # "digestive",
                     # "circulatory",
                     # "other_infectious",
                     # "neuro_nervo"
    )
    ),
    .fns = ~case_when(
      . %in% c("Male", "urban", "Hospital-onset", "early onset" ) ~ 1,
      . %in% c("Female", "rural", "Community-onset", "late onset") ~ 0,
      TRUE ~ NA_real_
    )
  )) %>% 
  mutate(
    across(
      c(sex,
        death_90,
        age_description,
        ethnicity_des2,
        ethnicity_di,
        imd_quintile,
        address,
        matage_grp,
        gestage_grp,
        birthwgt_grp,
        multibirth_bi,
        urban_rural,
        apportionment_rule_category,
        sample_onset,
        delivery_cat,
        hospital_site_code,
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
        neuro_nervo
      ),
      ~ as.factor(.)
    )
  )


## Now we drop rows with missing information for variables of interest

kleb_univariate <- kleb_univariate %>% 
  drop_na(any_of(c("death_90",
                   kleb_explanvars
  )
  )
  )
##2226 rows


tabyl(kleb_univariate$death_90)

##I have 14 deaths and 104 events


##after the meeting and dropping some variables i have 2291 observations with 237 deaths


##LOGISTIC REGRESSION


## relevel the factor variables

kleb_univariate <- kleb_univariate %>% 
  mutate(
    ethnicity_di = case_when(
      ethnicity_des2 %in% c("White") ~ "White",
      ethnicity_des2 %in% c("Black",
                            "South Asian",
                            "Asian other",
                            "Mixed",
                            "Other ethnicity") ~ "non-white")
  ) %>% 
  mutate(
    ethnicity_di = as.factor(ethnicity_di)
  ) %>% 
  mutate(
    ethnicity_di = fct_relevel(
      ethnicity_di,
      "White",
      after=0
    ) 
  ) %>% 
  mutate(
    ethnicity_des2 = fct_relevel(
      ethnicity_des2,
      "White",
      after = 0) %>% 
      factor(levels = c(
        "White",
        "Black",
        "South Asian",
        "Asian other",
        "Mixed",
        "Other ethnicity"
      )
      )
  ) %>%
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
    matage_grp = fct_relevel(
      matage_grp,
      "20-29 years",
      after = 0
    )
  ) %>% 
  mutate(
    age_description = fct_relevel(
      age_description,
      "0 to 28 days",
      after = 0
    ) %>% 
      factor(levels = c(
        "0 to 28 days",
        "29 to 90 days",
        "91 to 181 days",
        "182 to 272 days",
        "273 to 364 days"
      ))
  ) %>% 
  mutate(
    infant_age_grp = case_when(
      age_description %in% c("0 to 28 days") ~ "0 to 28 days",
      age_description %in% c("29 to 90 days") ~ "29 to 90 days",
      age_description %in% c("91 to 181 days",
                             "182 to 272 days",
                             "273 to 364 days") ~ "91 to 364 days"
    )
  ) %>% 
  mutate(
    infant_age_grp = as.factor(infant_age_grp)
  ) %>% 
  mutate(
    infant_age_grp = fct_relevel(
      infant_age_grp,
      "0 to 28 days",
      after = 0
    ) %>% 
      factor(levels=c(
        "0 to 28 days",
        "29 to 90 days",
        "91 to 364 days")
      )
  )%>% 
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
  ) %>% 
  mutate(
    mother_epidur = fct_relevel(
      mother_epidur,
      "<48h",
      after = 0
    ) %>% 
      factor(levels = c(
        "<48h",
        "2days to 1 week",
        "+1 week"
      )
      )
  ) %>%  
  mutate(
    address = fct_relevel(
      address,
      "London",
      after = 0
    ) %>% 
      factor(levels = c(
        "London",
        "East Midlands",
        "East of England",
        "North East",
        "North West",
        "South East",
        "South West",
        "West Midlands",
        "Yorkshire and the Humber",
        "Wales"
      )
      )
  )


# ec_univariate3_presentation <- ec_univariate3 %>%
#   mutate(
#     sex = recode(sex, `0` = "Female", `1` = "Male"),
#     apportionment_rule_category = recode(apportionment_rule_category, `0` = "Hospital", `1` = "Community"),
#     urban_rural = recode(urban_rural, `0` = "urban", `1` = "rural"),
#     sample_onset = recode(sample_onset, `0` = "EOS", `1` = "LOS")
#   )

kleb_univariate_presentation <- kleb_univariate %>%
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
    apportionment_rule_category = factor(apportionment_rule_category, levels = c(0, 1), labels = c("Hospital", "Community")),
    urban_rural = factor(urban_rural, levels = c(0, 1), labels = c("urban", "rural")),
    sample_onset = factor(sample_onset, levels = c(0, 1), labels = c("EOS", "LOS"))
  )

kleb_univ_tab <- kleb_univariate_presentation %>% 
  #ec_univariate3 %>% 
  dplyr::select(kleb_explanvars, death_90) %>% ## select variables of interest
  
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = death_90,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    exponentiate = TRUE,                     ## exponentiate to produce odds ratios (rather than log odds)
    test = "LTR",   ##likelihood test instead of wald
    label = list(
      sex = "Sex (infant)",
      #age_description = "Infant age group",
      matage_grp = "Maternal age group",
      ethnicity_des2 = "Ethnicity group (mother)",
      infant_age_grp = "Infant age group",
      imd_quintile = "Multiple deprivation quintile (mother)",
      address = "Region of origin",
      urban_rural = "Population density",
      year_sample = "Year",
      gestage_grp = "Gestational age group",
      birthwgt_grp = "Birth weight group",
      multibirth_bi = "Single or multiple birth",
      delivery_cat = "Delivery type",
      delivery_location = "Delivery location",
      mother_epidur = "Maternal hospital stay length",
      antenatal_stay = "Antenatal stay length",
      jaundice = "Jaundice",
      hemato = "Hematological & hemorrhagic",
      endo_nutri_meta = "Endocrine, nutritional & metabolic",
      accident_injury = "Accident & injury",
      congenital = "Congenital",
      perinatal_other = "Perinatal other disorders",
      perinat_resp_cardio = "Perinatal respiratory & cardiovascular",
      pregnancy_complication_trauma = "Pregnancy complication & birth trauma",
      genitourinary = "Genitourinary",
      skin = "Skin",
      eye = "Eye",
      digestive = "Digestive",
      circulatory = "Circulatory",
      other_infectious = "Other infectious",
      neuro_nervo = "Neurological & nervous system",
      umbilical_central_catheter_28d_prior = "Pre-Umbilical/central catheter",
      ventilation_28d_prior = "Pre-Invasive ventilation",
      umbilical_central_catheter_90d_post = "Umbilical/central catheter",
      ventilation_90d_post = "Invasive ventilation",
      apportionment_rule_category = "Hospital/community onset",
      sample_onset = "EOS/LOS"
    ) 
  )

kleb_univar_tbl1 <- kleb_univ_tab %>% 
  modify_caption("**Univariate Logistic regression Model Results (Klebsiella)**")
