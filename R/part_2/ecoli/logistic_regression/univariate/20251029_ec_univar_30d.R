### new univariate model 
##run on 29th october 2025


###Final univariate model building

source("paths/import_datasets.R")

##Univariate runs after corrections
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


ec_explanvars_oct <- c("sex",
                         "ethnicity_des2",
                         "imd_quintile",
                         #"phe_centre_reporting",
                         "address",
                         "urban_rural",
                         "matage_grp",
                         "gestage_grp",
                         "birthwgt_grp",
                         "multibirth_bi",
                         "infant_age_cat",
                         "delivery_cat",
                         "left_hospital",
                         "umbilical_central_catheter_28d_prior",
                         "ventilation_28d_prior",
                         "jaundice",
                         "hemato",
                         "endo_nutri_meta",
                         "accident_injury",
                         "congenital",
                         #"perinatal_other",
                         "perinatal_merged",
                         #"perinat_resp_cardio",
                         "pregnancy_complication_trauma",
                         "genitourinary",
                         "skin",
                         "digestive",
                         "circulatory",
                         "other_infectious",
                         "temp_reg"
)


##convert dichotomous variables to 0/1

ec_univariate_oct <- ec_final %>% 
  mutate(across(
    .cols = all_of(c("sex",
                     "urban_rural"
    )
    ),
    .fns = ~case_when(
      . %in% c("Male", "urban") ~ 1,
      . %in% c("Female", "rural") ~ 0,
      TRUE ~ NA_real_
    )
  )) %>% 
  mutate(
    across(
      c(sex,
        death_30,
        infant_age_cat,
        ethnicity_des2,
        imd_quintile,
        address,
        left_hospital,
        matage_grp,
        gestage_grp,
        birthwgt_grp,
        multibirth_bi,
        urban_rural,
        perinatal_merged,
        #apportionment_rule_category,
        #sample_onset,
        delivery_cat,
        #hospital_site_code,
        jaundice,
        hemato,
        endo_nutri_meta,
        accident_injury,
        congenital,
        #perinatal_other,
       # perinat_resp_cardio,
        pregnancy_complication_trauma,
        genitourinary,
        skin,
        eye,
        digestive,
        circulatory,
        other_infectious,
        neuro_nervo,
        temp_reg
      ),
      ~ as.factor(.)
    )
  )


## Now we drop rows with missing information for variables of interest

ec_univariate_oct <- ec_univariate_oct %>% 
  drop_na(any_of(c("death_30",
                   ec_explanvars_oct
  )
  )
  )


##LOGISTIC REGRESSION


## relevel the factor variables

ec_univariate_oct <- ec_univariate_oct %>% 
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
    infant_age_cat = as.factor(infant_age_cat)
  ) %>% 
  mutate(
    infant_age_cat = fct_relevel(
      infant_age_cat,
      "0-3 days",
      after = 0
    ) %>% 
      factor(levels=c(
        "0-3 days",
        "4-28 days",
        "29-90 days",
        "91-365 days")
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
  ) #%>% 
# mutate(
#   sample_onset = fct_relevel(
#     sample_onset,
#     "early onset",
#     after=0
#   )
# )


# ec_univariate3_presentation <- ec_univariate3 %>%
#   mutate(
#     sex = recode(sex, `0` = "Female", `1` = "Male"),
#     apportionment_rule_category = recode(apportionment_rule_category, `0` = "Hospital", `1` = "Community"),
#     urban_rural = recode(urban_rural, `0` = "urban", `1` = "rural"),
#     sample_onset = recode(sample_onset, `0` = "EOS", `1` = "LOS")
#   )

ec_univariate_oct_presentation <- ec_univariate_oct %>%
  mutate(
    sex = factor(sex, levels = c(1, 0), labels = c( "Male","Female")),
    apportionment_rule_category = factor(apportionment_rule_category, levels = c(0, 1), labels = c("Hospital", "Community")),
    urban_rural = factor(urban_rural, levels = c(0, 1), labels = c("rural", "urban")),
    #sample_onset = factor(sample_onset, levels = c(0, 1), labels = c("EOS", "LOS"))
  )

ec_univ_tab_oct <- ec_univariate_oct_presentation %>% 
  #ec_univariate3 %>% 
  dplyr::select(ec_explanvars_oct, death_30) %>% ## select variables of interest
  
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = death_30,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    exponentiate = TRUE,                     ## exponentiate to produce odds ratios (rather than log odds)
    test = "LTR",   ##likelihood test instead of wald
    label = list(
      sex = "Sex (infant)",
      #age_description = "Infant age group",
      matage_grp = "Maternal age group",
      ethnicity_des2 = "Ethnicity",
      infant_age_cat = "Infant age group",
      imd_quintile = "Multiple deprivation quintile",
      address = "Region of origin",
      urban_rural = "Population density",
      left_hospital = "Discharged?",
      #year_sample = "Year",
      gestage_grp = "Gestational age group",
      birthwgt_grp = "Birth weight group",
      multibirth_bi = "Single or multiple birth",
      delivery_cat = "Delivery type",
      #delivery_location = "Delivery location",
      mother_epidur = "Maternal hospital stay length",
      #antenatal_stay = "Antenatal stay length",
      jaundice = "Jaundice",
      perinatal_merged = "Perinatal disorders",
      hemato = "Hematological & hemorrhagic",
      endo_nutri_meta = "Endocrine, nutritional & metabolic",
      accident_injury = "Accident & injury",
      congenital = "Congenital",
      #perinatal_other = "Other perinatal disorders",
      #perinat_resp_cardio = "Perinatal respiratory & cardiovascular",
      pregnancy_complication_trauma = "Pregnancy complication & birth trauma",
      genitourinary = "Genitourinary",
      skin = "Skin",
      eye = "Eye",
      digestive = "Digestive",
      circulatory = "Circulatory",
      other_infectious = "Other infectious",
      neuro_nervo = "Neurological & nervous system",
      umbilical_central_catheter_28d_prior = "Umbilical/central catheter",
      ventilation_28d_prior = "Invasive ventilation",
      umbilical_central_catheter_90d_post = "Umbilical/central catheter",
      ventilation_90d_post = "Invasive ventilation",
      apportionment_rule_category = "Hospital/community onset",
      sample_onset = "EOS/LOS",
      temp_reg = "Temperature regulation"
    ) #,
    # levels = list(
    #   sex = c("0" = "Female",
    #           "1" = "Male"),
    #   apportionment_rule_category = c("0" = "Hospital",
    #                                   "1" = "Community"),
    #   urban_rural = c("0" = "urban",
    #                   "1" = "rural"),
    #   sample_onset = c( "0" = "EOS",
    #                     "1" = "LOS")
    # )
  ) %>% 
  bold_labels() %>% 
  bold_p()


ec_univ_tab_oct <- ec_univ_tab_oct %>% modify_column_hide(column = "stat_n")

vars_to_clean <- c(
  "jaundice",
  "accident_injury",
  "left_hospital",
  "hemato",
  "endo_nutri_meta",
  "congenital",
  "perinatal_other",
  "perinat_resp_cardio",
  "pregnancy_complication_trauma",
  "genitourinary",
  "skin",
  "eye",
  "digestive",
  "circulatory",
  "other_infectious",
  "temp_reg"
)

ec_univ_tab_oct <- ec_univ_tab_oct %>%
  modify_table_body(
    ~ .x %>%
      mutate(
        level = ifelse(
          variable %in% vars_to_clean & reference_row == TRUE,
          gsub("\\s*\\(ref\\s*=\\s*.*\\)", "", level),
          level
        )
      )
  )

