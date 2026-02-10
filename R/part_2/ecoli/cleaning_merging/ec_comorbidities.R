## for comorbidities
source("paths/import_datasets.R")


ec_comorbidity <- ec_comorb %>% 
  mutate(jaundice = case_when(
    str_detect(diag_concat, "^(P58|P59|R17)") ~ "yes",
    str_detect(diag_concat, ";(P58|P59|R17)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(hemato = case_when(
    str_detect(diag_concat, "^(D5|D6|D7|D8|P5|P)") ~ "yes",
    str_detect(diag_concat, ";(D5|D6|D7|D8|P5|P)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(neoplasm_carcinom = case_when(
    str_detect(diag_concat, "^(C|D0|D1|D2|D3|D4)") ~ "yes",
    str_detect(diag_concat, ";(C|D0|D1|D2|D3|D4)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(endo_nutri_meta = case_when(
    str_detect(diag_concat, "^(E0|E1|E2|E3|E4|E5|E6|E7|E8|P70|P71|P72|P74)") ~ "yes",
    str_detect(diag_concat, ";(E0|E1|E2|E3|E4|E5|E6|E7|E8|P70|P71|P72|P74)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(neuro_nervo = case_when(
    str_detect(diag_concat, "^(F0|F1|F2|F3|F4|F5|F6|F7|F8|F9|G1|G2|G3|G4|G5|G6|G7|G8|G9)") ~ "yes",
    str_detect(diag_concat, ";(F0|F1|F2|F3|F4|F5|F6|F7|F8|F9|G1|G2|G3|G4|G5|G6|G7|G8|G9)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(other_infectious = case_when(
    str_detect(diag_concat, "^(A01|A02|A03|A04|A05|A06|A07|A08|A09|A1|A2|A3|A40|A410|A411|A412|A413|A414|A42|A43|A44|A46|A48|A490|A491|A492|A943|A5|A6|A7|A8|A9|B0|B1|B2|B3|B4|B5|B6|B7|B8|B90|B91|B92|B94|B95|B960|B963|B964|B966|B967|B97|P35|P360|P361|P362|P363|P365|P37)") ~ "yes",
    str_detect(diag_concat, ";(A01|A02|A03|A04|A05|A06|A07|A08|A09|A1|A2|A3|A40|A410|A411|A412|A413|A414|A42|A43|A44|A46|A48|A490|A491|A492|A943|A5|A6|A7|A8|A9|B0|B1|B2|B3|B4|B5|B6|B7|B8|B90|B91|B92|B94|B95|B960|B963|B964|B966|B967|B97|P35|P360|P361|P362|P363|P365|P37)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(eye = case_when (
    str_detect(diag_concat, "^(H0|H1|H2|H3|H4|H5)") ~ "yes",
    str_detect(diag_concat, ";(H0|H1|H2|H3|H4|H5)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(ear =case_when(
    str_detect(diag_concat, "^(H6|H7|H8|H9)") ~ "yes",
    str_detect(diag_concat, ";(H6|H7|H8|H9)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(circulatory = case_when(
    str_detect(diag_concat, "^I") ~ "yes",
    str_detect(diag_concat, ";I") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(digestive= case_when(
    str_detect(diag_concat, "^(K|P76|P77|P78)") ~ "yes",
    str_detect(diag_concat, ";(K|P76|P77|P78)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(skin = case_when(
    str_detect(diag_concat, "^L") ~ "yes",
    str_detect(diag_concat, ";L") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(musculo = case_when(
    str_detect(diag_concat, "^M") ~ "yes",
    str_detect(diag_concat, ";M") ~ "yes",
    TRUE ~ "no"
  )
) %>% 
  mutate(genitourinary = case_when(
    str_detect(diag_concat, "^N") ~ "yes",
    str_detect(diag_concat, ";N") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(pregnancy_complication_trauma = case_when(
    str_detect(diag_concat, "^(P01|P02|P03|P04|P10|P11|P12|P13|P14|P15)") ~ "yes",
    str_detect(diag_concat, ";(P01|P02|P03|P04|P10|P11|P12|P13|P14|P15)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(perinat_resp_cardio = case_when(
    str_detect(diag_concat, "^(P19|P23|P24|P25|P26|P27|P28|P29)") ~ "yes",
    str_detect(diag_concat, ";(P19|P23|P24|P25|P26|P27|P28|P29)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(temp_reg = case_when(
    str_detect(diag_concat, "^(P80|P81|P83)") ~ "yes",
    str_detect(diag_concat, ";(P80|P81|P83)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(perinatal_other = case_when(
    str_detect(diag_concat, "^(P9)") ~ "yes",
    str_detect(diag_concat, ";P9") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(congenital = case_when(
    str_detect(diag_concat, "^Q") ~"yes",
    str_detect(diag_concat, ";Q") ~"yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(accident_injury = case_when(
    str_detect(diag_concat, "^(S|T|W|X|Y)") ~ "yes",
    str_detect(diag_concat, ";(S|T|W|X|Y)") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(dob_br = as.Date(dob_br)
           ) %>% 
  distinct() %>% 
  select(
    nhs,
         id,
        #dob_br,
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
         )
 
##merging dataset by id and nhs number
ec_infants_comorb <- left_join(ec_infants,
                               ec_comorbidity,
                               by = c("id"))
                               #by = c("nhs.x" = "nhs"))


tabyl(ec_infants_comorb$jaundice)
##explore
ec_infants_comorb %>% 
  mutate(death_90 = ifelse(death_90 == 1,
                           "Death",
                           "Survive")) %>% 
  select(death_90,
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
         neuro_nervo) %>% 
  tbl_summary(by= death_90,
              missing = "no",
              percent = "row",
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

###explore the missmatches

ecselected_vars_comor <- ec_infants_comorb %>% 
  select(sex,
         death_90,
         data_collection_date,
         ethnicity_des,
         imd04_grp,
         address,
         urban_rural,
         matage_grp,
         gestage_grp,
         birthwgt_grp,
         #multibirth_grp,
         multibirth_bi,
         age_description,
         apportionment_rule_category,
         #left_hospital_period,
         imd_quintile,
         phe_region_reporting,
         phe_centre_reporting,
         hospital_site_code,
         trust_provider_code,
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
         neuro_nervo
  )
gg_miss_var(ecselected_vars_comor, show_pct = TRUE)
gg_miss_fct(ecselected_vars_comor, phe_centre_reporting)
gg_miss_fct(ecselected_vars_comor, trust_provider_code)
gg_miss_fct(ecselected_vars_comor, hospital_site_code) +
  theme(axis.text.x = element_text(size = 3))
gg_miss_fct(ecselected_vars_comor, data_collection_date)
