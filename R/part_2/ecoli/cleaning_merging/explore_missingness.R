##Exploring missing data

##assess missingness

pacman::p_load(
  rio,           # import/export
  tidyverse,     # data mgmt and viz
  naniar,        # assess and visualize missingness
  mice           # missing data imputation
)
source("paths/import_datasets.R")

#percent of ALL data frame values that are missing
pct_miss(ec_infants)

##percent of rows with any value missing

pct_miss_case(ec_infants)


#visualize missingness
ecselected_vars <- ec_infants %>% 
  select(sex,
        death_90,
        data_collection_date,
        ethnicity_des,
        #imd04_grp,
        address,
        urban_rural,
        matage_grp,
        gestage_grp,
        birthwgt_grp,
        multibirth_grp,
        multibirth_bi,
        age_description,
        apportionment_rule_category,
        left_hospital_period,
        imd_quintile,
        phe_region_reporting,
        phe_centre_reporting,
        hospital_site_code,
        trust_provider_code
        )

gg_miss_var(ecselected_vars, show_pct = TRUE)

gg_miss_var(ecselected_vars, show_pct = TRUE, facet= death_90)

##seems very much equally distributed missing data

gg_miss_fct(ecselected_vars, address)

gg_miss_fct(ecselected_vars, phe_centre_reporting)
gg_miss_fct(ecselected_vars, trust_provider_code)
gg_miss_fct(ecselected_vars, hospital_site_code) +
  theme(axis.text.x = element_text(size = 3))
gg_miss_fct(ecselected_vars, data_collection_date)

##interesting because it seems like we are missing all the data on the variable imd04_grp for 2018
pct_miss_case(ecselected_vars)
vis_miss(ecselected_vars)


na_LONDON <- ecselected_vars %>%  
  filter(phe_centre_reporting == "LONDON")

gg_miss_fct(na_LONDON, hospital_site_code)



##explore missingness in the univariate dataframe
ecselected_vars_uni <- ec_univariate %>% 
  select(sex,
         death_90,
         #data_collection_date,
         ethnicity_des,
         #imd04_grp,
         #address,
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
         #phe_region_reporting,
         #phe_centre_reporting,
         hospital_site_code,
         trust_provider_code
  )
gg_miss_var(ecselected_vars_uni, show_pct = TRUE)
