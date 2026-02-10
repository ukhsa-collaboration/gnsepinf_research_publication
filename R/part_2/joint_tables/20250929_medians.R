###Median ages

pacman::p_load(dplyr,
               janitor,
               lubridate,
               skimr)
source("paths/import_datasets.R")

###EC matage median
 ec_br_dcs %>% 
  mutate_at(vars(mother_dob_br), ~ymd(.)
  ) %>% 
  mutate_at(vars(dob_br), ~ymd(.)
  ) %>% 
  mutate(matage=floor(decimal_date(dob_br) - decimal_date(mother_dob_br))
  ) %>% 
   summarise(median_age = median(matage, na.rm = TRUE),
             q1_age = quantile(matage, 0.25, na.rm = TRUE),
             q3_age = quantile(matage, 0.75, na.rm = TRUE),
             IQR_age = IQR(matage, na.rm = TRUE),
             mean_age = mean(matage, na.rm = TRUE),
             sd_age = sd(matage, na.rm = TRUE))
 
 
 ##Kleb matage median:
kleb_br_dcs <- import(here("data", "Linkage_Klebsiella_DCS_BR_Success.csv"))
 
  kleb_br_dcs %>% 
   mutate_at(vars(mother_dob_br), ~ymd(.)
   ) %>% 
   mutate_at(vars(dob_br), ~ymd(.)
   ) %>% 
   mutate(matage=floor(decimal_date(dob_br) - decimal_date(mother_dob_br))
   ) %>% 
    summarise(median_age = median(matage, na.rm = TRUE),
              q1_age = quantile(matage, 0.25, na.rm = TRUE),
              q3_age = quantile(matage, 0.75, na.rm = TRUE),
              IQR_age = IQR(matage, na.rm = TRUE),
              mean_age = mean(matage, na.rm = TRUE),
              sd_age = sd(matage, na.rm = TRUE))
 
 
 ##pseu mat median age
  pseu_br_dcs <- import(here("data", "Linkage_Paeruginosa_DCS_BR_Success.csv"))
  
 
 pseu_br_dcs%>% 
   mutate_at(vars(mother_dob_br), ~ymd(.)
   ) %>% 
   mutate_at(vars(dob_br), ~ymd(.)
   ) %>% 
   mutate(matage=floor(decimal_date(dob_br) - decimal_date(mother_dob_br))
   ) %>% 
   summarise(median_age = median(matage, na.rm = TRUE),
             q1_age = quantile(matage, 0.25, na.rm = TRUE),
             q3_age = quantile(matage, 0.75, na.rm = TRUE),
             IQR_age = IQR(matage, na.rm = TRUE),
             mean_age = mean(matage, na.rm = TRUE),
             sd_age = sd(matage, na.rm = TRUE))
 
 
 ##Infant age in DAYS for kleb
 
 kleb_br_dcs %>% 
   mutate(infage_days = difftime(
     data_collection_date,
     dob_br,
     unit="days"
   )
   ) %>% 
   summarise(
     median_age = median(infage_days, na.rm = TRUE),
     q1_age = quantile(infage_days, 0.25, na.rm = TRUE),
     q3_age = quantile(infage_days, 0.75, na.rm = TRUE),
     IQR_age = IQR(infage_days, na.rm = TRUE),
     mean_age = mean(infage_days, na.rm = TRUE),
     sd_age = sd(infage_days, na.rm = TRUE)
   )
 
 
 kleb_br_dcs %>% 
   mutate(infage_days = difftime(
     data_collection_date,
     dob_br,
     unit="days"
   )
   ) %>% 
   mutate(infant_age_cat = case_when(
     infage_days <= as.difftime(3, units = "days") ~ "0-3 days",
     infage_days > as.difftime(3, units = "days") & infage_days <= as.difftime(28, units = "days") ~ "4-28 days",
     infage_days > as.difftime(28, units = "days") & infage_days <= as.difftime(90, units = "days") ~ "29-90 days",
     infage_days > as.difftime(90, units = "days") & infage_days <= as.difftime(365, units = "days") ~ "91-365 days",
     TRUE ~ NA_character_
   )) %>% 
     tabyl(infant_age_cat)

  ##ecol
 ec_br_dcs %>% 
   mutate(infage_days = difftime(
     data_collection_date,
     dob_br,
     unit="days"
   )
   ) %>% 
   summarise(
     median_age = median(infage_days, na.rm = TRUE),
     q1_age = quantile(infage_days, 0.25, na.rm = TRUE),
     q3_age = quantile(infage_days, 0.75, na.rm = TRUE),
     IQR_age = IQR(infage_days, na.rm = TRUE),
     mean_age = mean(infage_days, na.rm = TRUE),
     sd_age = sd(infage_days, na.rm = TRUE)
   )
 
 
 ec_br_dcs %>% 
   mutate(infage_days = difftime(
     data_collection_date,
     dob_br,
     unit="days"
   )
   ) %>% 
   mutate(infant_age_cat = case_when(
     infage_days <= as.difftime(3, units = "days") ~ "0-3 days",
     infage_days > as.difftime(3, units = "days") & infage_days <= as.difftime(28, units = "days") ~ "4-28 days",
     infage_days > as.difftime(28, units = "days") & infage_days <= as.difftime(90, units = "days") ~ "29-90 days",
     infage_days > as.difftime(90, units = "days") & infage_days <= as.difftime(365, units = "days") ~ "91-365 days",
     TRUE ~ NA_character_
   )) %>% 
   tabyl(infant_age_cat)
 
 ##pseu
 pseu_br_dcs %>% 
   mutate(infage_days = difftime(
     data_collection_date,
     dob_br,
     unit="days"
   )
   ) %>% 
   summarise(
     median_age = median(infage_days, na.rm = TRUE),
     q1_age = quantile(infage_days, 0.25, na.rm = TRUE),
     q3_age = quantile(infage_days, 0.75, na.rm = TRUE),
     IQR_age = IQR(infage_days, na.rm = TRUE),
     mean_age = mean(infage_days, na.rm = TRUE),
     sd_age = sd(infage_days, na.rm = TRUE)
   )
 
 
 pseu_br_dcs %>% 
   mutate(infage_days = difftime(
     data_collection_date,
     dob_br,
     unit="days"
   )
   ) %>% 
   mutate(infant_age_cat = case_when(
     infage_days <= as.difftime(3, units = "days") ~ "0-3 days",
     infage_days > as.difftime(3, units = "days") & infage_days <= as.difftime(28, units = "days") ~ "4-28 days",
     infage_days > as.difftime(28, units = "days") & infage_days <= as.difftime(90, units = "days") ~ "29-90 days",
     infage_days > as.difftime(90, units = "days") & infage_days <= as.difftime(365, units = "days") ~ "91-365 days",
     TRUE ~ NA_character_
   )) %>% 
   tabyl(infant_age_cat)

 
 
 ###Ever left hospital and no
 
ec_br_dcs %>% 
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
      dob_br,
      units = "days"
    )
  ) %>% 
  mutate(
    diff_dob_collection = difftime(
      data_collection_date,
      dob_br,
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
  tabyl(left_hospital)


kleb_br_dcs %>% 
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
      dob_br,
      units = "days"
    )
  ) %>% 
  mutate(
    diff_dob_collection = difftime(
      data_collection_date,
      dob_br,
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
  tabyl(left_hospital)


pseu_br_dcs %>% 
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
      dob_br,
      units = "days"
    )
  ) %>% 
  mutate(
    diff_dob_collection = difftime(
      data_collection_date,
      dob_br,
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
  tabyl(left_hospital)



##Median for gestational weeks 
ec %>% 
  summarise(
    median_gest = median(gestat_1, na.rm = TRUE),
    q1_age = quantile(gestat_1, 0.25, na.rm = TRUE),
    q3_age = quantile(gestat_1, 0.75, na.rm = TRUE),
    IQR_age = IQR(gestat_1, na.rm = TRUE),
    mean_age = mean(gestat_1, na.rm = TRUE),
    sd_age = sd(gestat_1, na.rm = TRUE)
  )

##birthweight
ec %>% 
  summarise(
    median_gest = median(birthwgt_br, na.rm = TRUE),
    q1_age = quantile(birthwgt_br, 0.25, na.rm = TRUE),
    q3_age = quantile(birthwgt_br, 0.75, na.rm = TRUE),
    IQR_age = IQR(birthwgt_br, na.rm = TRUE),
    mean_age = mean(birthwgt_br, na.rm = TRUE),
    sd_age = sd(birthwgt_br, na.rm = TRUE)
  )

kleb %>% 
  summarise(
    median_gest = median(gestat_1, na.rm = TRUE),
    q1_age = quantile(gestat_1, 0.25, na.rm = TRUE),
    q3_age = quantile(gestat_1, 0.75, na.rm = TRUE),
    IQR_age = IQR(gestat_1, na.rm = TRUE),
    mean_age = mean(gestat_1, na.rm = TRUE),
    sd_age = sd(gestat_1, na.rm = TRUE)
  )

#birthweight
kleb %>% 
  summarise(
    median_gest = median(birthwgt_br, na.rm = TRUE),
    q1_age = quantile(birthwgt_br, 0.25, na.rm = TRUE),
    q3_age = quantile(birthwgt_br, 0.75, na.rm = TRUE),
    IQR_age = IQR(birthwgt_br, na.rm = TRUE),
    mean_age = mean(birthwgt_br, na.rm = TRUE),
    sd_age = sd(birthwgt_br, na.rm = TRUE)
  )

pseud %>% 
  summarise(
    median_gest = median(gestat_1, na.rm = TRUE),
    q1_age = quantile(gestat_1, 0.25, na.rm = TRUE),
    q3_age = quantile(gestat_1, 0.75, na.rm = TRUE),
    IQR_age = IQR(gestat_1, na.rm = TRUE),
    mean_age = mean(gestat_1, na.rm = TRUE),
    sd_age = sd(gestat_1, na.rm = TRUE)
  )
#birthweight
pseud %>% 
  filter(birthwgt_br != 0) %>% 
  summarise(
    median_gest = median(birthwgt_br, na.rm = TRUE),
    q1_age = quantile(birthwgt_br, 0.25, na.rm = TRUE),
    q3_age = quantile(birthwgt_br, 0.75, na.rm = TRUE),
    IQR_age = IQR(birthwgt_br, na.rm = TRUE),
    mean_age = mean(birthwgt_br, na.rm = TRUE),
    sd_age = sd(birthwgt_br, na.rm = TRUE)
  )

##IMD median

ec_postcodes %>% 
  mutate(imd = case_when(
    imd_quintile == "1st" ~ 1,
    imd_quintile == "2nd" ~ 2,
    imd_quintile == "3rd"~ 3,
    imd_quintile == "4th" ~ 4,
    imd_quintile == "5th" ~5
  )) %>% 
  summarise(
    median_gest = median(imd, na.rm = TRUE),
    q1_age = quantile(imd, 0.25, na.rm = TRUE),
    q3_age = quantile(imd, 0.75, na.rm = TRUE),
    IQR_age = IQR(imd, na.rm = TRUE),
    mean_age = mean(imd, na.rm = TRUE),
    sd_age = sd(imd, na.rm = TRUE)
  )

kleb_postcodes %>% 
  mutate(imd = case_when(
    imd_quintile == "1st" ~ 1,
    imd_quintile == "2nd" ~ 2,
    imd_quintile == "3rd"~ 3,
    imd_quintile == "4th" ~ 4,
    imd_quintile == "5th" ~5
  )) %>% 
  summarise(
    median_gest = median(imd, na.rm = TRUE),
    q1_age = quantile(imd, 0.25, na.rm = TRUE),
    q3_age = quantile(imd, 0.75, na.rm = TRUE),
    IQR_age = IQR(imd, na.rm = TRUE),
    mean_age = mean(imd, na.rm = TRUE),
    sd_age = sd(imd, na.rm = TRUE)
  )


pseu_postcodes %>% 
  mutate(imd = case_when(
    imd_quintile == "1st" ~ 1,
    imd_quintile == "2nd" ~ 2,
    imd_quintile == "3rd"~ 3,
    imd_quintile == "4th" ~ 4,
    imd_quintile == "5th" ~5
  )) %>% 
  summarise(
    median_gest = median(imd, na.rm = TRUE),
    q1_age = quantile(imd, 0.25, na.rm = TRUE),
    q3_age = quantile(imd, 0.75, na.rm = TRUE),
    IQR_age = IQR(imd, na.rm = TRUE),
    mean_age = mean(imd, na.rm = TRUE),
    sd_age = sd(imd, na.rm = TRUE)
  )
