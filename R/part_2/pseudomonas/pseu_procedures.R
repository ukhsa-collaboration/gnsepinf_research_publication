###invasive procedures pseudomonas

## First we will recode the variables
source("paths/import_datasets.R")


pseu_invasive <- pseu_invas %>% 
  mutate(urinary_catheter = case_when(
    str_detect(icd10_proc, "M47") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(umbilical_central_catheter = case_when(
    icd10_proc %in% c("O152", "O153", "L911", "L912", "L913", "L914", "L997", "L915", "L916") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(intubation = case_when(
    icd10_proc %in% c("G211",	"G212",	"G213",	"G214",	"G47",	"G471",	"G472",	"G474",	"G478",	"G479",	"G571",	"G572") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(ventilation = case_when(
    icd10_proc %in% c("E851") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(cardiac_surgery = case_when(
    icd10_proc %in% c("K171",	"K18",	"K185",	"K187",	"K188",	"K189",	"K19",	"K196",	"K198",	"K199",	"K553",	"K554",	"K585",	"K60",	"K601",	"K602",	"K603",	"K604",	"K605",	"K606",	"K607",	"K608",	"K609",	"K61",	"K611",	"K612",	"K613",	"K614", "K615",	"K616",	"K617",	"K618",	"K619",	"K681",	"K73",	"K731",	"K732",	"K733",	"K738",	"K739",	"K74",	"K741",	"K742",	"K743",	"K748",	"K749",	"K76",	"K761",	"K768",	"K769") ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(bowel_gastric_surgery = case_when(
    str_detect(icd10_proc, "H62")  ~ "yes",
    TRUE ~ "no"
  )) %>% 
  distinct()

## for this dataset, I want to know what procedures happened before and after
##the positive specimen
##then for catheter and intubiation and anything vascular 28 days before positive
##then for surgery 30 days pre bacteriemia

## to calculate that i will link with data from the raw deduplicated datafile
##that I will use just for the following variables: id, nhs, dob_br, data_collection_date,
##date_admitted

pseu_spell <- pseu_infants %>% 
  select(id,
         nhs.x,
         dob_br,
         data_collection_date,
         date_admitted
  )
pseu_procedures <- left_join(pseu_invasive,
                             pseu_spell,
                             by = c("newnhsno" = "nhs.x"))

##now that i have the matches I do a few changes
pseu_procedures <- pseu_procedures %>% 
  mutate(spell_start_date = as.Date (spell_start_date) ##format so i can use dates
  ) %>% 
  mutate(days_procedure_pre_episode = data_collection_date - spell_start_date
  ) %>% 
  mutate(cardiac_surgery_30d_prior = case_when(
    (cardiac_surgery == "yes")
    & days_procedure_pre_episode <= 0 
    & days_procedure_pre_episode >= -30 ~ "yes",
    TRUE ~ "no"
  )
  ) %>%
  mutate(bowel_gastric_surgery_30d_prior = case_when(
    (bowel_gastric_surgery == "yes")
    & days_procedure_pre_episode <= 0 
    & days_procedure_pre_episode >= -30 ~ "yes",
    TRUE ~ "no"
  )
  ) %>%
  mutate(urinary_catheter_28d_prior = case_when (
    (urinary_catheter == "yes") 
    & days_procedure_pre_episode <= 0 
    & days_procedure_pre_episode >= -28 ~ "yes",
    TRUE ~ "no"
  )
  ) %>%
  mutate(umbilical_central_catheter_28d_prior = case_when (
    (umbilical_central_catheter == "yes") 
    & days_procedure_pre_episode <= 0 
    & days_procedure_pre_episode >= -28 ~ "yes",
    TRUE ~ "no"
  )
  ) %>%
  mutate(ventilation_28d_prior = case_when (
    (ventilation == "yes") 
    & days_procedure_pre_episode <= 0 
    & days_procedure_pre_episode >= -28 ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(intubation_28d_prior = case_when (
    (intubation == "yes") 
    & days_procedure_pre_episode <= 0 
    & days_procedure_pre_episode >= -28 ~ "yes",
    TRUE ~ "no"
  )
  ) %>% ## then for post bacteriemia i will set a limit of 90 days?
  mutate(cardiac_surgery_90d_post = case_when(
    (cardiac_surgery == "yes")
    & days_procedure_pre_episode > 0 
    & days_procedure_pre_episode <= 90 ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(bowel_gastric_surgery_90d_post = case_when(
    (bowel_gastric_surgery == "yes")
    & days_procedure_pre_episode > 0 
    & days_procedure_pre_episode <= 90 ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(urinary_catheter_90d_post = case_when(
    (urinary_catheter == "yes") 
    & days_procedure_pre_episode > 0 
    & days_procedure_pre_episode <= 90 ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(umbilical_central_catheter_90d_post = case_when(
    (umbilical_central_catheter == "yes") 
    & days_procedure_pre_episode > 0 
    & days_procedure_pre_episode <= 90 ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(ventilation_90d_post = case_when(
    (ventilation == "yes") 
    & days_procedure_pre_episode > 0 
    & days_procedure_pre_episode <= 90 ~ "yes",
    TRUE ~ "no"
  )
  ) %>% 
  mutate(intubation_90d_post = case_when(
    (intubation == "yes") 
    & days_procedure_pre_episode > 0 
    & days_procedure_pre_episode <= 90 ~ "yes",
    TRUE ~ "no"
  )
  )

pseu_procedures <- pseu_procedures %>% 
  drop_na(any_of(c("days_procedure_pre_episode")))


##I think i will need to merge 4 separate times
## one time for the procedure 28d prior, surgery 30 days prior, then procedure 90d after,
##and procedure 90d after
## or should i merge for each procedure? is it the only way in which i will not loose data?
pseu_group_procedures <- pseu_procedures %>% 
  select(newnhsno,
         cardiac_surgery_30d_prior,
         bowel_gastric_surgery_30d_prior,
         urinary_catheter_28d_prior,
         umbilical_central_catheter_28d_prior,
         ventilation_28d_prior,
         intubation_28d_prior,
         cardiac_surgery_90d_post,
         bowel_gastric_surgery_90d_post,
         urinary_catheter_90d_post,
         umbilical_central_catheter_90d_post,
         ventilation_90d_post,
         intubation_90d_post
  ) %>% 
  group_by(newnhsno) %>% 
  summarize(across(everything(), ~ ifelse(any(. == "yes"), "yes", "no")))

## now I merge the dataset with ec_infants_comorb
pseu_final <- left_join(pseu_infants_comorb,
                        pseu_group_procedures,
                        by = c("nhs.x" = "newnhsno"))
