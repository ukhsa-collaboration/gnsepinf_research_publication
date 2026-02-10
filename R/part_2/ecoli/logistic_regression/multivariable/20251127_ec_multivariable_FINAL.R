###27/11/2025
###TABLE FOR SUBMISSION of publication
source("paths/import_datasets.R")

pacman::p_load(
  rio,            # import/export
  here,           # file pathways
  flextable,      # make HTML tables 
  officer,
  gtsummary,# helper functions for tables
  tidyverse)      # data management, summary, and visualization
###all mortality of 30d

border_style = officer::fp_border(color="black", width = 1)

ec_model_final_paper <- tbl_merge(
  tbls = list(ec_univ_tab_oct, ec_jaundice_model_paper),
  tab_spanner = c("**Univariate**", "**Multivariable**")
) %>% 
  as_flex_table()
#modify_caption("Univariate and multivariable logistic regression models (E. coli)")
ec_model_final_paper<- ec_model_final_paper %>% 
  vline(part= "all", j=2, border=border_style) %>% 
  vline(part= "all", j=5, border=border_style) %>% 
  vline(part= "all", j=8, border=border_style)

save_as_docx(
  "Table 4. Unadjusted and adjusted multivariable logistic regression analysis of potential predictors of all-cause mortality 30 days after the first positive E. coli specimen" = ec_model_final_paper,
  path = here("outputs", "20251127_ec_table_manuscript.docx"))


##with the descriptive

ec_manuscript_table <- tbl_merge(
  tbls = list(ec_descriptive_table, ec_univ_tab_oct, ec_jaundice_model_paper),
  tab_spanner = c(" ", "**Unadjusted**", "**Adjusted**")
) %>% 
  as_flex_table()

ec_manuscript_table <- ec_manuscript_table  %>% 
  vline(part= "all", j=4, border=border_style) %>% 
  vline(part= "all", j=8, border=border_style) 

save_as_docx(
  "Table 4. Unadjusted and adjusted multivariable logistic regression analysis of potential predictors of all-cause mortality 30 days after the first positive E. coli specimen" = ec_manuscript_table,
  path = here("outputs", "20251127_ec_table_manuscript.docx"))
