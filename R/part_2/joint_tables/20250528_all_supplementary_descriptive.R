###Merging the supplementary table from different pathogens
source("paths/import_datasets.R")

pacman::p_load(
  rio,            # import/export
  here,           # file pathways
  flextable,      # make HTML tables 
  officer,        # helper functions for tables
  tidyverse)      # data management, summary, and visualization
###all mortality of 30d

border_style = officer::fp_border(color="black", width = 1)

descriptive_supplementary <- tbl_merge(
  tbls = list(ec_supplementary, kleb_supplementary, pseu_supplementary),
  tab_spanner = c("**Escherichia coli**", "**Klebsiella spp**", "**Pseudomona aeruginosa**")
) %>% 
  as_flex_table()

descriptive_supplementary <- descriptive_supplementary  %>% 
  vline(part= "all", j=6, border=border_style) %>% 
  vline(part= "all", j=11, border=border_style) 

save_as_html(
  "Supplementary table 1. Escherichia coli, Klebsiella spp., and Pseudomonas aeruginosa case characteristics by 30 day all caouse mortality outcome" = descriptive_supplementary,
  path = here("outputs", "20250528_supplementary_table1.html"))

