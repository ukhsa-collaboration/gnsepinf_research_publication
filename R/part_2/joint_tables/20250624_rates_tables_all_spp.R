###Mega table

##code updated on the 30th June 2025

library(dplyr)
library(purrr)
library(flextable)
source("paths/import_datasets.R")

# Define a function that renames rate columns to include the pathogen letter (e.g. A_Incidence)
prep_pathogen <- function(df, pathogen) {
  df %>%
    rename_with(~ paste0(pathogen, "_", .), c("cases", "incidence", "deaths", "mortality_rate"))
}

ec <- bind_rows(
  prep_pathogen(ec_imdtable_paper, "ec"),
  prep_pathogen(ec_ethrates_table, "ec"),
  prep_pathogen(ec_pop_rates, "ec")
) %>% 
  mutate(ec_cases_incidence = sprintf("%d (%.1f)", 
                                   ec_cases,
                                   ec_incidence)) %>% 
  mutate(ec_deaths_incidence = sprintf("%d (%.1f)", 
                                   ec_deaths,
                                   ec_mortality_rate)) %>% 
  select(-population_ethn11.x,
         -ec_cases,
         -ec_incidence,
         -ec_deaths,
         -ec_mortality_rate)

kleb <- bind_rows(
  prep_pathogen(kleb_imdtable_paper, "kleb"),
  prep_pathogen(kleb_ethrates_table, "kleb"),
  prep_pathogen(kleb_pop_rates, "kleb")
) %>% 
  mutate(kleb_cases_incidence = sprintf("%d (%.1f)", 
                                   kleb_cases,
                                   kleb_incidence)) %>% 
  mutate(kleb_deaths_incidence = sprintf("%d (%.1f)", 
                                    kleb_deaths,
                                    kleb_mortality_rate)) %>% 
  select(-population_ethn11.x,
         -kleb_cases,
         -kleb_incidence,
         -kleb_deaths,
         -kleb_mortality_rate)

pseu <- bind_rows(
  prep_pathogen(pseu_imdtable_paper, "pseu"),
  prep_pathogen(pseu_ethrates_table, "pseu"),
  prep_pathogen(pseu_pop_rates, "pseu")
) %>% 
  mutate(pseu_cases_incidence = sprintf("%d (%.1f)", 
                                   pseu_cases,
                                   pseu_incidence)) %>% 
  mutate(pseu_deaths_incidence = sprintf("%d (%.1f)", 
                                    pseu_deaths,
                                    pseu_mortality_rate)) %>% 
  select(-population_ethn11.x,
         -pseu_cases,
         -pseu_incidence,
         -pseu_deaths,
         -pseu_mortality_rate)

# Merge all the pathogen tables together by "Variable" and "Group"
all_data <- full_join(ec, kleb, by = c("variable", "group")) %>%
  full_join(pseu, by = c("variable", "group"))


df_final <- all_data %>%
  group_by(variable) %>%             # Group by IMD, Ethnicity, etc.
  group_split() %>%                  # Split into list of smaller data frames
  purrr::map_dfr(~{
    section <- unique(.x$variable)   # Get section name
    
    # Find numeric/rate column names (everything except variable & group)
    rate_cols <- setdiff(names(.x), c("variable", "group"))
    
    # Create a tibble with NA values for all rate columns
    header_row <- tibble(
      variable = section,
      group = "",
      !!!setNames(rep(NA, length(rate_cols)), rate_cols)
    )
    
    # Combine the header row and the data
    bind_rows(header_row, .x)
  })
df_final <- df_final %>% 
  mutate(
    variable = ifelse(group != "", "", variable)
  )

# Create the flextable
ft <- flextable(df_final)# Rename columns to readable labels
ft <- ft %>% 
  set_header_labels(
    variable = "",
    group = "",
    ec_cases_incidence = "Number of cases (incidence per 100,000 live-births)", ec_deaths_incidence = "Number of deaths (mortality rate per 100,000 live-births)",
    kleb_cases_incidence = "Number of cases (incidence per 100,000 live-births)", kleb_deaths_incidence = "Number of deaths (mortality rate per 100,000 live-births)",
    pseu_cases_incidence = "Number of cases (incidence per 100,000 live-births)", pseu_deaths_incidence = "Number of deaths (mortality rate per 100,000 live-births)"
    )


# Add a top header row with pathogen names
ft <- ft %>% 
  add_header_row(
    values = c("", "", "Escherichia coli", "Klebsiella spp.", "Pseudomonas aeruginosa"),  # Titles for grouped columns
    colwidths = c(1, 1, 2, 2, 2)  # Each pathogen has 4 columns
  ) %>%
  italic(i = 1, j = 3:8, part = "header") %>%      # Italicize the pathogen names
  align(align = "center", part = "header")         # Center all header text

# Merge the pathogen names across their 4 columns
ft <-  merge_h(ft, part = "header")

# Find section header rows (where Group is blank)
section_rows <- which(df_final$group == "")

# Format section rows: bold + italic + merge all cells in row
ft <- bold(ft, i = section_rows, part = "body")
ft <- italic(ft, i = section_rows, part = "body")
ft <- merge_h(ft, i = section_rows, part = "body")
ft <- align(ft, i = section_rows, align = "left", part = "body")
border_style = officer::fp_border(color="black", width=1)


# Apply a clean style and auto-fit the table
#ft <- theme_booktabs(ft)
ft <- ft %>% 
  vline(part="all", j=2, border=border_style) %>% 
  vline(part = "all", j=4, border=border_style) %>% 
  vline(part="all", j=6, border=border_style) %>% 
  align(align = "center", j= c(2:7), part = "all") %>% 
  bold(i=1, bold=TRUE, part="header") %>% 
  bold(j=1, bold=TRUE, part="body") %>% 
  bold(i=2, bold=TRUE, part="header")

ft <- footnote(ft, i = 1, j = c(3, 5, 7),
                 #c(1, 7, 17), j=1,
               value = as_paragraph(
                 c("Live-births ONS data 2011-2019",
                   "Live-births ONS data 2017-2019",
                   "")
               ),
               ref_symbols = c("a", "b", "b"),
               part= "header"
)

ft <- autofit(ft)


save_as_docx(
  "Incidence and case mortality rates for Escherichia coli (E.coli), Klebsiella spp and Pseudomonas aeruginosa (P. aeruginosa) stratified for IMD quintile, ethnicity and Region of origin" = ft,
  path = here("outputs", "20250701_rates_all_spp.docx"))
save_as_html(
  "Incidence and case mortality rates for Escherichia coli (E.coli), Klebsiella spp and Pseudomonas aeruginosa (P. aeruginosa) stratified for IMD quintile, ethnicity and Region of origin" = ft,
  path = here("outputs", "20250701_rates_all_spp.html"))
