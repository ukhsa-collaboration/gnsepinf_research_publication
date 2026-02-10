# Gram Negative Bacteraemia in infants in England 2011-2019 code repository

## Purpose
This repository contains the code that was use to conduct the analysis and produce the figures and tables included in the publication "Characteristics and factors associated with mortality of infants with Gram-negative bacteraemia in England, 2011-2019: A national retrospective cohort population-based surveillance data-linkage study"

## Untracked files
This repository only contains non-disclosive files, that is, code without file paths, and summary statistics. This project is set up so only files that are safe to upload to Github, such as code, are uploaded by default. This means all files ending in `.csv`, and all files in the `data/` and `paths/` folders (except README) are untracked, i.e. they will not be uploaded to GitHub.

## File tree

```
GNBSI_research_project-r/
├── paths/
│   ├── README.md
│   └── paths.R (untracked)
│   └──import_datasets.R (untracked)
├── R/
│   ├── README.md
│   └── part_1/
│   └──── E.coli/
│   └─────── birth_weight.R
│   └─────── DCS_Ecoli_frequencyepisodes.R
│   └─────── DCS-BR-mHES_linkage_Ecoli.R
│   └─────── DSC_Ecoli_cohort.R
│   └─────── DSC_iHES_linkage_New.R
│   └─────── gestational_age.R
│   └─────── HES_spells.R
│   └─────── mortality.R
│   └─────── mothers_ethnicity.R
│   └─────── multiple_birth_count.R
│   └─────── pull_DCS_Ecoli.R
│   └─────── recurrence.R
│   └─────── social_deprivation.R
│   └─────── workflow_E.coli.R
│   └──── Klebsiella/
│   └─────── Same as above but for the Klebsiella cohort
│   └──── P.aeruginosa/
│   └─────── Same as above but for the P. aeruginosa cohort
│   └── part_2
│   └──── E.coli/
│   └─────── cleaning_merging/
│   └─────────── ec_comorbidities.R
│   └─────────── ec_data_clean.R
│   └─────────── ec_invasive_procedures.R
│   └─────────── ec_merging_cleaning.R
│   └─────────── ec_merging_final_dataset.R
│   └─────────── ec_seasonality.R
│   └─────────── ec_seasons.R
│   └─────────── ec_vitality.R
│   └─────────── explore_missingness.R
│   └─────── clustering/
│   └─────────── ec_centre_funnel.R
│   └─────────── ec_hospital_funnel.R
│   └─────────── ec_trust_funnel.R
│   └─────────── ICC.R
│   └─────── descriptive_tables/
│   └─────────── contains scripts for all sorts of descriptive tables
│   └─────── logistic_regression/
│   └─────────── univariate/
│   └─────────────── 20251029_ec_univar_30d.R
│   └─────────────── ec_paper_univariable.R
│   └─────────── multivariable/
│   └─────────────── 20251127_ec_multivariable_FINAL.R
│   └─────── survival_analysis/
│   └─────────────── ec_survival.R
│   └──── Klebsiella/
│   └─────── Same as above but for the Klebsiella cohort
│   └──── P.aeruginosa/
│   └─────── Same as above but for the P. aeruginosa cohort
│   └──── joint_tables/
│   └─────── 20250625_rates_table_all_spp.R
│   └─────── 20250528_all_supplementary_descriptive.R
│   └─────── 20250625_rates_line_chart.R
│   └─────── 20250625_totals_rates_all_spp.R
│   └─────── 20250630_timeline_manuscript_incidence_episodes.R
│   └─────── 20250929_medians.R
│   └─────── amr_tables.R
│   └──── joint_figures/
│   └─────── 20250527_survival_spp.R
│   └─────── 20250528_survivall_illness_allspp.R
├── project-template-r.Rproj
└── README.md
```
