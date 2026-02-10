###km curve with the event being the children age falling ill

##First I will need to create a new variable 
library(dplyr)
source("paths/import_datasets.R")

ec_ageill <- ec_final %>% 
  dplyr::select(id,
         data_collection_date,
         data_collection,
         infage_days,
         infant_age_cat
  ) %>% 
  mutate(ill = case_when(
    infage_days <366 ~1,
    infage_days >366 ~0
  ))

kleb_ageill <- kleb_final %>% 
  dplyr::select(id,
         data_collection_date,
         data_collection,
         infage_days,
         infant_age_cat
  )%>% 
  mutate(ill = case_when(
    infage_days <366 ~1,
    infage_days >366 ~0
  ))

pseu_ageill <- pseu_final %>% 
  dplyr::select(id,
         data_collection_date,
         data_collection,
         infage_days,
         infant_age_cat
  ) %>% 
  mutate(infage_days = as.numeric(infage_days)
         ) %>% 
  mutate(ill = case_when(
    infage_days <366 ~1,
    infage_days >366 ~0
  ))

combined_ageill <- bind_rows(ec_ageill, kleb_ageill)
combined_ageill <- bind_rows(combined_ageill, pseu_ageill)

tabyl(combined_ageill$data_collection
)


pacman::p_load(
  rio,       # import data
  Hmisc,     # describing data
  skimr,     # summarise data  
  dplyr,     # adds new variables and preserves existing 
  janitor,   # Data cleaning, table display
  flextable, # creating and customising tables
  EpiStats,  # Tools for Epidemiologists
  broom,     # model extraction
  sjPlot,    # tabulates models
  forcats,   # managing and manipulating factor variables
  lubridate, # dealing with times and dates
  epiR,      # tools for analysis
  stats,     # for glm
  lmtest,    # for likelihood ratio test
  ggplot2,   # data visualisation
  MASS,      # negative binomial
  multcomp,  # gets combination of ORs or RRs
  sandwich,   # Robust covariance matrix estimators
  survival,
  survminer
)

#first we need to create a survival object
summary(combined$diff_time_death_bact)

##first I prepare the dataset
combined_surv_ill <- combined_ageill %>% 
  mutate(
    futime = ifelse(!is.na(infage_days), infage_days, 365)
  ) %>% 
  mutate(
    futime= case_when(
      futime >365 ~ 365,
      TRUE ~ futime
    )
  ) %>% 
  mutate(ill = as.numeric(ill))

tabyl(combined_surv$futime)
combined_surv_ill %>% dplyr::select(futime) %>% head(50)


combined_survobj <- Surv(time = combined_surv_ill$futime,
                         event = combined_surv_ill$ill) 

head(combined_survobj, 50)
combined_linelistsurv_fit_ill <-  survival::survfit(combined_survobj ~ 1,
                                                data = combined_surv_ill)
summary(combined_linelistsurv_fit)
##print summary at specific times

plot(combined_linelistsurv_fit_ill, 
     xlab = "Days of follow-up",    # x-axis label
     ylab="Survival Probability",   # y-axis label
     main= "Overall survival curve" # figure title
)

### now we create a plot for the risk factors
# Create the survival object for sex
km.ill <- survival::survfit(combined_survobj ~ data_collection, 
                                 data = combined_surv_ill)

# Kaplan-Meier curve stratified by 'sex' variable (base R)
plot(km.ill, 
     main = "Kaplan-Meier Curve for E.coli sepsis death by gender",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red", "black"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("E. coli", "Klebsiella spp.", "P. aeruginosa"), 
       col = c("blue", "red", "black"), 
       lwd = 1)
# checking for the difference
survival::survdiff(combined_survobj ~ data_collection, 
                   data = combined_surv_ill)




# plot
combined_km_ill <- ggsurvplot( 
  km.ill,
  data = combined_surv_ill,
  size = 1, #linetype = "strata",   # line types
  conf.int = T,
  surv.scale = "percent",  
  xlim= c(0, 365),
  break.time.by = 30, 
  xlab = "Infant age (days)",
  ylab= "GNBSI probability",
  pval = T,
  pval.coord = c(210,.4),
  pval.size = 4,
  risk.table = T,
  legend.title = "Specimen",
  legend.labs = c("E. coli", "Klebsiella spp.", "P. aeruginosa"),
  font.legend = 11,
  fontsize= 6,
  risk.table.fontsize = 4,
  risk.table.y.text = 5,
  risk.table.y.text.col = 5,
  palette = c("#003B5C","#E40046", "#00AB8E"),
  #surv.median.line = "hv", 
  ggtheme = theme_light()
)

combined_km_ill$plot <- combined_km_ill$plot +
  theme(
    axis.title.x = element_text(size = 14),  # KM x-axis title
    axis.title.y = element_text(size = 14),  # KM y-axis title
    axis.text.x  = element_text(size = 12),  # KM x-axis tick labels
    axis.text.y  = element_text(size = 12),  # KM y-axis tick labels
    legend.text  = element_text(size = 12),  # Legend labels
    legend.title = element_text(size = 12)   # Legend title
  )


full_plot <- grid.arrange(
  combined_km_ill$plot,
  combined_km_ill$table,
  ncol = 1,
  heights = c(3, 1)  # adjust relative heights of plot and risk table
)

ggsave(
  filename = "km_plot_with_risktable.jpeg",
  plot = full_plot,
  #width = 8, height = 8,   # adjust total size
  dpi = 300
)




