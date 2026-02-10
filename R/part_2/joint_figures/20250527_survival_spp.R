#KM survival curve for the different pathogens

##First I will need to create a new variable 
library(dplyr)
source("paths/import_datasets.R")

ec_combined <- ec_final %>% 
  select(id,
         data_collection_date,
         data_collection,
         death_90,
         DateOfDeath,
         diff_time_death_bact
         )

kleb_combined <- kleb_final %>% 
  select(id,
         data_collection_date,
         data_collection,
         death_90,
         DateOfDeath,
         diff_time_death_bact
  )

pseu_combined <- pseu_final %>% 
  select(id,
         data_collection_date,
         data_collection,
         death_90,
         DateOfDeath,
         diff_time_death_bact
         
  )

combined <- bind_rows(ec_combined, kleb_combined)
combined <- bind_rows(combined, pseu_combined)

tabyl(combined$data_collection
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

##then I prepare the dataset
combined_surv <- combined %>% 
  mutate(
    futime = ifelse(!is.na(diff_time_death_bact), diff_time_death_bact, 90)
  ) %>% 
  mutate(
    futime= case_when(
      futime >90 ~ 90,
      TRUE ~ futime
    )
  ) %>% 
  mutate(death_90 = as.numeric(death_90))

tabyl(combined_surv$futime)
combined_surv %>% dplyr::select(futime) %>% head(50)


combined_survobj <- Surv(time = combined_surv$futime,
                event = combined_surv$death_90) 

head(combined_survobj, 50)
combined_linelistsurv_fit <-  survival::survfit(combined_survobj ~ 1,
                                       data = combined_surv)
summary(combined_linelistsurv_fit)
##print summary at specific times

plot(combined_linelistsurv_fit, 
     xlab = "Days of follow-up",    # x-axis label
     ylab="Survival Probability",   # y-axis label
     main= "Overall survival curve" # figure title
)

### now we create a plot for the risk factors
# Create the survival object for sex
km.combined <- survival::survfit(combined_survobj ~ data_collection, 
                                 data = combined_surv)

# Kaplan-Meier curve stratified by 'sex' variable (base R)
plot(km.combined, 
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
                   data = combined_surv)




# plot
combined_km <- ggsurvplot( 
  km.combined,
  data = combined_surv,
  size = 1, #linetype = "strata",   # line types
  conf.int = T,
  surv.scale = "percent",  
  xlim= c(0, 90),
  break.time.by = 10, 
  xlab = "Days since first positive specimen",
  ylab= "Survival Probability",
  pval = T,
  pval.coord = c(70,.4),
  pval.size = 3,
  risk.table = T,
  legend.title = "Specimen",
  legend.labs = c("E. coli", "Klebsiella spp.", "P. aeruginosa"),
  font.legend = 11,
  font.x = 9,
  font.y=9,
  fontsize= 6,
  risk.table.fontsize = 4,
  risk.table.y.text = 5,
  risk.table.y.text.col = 5,
  palette = c("#003B5C","#E40046", "#00AB8E"),
  #surv.median.line = "hv", 
  ggtheme = theme_light()
)


combined_km$plot <- combined_km$plot +
  theme(
    axis.title.x = element_text(size = 14),  # KM x-axis title
    axis.title.y = element_text(size = 14),  # KM y-axis title
    axis.text.x  = element_text(size = 12),  # KM x-axis tick labels
    axis.text.y  = element_text(size = 12),  # KM y-axis tick labels
    legend.text  = element_text(size = 12),  # Legend labels
    legend.title = element_text(size = 12)   # Legend title
  )

library(gridExtra)

full_plot <- grid.arrange(
  combined_km$plot,
  combined_km$table,
  ncol = 1,
  heights = c(3, 1)  # adjust relative heights of plot and risk table
)

ggsave(
  filename = "km_plot__survival_with_risktable.jpeg",
  plot = full_plot,
  #width = 8, height = 8,   # adjust total size
  dpi = 300
)

