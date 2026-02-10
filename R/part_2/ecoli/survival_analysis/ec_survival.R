##survival analysis
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
source("paths/import_datasets.R")

#first we need to create a survival object
summary(ec_univariate3$diff_time_death_bact)

##first I prepare the dataset
ecsurv <- ec_univariate3 %>% 
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

tabyl(ecsurv$futime)
ecsurv %>% dplyr::select(futime) %>% head(50)


survobj <- Surv(time = ecsurv$futime,
                event = ecsurv$death_90) 

head(survobj, 50)
linelistsurv_fit <-  survival::survfit(survobj ~ 1,
                                       data = ecsurv)
summary(linelistsurv_fit)
##print summary at specific times

plot(linelistsurv_fit, 
     xlab = "Days of follow-up",    # x-axis label
     ylab="Survival Probability",   # y-axis label
     main= "Overall survival curve" # figure title
)

### now we create a plot for the risk factors
# Create the survival object for sex
km.sex <- survival::survfit(survobj ~ sex, data = ecsurv)

# Kaplan-Meier curve stratified by 'sex' variable (base R)
plot(km.sex, 
     main = "Kaplan-Meier Curve for E.coli sepsis death by gender",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("Female", "Male"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ sex, data = ecsurv)


### now for ETHNICITY

# Create the survival object for sex
km.eth <- survival::survfit(survobj ~ ethnicity_des2, data = ecsurv)

plot(km.eth, 
     main = "Kaplan-Meier Curve for E. coli sepsis death by ethnicity",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red", "green", "purple", "orange", "black"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("White", "Black", "South Asian", "Asian other", "Mixed", "other ethnicity"), 
       col = c("blue", "red", "green", "purple", "orange", "black"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ ethnicity_des2, data = ecsurv)

### now for MATERNAL AGE GROUP

# Create the survival object for sex
km.matage <- survival::survfit(survobj ~ matage_grp, data = ecsurv)

plot(km.matage, 
     main = "Kaplan-Meier Curve for E coli death by Maternal age",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red", "green", "purple"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("20-29 years","<20 years",  "30-39 years", "40+ years"), 
       col = c("blue", "red", "green", "purple"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ matage_grp, data = ecsurv)


##BIRTHWEIGHT

km.btwg <- survival::survfit(survobj ~ birthwgt_grp, data = ecsurv)

plot(km.btwg, 
     main = "Kaplan-Meier Curve for E coli death by Birthweight group",
     xlab = "Time (days)", 
     ylab = "Probability of death",
     col = c("blue", "red", "green", "purple"
     ), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c( "Normal",  "Low", "Very Low", "Extremely Low"), 
       col = c("blue", "red", "green", "purple"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ birthwgt_grp, data = ecsurv)
##this one is significant - risk factor



##GESTATIONAL AGE
km.gestage <- survival::survfit(survobj ~ gestage_grp, data = ecsurv)

plot(km.gestage, 
     main = "Kaplan-Meier Curve for E coli death by Gestational age",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red", "green", "purple"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("Term", "Moderate Pre-term", "Very preterm", "Extremely preterm"), 
       col = c("blue", "red", "green", "purple"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ gestage_grp, data = ecsurv)
##significant risk factor

##MULTIBIRTH
km.twin <- survival::survfit(survobj ~ multibirth_bi, data = ecsurv)

plot(km.twin, 
     main = "Kaplan-Meier Curve for E coli death by type of birth",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("Single", "Twins (+)"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ multibirth_bi, data = ecsurv)
##significant risk factor
##risk factor

##apportionment rule cat
km.app <- survival::survfit(survobj ~ apportionment_rule_category, data = ecsurv)

plot(km.app, 
     main = "Kaplan-Meier Curve for E coli death by apportionment category",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("Community", "Hospital"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ apportionment_rule_category, data = ecsurv)
##significant risk factor
##risk factor

##ONSET TYPE
km.onset <- survival::survfit(survobj ~ sample_onset, data = ecsurv)

plot(km.onset, 
     main = "Kaplan-Meier Curve for E coli death by onset type",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("LOS","EOS"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ sample_onset, data = ecsurv)
##significant risk factor
##risk factor

##DELIVERY CAT
km.delivery <- survival::survfit(survobj ~ delivery_cat, data = ecsurv)

plot(km.delivery, 
     main = "Kaplan-Meier Curve for E coli death by delivery type",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red", "green"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("Vaginal", "c-section", "manipulation/breech/other"), 
       col = c("blue", "red", "green"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ delivery_cat, data = ecsurv)
##significant risk factor


##jaundice
km.jaun <- survival::survfit(survobj ~ jaundice, data = ecsurv)

plot(km.jaun, 
     main = "Kaplan-Meier Curve for E coli death by jaundice",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ jaundice, data = ecsurv)
##significant risk factor
##risk factor

##hemato
km.hem <- survival::survfit(survobj ~ hemato, data = ecsurv)

plot(km.hem, 
     main = "Kaplan-Meier Curve for E coli death by hematological comorbidities",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ hemato, data = ecsurv)
##significant risk factor
##risk factor

##endo nutri metabolic comorbidity

km.met <- survival::survfit(survobj ~ endo_nutri_meta, data = ecsurv)

plot(km.met, 
     main = "Kaplan-Meier Curve for E coli death by endo/nutri/meta comorbidities",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ hemato, data = ecsurv)
##significant risk factor
##risk factor


##accident injury

km.acc <- survival::survfit(survobj ~ accident_injury, data = ecsurv)

plot(km.acc, 
     main = "Kaplan-Meier Curve for E coli death by accident/injury",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ accident_injury, data = ecsurv)
##significant risk factor
##risk factor

##congenital

km.con <- survival::survfit(survobj ~ congenital, data = ecsurv)

plot(km.con, 
     main = "Kaplan-Meier Curve for E coli death by congenital conditions",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ congenital, data = ecsurv)
##significant risk factor

###PERINATAL OR OTHER

km.peri <- survival::survfit(survobj ~ perinatal_other, data = ecsurv)

plot(km.peri, 
     main = "Kaplan-Meier Curve for E coli death (other perinatal conditions",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ perinatal_other, data = ecsurv)
##significant risk factor


###birth complications

km.com <- survival::survfit(survobj ~ pregnancy_complication_trauma, data = ecsurv)

plot(km.com, 
     main = "Kaplan-Meier Curve for E coli death (pregnancy complications/trauma)",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ perinat_resp_cardio, data = ecsurv)
##significant risk factor

###resp cardio perinatal

km.periresp <- survival::survfit(survobj ~ perinat_resp_cardio, data = ecsurv)

plot(km.periresp, 
     main = "Kaplan-Meier Curve for E coli death (perinatal respiratory or cardio conditions)",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ perinat_resp_cardio, data = ecsurv)
##significant risk factor


###genitourinary

km.geni <- survival::survfit(survobj ~ genitourinary, data = ecsurv)

plot(km.geni, 
     main = "Kaplan-Meier Curve for E coli death (genitourinary complications)",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ genitourinary, data = ecsurv)
##significant risk factor

###skin

km.skin <- survival::survfit(survobj ~ skin, data = ecsurv)

plot(km.skin, 
     main = "Kaplan-Meier Curve for E coli death (skin conditions)",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ skin, data = ecsurv)
##significant risk factor

###digestive

km.dig <- survival::survfit(survobj ~ digestive, data = ecsurv)

plot(km.dig, 
     main = "Kaplan-Meier Curve for E coli death (digestive conditions)",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ digestive, data = ecsurv)
##significant risk factor

###circulatory

km.circ <- survival::survfit(survobj ~ circulatory, data = ecsurv)

plot(km.circ, 
     main = "Kaplan-Meier Curve for E coli death (circulatory conditions)",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ circulatory, data = ecsurv)
##significant risk factor

###other infections

km.inf <- survival::survfit(survobj ~ other_infectious, data = ecsurv)

plot(km.inf, 
     main = "Kaplan-Meier Curve for E coli death (other infections)",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ other_infectious, data = ecsurv)
##significant risk factor

###catheter pre-

km.pcath <- survival::survfit(survobj ~ umbilical_central_catheter_28d_prior, data = ecsurv)

plot(km.pcath, 
     main = "Kaplan-Meier Curve for E coli death (umb/centr catheter 28d prior)",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ umbilical_central_catheter_28d_prior, data = ecsurv)
##significant risk factor

###catheter post-

km.cathp <- survival::survfit(survobj ~ umbilical_central_catheter_90d_post, data = ecsurv)

plot(km.cathp, 
     main = "Kaplan-Meier Curve for E coli death (umb/centr catheter 90d post)",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ umbilical_central_catheter_90d_post, data = ecsurv)
##significant risk factor

###ventilation prior

km.pven <- survival::survfit(survobj ~ ventilation_28d_prior, data = ecsurv)

plot(km.pven, 
     main = "Kaplan-Meier Curve for E coli death (inv ventilation 28d prior)",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ ventilation_28d_prior, data = ecsurv)
##significant risk factor

###ventilation post-

km.venp <- survival::survfit(survobj ~ ventilation_90d_post, data = ecsurv)

plot(km.venp, 
     main = "Kaplan-Meier Curve for E coli death (inv ventilation 90d post)",
     xlab = "Time (days)", 
     ylab = "Probability of Death",
     col = c("blue", "red"), # Different colors for each group
     lwd = 1,                # Line visibility
     conf.int = TRUE)        # Show 95%CI
# Add a legend
legend("bottomleft", 
       legend = c("no","yes"), 
       col = c("blue", "red"), 
       lwd = 1)
# checking for the difference
survival::survdiff(survobj ~ ventilation_90d_post, data = ecsurv)
##significant risk factor