pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,     # converting tables to pretty images
  lme4,          #calculation of ICC for cluster assessment
  performance,
  ggplot2
)
source("paths/import_datasets.R")


#quick exploration of clustering of cases
ec_univariate %>% 
  count(hospital_site_name, laboratory_where_specimen_processed) %>% 
  arrange(desc(n))

#working out clustering ICC values for hospital
# Fit the null model
ec_model <- glmer(death_90 ~ 1 + (1 | hospital_site_code), 
                  data = ec_univariate3, 
                  family = binomial(link = "logit"))

# Extract variance components
ec_variance_components <- VarCorr(ec_model)

# Hospital-level variance
ec_hospital_variance <- as.numeric(ec_variance_components$hospital_site_code[1])

# Residual variance for logistic regression
ec_residual_variance <- (pi^2) / 3  # Fixed value for logistic models

# Calculate ICC
ec_icc <- ec_hospital_variance / (ec_hospital_variance + ec_residual_variance)
print(ec_icc)

performance::icc(ec_model)

##RESULTS: ICC=0.1215908 WHICH INDICATES MODERATE CLUSTERING 
##BY HOSPITAL OF DEATH OUTCOMES

##CLUSTERING BY TRUST

ect_model <- glmer(death_90 ~ 1 + (1 | trust_provider_code), 
                   data = ec_univariate3, 
                   family = binomial(link = "logit"))

# Extract variance components
ect_variance_components <- VarCorr(ect_model)

# Hospital-level variance
ect_hospital_variance <- as.numeric(ect_variance_components$trust_provider_code[1])

# Residual variance for logistic regression
ect_residual_variance <- (pi^2) / 3  # Fixed value for logistic models

# Calculate ICC
ect_icc <- ect_hospital_variance / (ect_hospital_variance + ect_residual_variance)
print(ect_icc)

performance::icc(ect_model)

##MODERATE CLUSTERING WITH AN ICC OF 0.135847


##CLUSTERING FOR phe_region_reporting
ecr_model <- glmer(death_90 ~ 1 + (1 | phe_region_reporting), 
                   data = ec_univariate, 
                   family = binomial(link = "logit"))

# Extract variance components
ecr_variance_components <- VarCorr(ecr_model)

# Hospital-level variance
ecr_hospital_variance <- as.numeric(ecr_variance_components$phe_region_reporting[1])

# Residual variance for logistic regression
ecr_residual_variance <- (pi^2) / 3  # Fixed value for logistic models

# Calculate ICC
ecr_icc <- ecr_hospital_variance / (ecr_hospital_variance + ecr_residual_variance)
print(ecr_icc)

performance::icc(ecr_model)

##minimal icc = 0.002167503 for the outcome of interest


##CLUSTERING FOR phe_centre_reporting
ecr_model <- glmer(death_90 ~ 1 + (1 | phe_centre_reporting), 
                   data = ec_univariate, 
                   family = binomial(link = "logit"))

# Extract variance components
ecr_variance_components <- VarCorr(ecr_model)

# Hospital-level variance
ecr_centre_variance <- as.numeric(ecr_variance_components$phe_centre_reporting[1])

# Residual variance for logistic regression
ecr_residual_variance <- (pi^2) / 3  # Fixed value for logistic models

# Calculate ICC
ecr_icc <- ecr_centre_variance / (ecr_centre_variance + ecr_residual_variance)
print(ecr_icc)


##Result is ICC = 0.0043