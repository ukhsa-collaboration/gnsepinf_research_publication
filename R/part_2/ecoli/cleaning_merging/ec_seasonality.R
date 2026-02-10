###paper saying that seasonality is different in hospital onset (no seasonal) vs 
##community onset (seasonal in the north of england)

pacman::p_load(
  rio,          # file import/export
  here,         # relative filepaths 
  lubridate,    # working with dates/epiweeks
  aweek,        # alternative package for working with dates/epiweeks
  incidence2,   # epicurves of linelist data
  i2extras,     # supplement to incidence2
  stringr,      # search and manipulate character strings
  forcats,      # working with factors
  RColorBrewer, # Color palettes from colorbrewer2.org
  tidyverse,     # data management + ggplot2 graphics
  cowplot
) 

source("paths/import_datasets.R")

ec_weekly_breaks <- seq.Date(from = as.Date ("2011-05-01"),
                             to = as.Date("2019-03-31"),
                             by = "weeks")
ec_monthly_breaks <- seq.Date(from = as.Date ("2011-05-01"),
                              to = as.Date("2019-03-31"),
                              by = "months")
year_breaks <- seq(
  from = as.Date("2020-01-01"),
  to = as.Date("2024-01-01"),
  by = "1 year"
)



ec_weekly_breaks_central <- seq.Date(
  from = floor_date(min(ec_final$data_collection_date, na.rm=T),   "week", week_start = 1), # Monday before first case
  to   = ceiling_date(max(ec_final$data_collection_date, na.rm=T), "week", week_start = 1), # Monday after last case
  by   = "week")    # bins are 7-days 

##EPICURVE BY WEEK ###################
curve_coli <- 
  ggplot(data = ec_final) +
  geom_histogram(
    mapping = aes(x = data_collection_date),
    breaks = ec_weekly_breaks_central,
    closed = "left",
    color = "darkblue",
    fill = "lightblue") +
  scale_x_date(
    limits = c(min(ec_final$data_collection_date), max(ec_final$data_collection_date)),  # same limits
    date_breaks = "1 year",    # major breaks at 1-year intervals
    date_minor_breaks = "1 month", # minor breaks at 1-month intervals
    date_labels = "%b\n%Y"     # consistent labels
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  theme_minimal()


###############epicurve by month################
monthcurve_coli <- ec_final %>% 
  filter(apportionment_rule_category == "Community-onset") %>% 
  ggplot() +
  geom_histogram(
    mapping = aes(x = data_collection_date),
    breaks = ec_monthly_breaks,
    closed = "left",
    color = "darkblue",
    fill = "lightblue") +
  scale_x_date (
    expand = c(0,0),
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%b\n%Y"
  ) +
  scale_y_continuous(
    expand = c(0,0)
  )+
  theme_minimal() 

death_month_ecoli <- ec_final %>%                        # begin with linelist
  group_by(month = floor_date(data_collection_date, "month")) %>%  # create month column
  
  # summarise to get monthly total deaths
  summarise(
    n_cases = n(),
    died = sum(death_90 == 1, na.rm = TRUE)   # count total deaths per week
  ) %>% 
  
  # begin plot
  ggplot() +
  
  # line of weekly total deaths
  geom_line(                                
    mapping = aes(x = month, y = died),      
    size = 1,
    color = "black") +
  
  # Align x-axis with histogram plot
  scale_x_date(
    limits = c(min(ecj$data_collection_date), max(ecj$data_collection_date)),  # same limits
    date_breaks = "1 year",    # major breaks at 1-year intervals
    date_minor_breaks = "1 month", # minor breaks at 1-month intervals
    date_labels = "%b\n%Y"     # consistent labels
  ) +
  
  # y-axis adjustments
  scale_y_continuous(                
    breaks = seq(0, max(ecj$death_90 == 1, na.rm = TRUE), 5), 
    position = "right") +
  
  labs(x = "", y = "Total deaths") +        
  theme_cowplot()

