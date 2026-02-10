## plots for the line chart on incidence rates 

##

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
  gt,
  ggplot2,
  ukhsacharts
)
source("paths/import_datasets.R")

if (!require(remotes)) install.packages("remotes")
remotes::install_git('https://gitlab.phe.gov.uk/fs-data-science-leads/ukhsacharts',
                     upgrade = "never", 
                     build_vignettes = TRUE)

##first of all we need to add a category so that we have the periods for all the cases

#Periods go from April to March of each year

ec_rates <- ec_clean %>% 
  mutate(period = case_when(
  (year_no == 2011 & month_no %in% 4:12 ) |
    (year_no==2012 & month_no %in% 1:3 ) ~ "2011/2012",
  (year_no == 2012 & month_no %in% 4:12 ) |
    (year_no==2013 & month_no %in% 1:3 ) ~ "2012/2013",
  (year_no == 2013 & month_no %in% 4:12 ) | 
    (year_no==2014 & month_no %in% 1:3 ) ~ "2013/2014",
  (year_no == 2014 & month_no %in% 4:12 ) | 
    (year_no==2015 & month_no %in% 1:3 ) ~ "2014/2015",
  (year_no == 2015 & month_no %in% 4:12 ) | 
    (year_no==2016 & month_no %in% 1:3 ) ~ "2015/2016",
  (year_no == 2016 & month_no %in% 4:12 ) | 
    (year_no==2017 & month_no %in% 1:3 ) ~ "2016/2017",
  (year_no == 2017 & month_no %in% 4:12 ) | 
    (year_no==2018 & month_no %in% 1:3 ) ~ "2017/2018",
  (year_no == 2018 & month_no %in% 4:12 ) | 
    (year_no==2019 & month_no %in% 1:3 ) ~ "2018/2019"
  )) %>% 
  group_by(period) %>% 
  tally() %>% 
  mutate(species = case_when(
    n>1 ~ "E. coli"
  ))

kleb_rates <- kleb_clean %>% 
  mutate(period = case_when(
    (year_no == 2011 & month_no %in% 4:12 ) |
      (year_no==2012 & month_no %in% 1:3 ) ~ "2011/2012",
    (year_no == 2012 & month_no %in% 4:12 ) |
      (year_no==2013 & month_no %in% 1:3 ) ~ "2012/2013",
    (year_no == 2013 & month_no %in% 4:12 ) | 
      (year_no==2014 & month_no %in% 1:3 ) ~ "2013/2014",
    (year_no == 2014 & month_no %in% 4:12 ) | 
      (year_no==2015 & month_no %in% 1:3 ) ~ "2014/2015",
    (year_no == 2015 & month_no %in% 4:12 ) | 
      (year_no==2016 & month_no %in% 1:3 ) ~ "2015/2016",
    (year_no == 2016 & month_no %in% 4:12 ) | 
      (year_no==2017 & month_no %in% 1:3 ) ~ "2016/2017",
    (year_no == 2017 & month_no %in% 4:12 ) | 
      (year_no==2018 & month_no %in% 1:3 ) ~ "2017/2018",
    (year_no == 2018 & month_no %in% 4:12 ) | 
      (year_no==2019 & month_no %in% 1:3 ) ~ "2018/2019"
  )) %>% 
  group_by(period) %>% 
  tally() %>% 
  mutate(species = case_when(
    n>1 ~ "Klebsiella spp."
  ))

pseu_rates <- pseu_clean %>% 
  mutate(period = case_when(
    (year_no == 2011 & month_no %in% 4:12 ) |
      (year_no==2012 & month_no %in% 1:3 ) ~ "2011/2012",
    (year_no == 2012 & month_no %in% 4:12 ) |
      (year_no==2013 & month_no %in% 1:3 ) ~ "2012/2013",
    (year_no == 2013 & month_no %in% 4:12 ) | 
      (year_no==2014 & month_no %in% 1:3 ) ~ "2013/2014",
    (year_no == 2014 & month_no %in% 4:12 ) | 
      (year_no==2015 & month_no %in% 1:3 ) ~ "2014/2015",
    (year_no == 2015 & month_no %in% 4:12 ) | 
      (year_no==2016 & month_no %in% 1:3 ) ~ "2015/2016",
    (year_no == 2016 & month_no %in% 4:12 ) | 
      (year_no==2017 & month_no %in% 1:3 ) ~ "2016/2017",
    (year_no == 2017 & month_no %in% 4:12 ) | 
      (year_no==2018 & month_no %in% 1:3 ) ~ "2017/2018",
    (year_no == 2018 & month_no %in% 4:12 ) | 
      (year_no==2019 & month_no %in% 1:3 ) ~ "2018/2019"
  )) %>% 
  group_by(period) %>% 
  tally() %>% 
  mutate(species = case_when(
    n>1 ~ "P. aeruginosa"
  ))

rates_all_spp <- rbind(ec_rates, kleb_rates)
rates_all_spp <- rbind(rates_all_spp, pseu_rates)


livebirths_clean <- ons_regions %>% 
  clean_names() %>% 
  pivot_longer(
    cols = 4:24,
    names_to = "year_no",
    values_to = "live_births"
  ) %>% 
  mutate_at(
    vars(live_births),
    ~ as.numeric(gsub("[^0-9.-]", "", .))
  ) %>% 
  mutate(period = case_when(
    (year_no == "x2011" & month_of_birth %in% 4:12 ) |
      (year_no== "x2012" & month_of_birth %in% 1:3 ) ~ "2011/2012",
    (year_no == "x2012" & month_of_birth %in% 4:12 ) |
      (year_no== "x2013" & month_of_birth %in% 1:3 ) ~ "2012/2013",
    (year_no == "x2013" & month_of_birth %in% 4:12 ) |
      (year_no== "x2014" & month_of_birth %in% 1:3 ) ~ "2013/2014",
    (year_no == "x2014" & month_of_birth %in% 4:12 ) |
      (year_no== "x2015" & month_of_birth %in% 1:3 ) ~ "2014/2015",
    (year_no == "x2015" & month_of_birth %in% 4:12 ) |
      (year_no== "x2016" & month_of_birth %in% 1:3 ) ~ "2015/2016",
    (year_no == "x2016" & month_of_birth %in% 4:12 ) |
      (year_no== "x2017" & month_of_birth %in% 1:3 ) ~ "2016/2017",
    (year_no == "x2017" & month_of_birth %in% 4:12 ) |
      (year_no== "x2018" & month_of_birth %in% 1:3 ) ~ "2017/2018",
    (year_no == "x2018" & month_of_birth %in% 4:12 ) |
      (year_no== "x2019" & month_of_birth %in% 1:3 ) ~ "2018/2019"
  )
  ) %>% 
  drop_na(any_of(c("period")
  )
  ) %>% 
  group_by(period) %>% 
  summarise(total_live_births = sum(live_births, na.rm = TRUE))
  
rates_table <- left_join(rates_all_spp,
                         livebirths_clean,
                         by= c("period"))

rates_table <- rates_table %>% 
  mutate(incidence = (n/total_live_births) * 100000)

timeline <- ggplot(rates_table,
       aes( x = period,
            y = incidence,
            group= species,
            color = species)) +
  geom_line(linewidth = 1,
            linetype = "solid",
            #alpha = 0.5
            ) +
  geom_point(size = 4,
             #shape = 21,
             shape = 16,
             #fill = "white",
             #stroke = 1.2,
             alpha = 1
             )+
  labs(x = "Financial year",
       y = "Incidence per 100,000 live births") +
  scale_color_manual(name = "",
                     values = c(
                       "E. coli" = "#003B5C",
                      "Klebsiella spp." = "#E40046",
                      "P. aeruginosa" = "#00AB8E"
                     )) +
  theme_ukhsa() +
  theme(axis.text.x = element_text(angle = 30,
                                   size = 10,
                                   hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        #legend.title = element_text(size = 13, face= "bold"),
        legend.text = element_text(size = 10, 
                                   face = "italic")
  )

ggsave(
  filename = "manuscript_incidence_figure.jpeg",
  plot = timeline,                # your ggplot object
  dpi = 300
  # width = 107 / 25.4,      # convert mm to inches
  # height = 80 / 25.4       # set whatever height you need (example)
)
