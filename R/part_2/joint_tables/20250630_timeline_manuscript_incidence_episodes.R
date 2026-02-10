### incidence plot but with the values in the manuscript
##number of episodes per year period per species

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

period <- c("2011/2012",
            "2012/2013",
            "2013/2014",
            "2014/2015",
            "2015/2016",
            "2016/2017",
            "2017/2018",
            "2018/2019",
            "2017/2018",
            "2018/2019",
            "2017/2018",
            "2018/2019")
species <- c("E. coli",
             "E. coli",
             "E. coli",
             "E. coli",
             "E. coli",
             "E. coli",
             "E. coli",
             "E. coli",
             "Klebsiella spp.",
             "Klebsiella spp.",
             "P. aeruginosa",
             "P. aeruginosa"
             )
incidence <- c(63.4,
               74.5,
               83.7,
               86.8,
               88.2, 
               97.0,
               89.5,
               94.0,
               24.1,
               26.5,
               7.7,
               7.5
               )

paper_chart <- data.frame(period, species, incidence)



timeline_paper <- ggplot(paper_chart,
                   aes( x = period,
                        y = incidence,
                        group= species,
                        color = species)) +
  geom_line() +
  geom_point()+
  labs(x = "",
       y = "Episodes per 100,000 live births") +
  scale_color_manual(name = "Legend",
                     values = c(
                       "E. coli" = "#003B5C",
                       "Klebsiella spp." = "#E40046",
                       "P. aeruginosa" = "#00AB8E"
                     )) +
  theme_ukhsa() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title = element_text(size = 9),
        legend.title = element_text(size = 9, face= "bold"))
