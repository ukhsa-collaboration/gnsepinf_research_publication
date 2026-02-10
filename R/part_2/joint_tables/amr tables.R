library(skimr)
library(lubridate)
library(tidyr)
library(dplyr)
library(flextable)
library(ggplot2)
library(ukhsacharts)

source("paths/import_datasets.R")

#skim(amr)

#table(amr$cephalosporin)

# amr_celan <- amr %>% 
#   mutate(tested)

### as the dataset is already clean

amr %>%  
  filter(datacollection == "PSEUDOMONAS AERUGINOSA") %>% 
  filter(cephalosporin %in% c("S", "R", "I")) %>% 
  summarise(
    tested = n(),
    resistant = sum(cephalosporin == "R"),
    resistance_rate = resistant/tested *100
  )
table(amr$datacollection)


###merge dataset
ec_amr_join <- ec_all_joined %>% dplyr::select(id,
                                        year_sample,
                                        data_collection_date)
kleb_amr_join <- kleb_all_joined %>% dplyr::select(id, 
                                            year_sample,
                                            data_collection_date)
pseu_amr_join <- pseu_all_joined %>% dplyr::select(id, 
                                            year_sample,
                                            data_collection_date)

ec_amr <- left_join(ec_amr_join,
                    amr,
                    by= c("id")
)

kleb_amr <- left_join(kleb_amr_join,
                      amr,
                      by = c("id"))
pseu_amr <- left_join(pseu_amr_join,
                      amr,
                      by = c("id"))

##to make the tables faster I will just create a mega dataset with all amr data from all the pathogens

combined_amr <- bind_rows(ec_amr, kleb_amr)
combined_amr <- bind_rows(combined_amr, pseu_amr)
combined_amr <- combined_amr %>% 
  mutate(year_period = case_when(
    data_collection_date >= as.Date("2015-04-01") & data_collection_date <= "2016-03-31" ~ "2015-2016",
    data_collection_date >= as.Date("2016-04-01") & data_collection_date <= "2017-03-31" ~ "2016-2017",
    data_collection_date >= as.Date("2017-04-01") & data_collection_date <= "2018-03-31" ~ "2017-2018",
    data_collection_date >= as.Date("2018-04-01") & data_collection_date <= "2019-03-31" ~ "2018-2019",
   TRUE ~ NA_character_ 
  )) %>% 
  mutate(year_period = as.factor(year_period))


table(combined_amr$datacollection
      )
amr_long <- combined_amr %>% 
  mutate(mdr = unclass(mdr),
    mdr = case_when(
    mdr == 1 ~ "R",
    mdr == 0 ~ "S"
  )) %>% 
  pivot_longer(
    cols = c(carbapenem,
             cephalosporin,
             rPIPTAZ,
             rCIP,
             rGENT,
             rAMOXCLAV,
             mdr),
    names_to = "antibiotic",
    values_to = "result"
  )

ab_label_map <- c(
  cephalosporin = "Cephalosporin",
  carbapenem = "Carbapenem",
  rCIP = "Ciprofloxacin",
  rGENT = "Gentamicin",
  rPIPTAZ = "Piperacillin-Tazobactam",
  rAMOXCLAV = "Amoxicillin-Clavulanic Acid",
  mdr = "Multidrug Resistance"
)

desired_order <- c("Cephalosporin", "Carbapenem", "Amoxicillin-Clavulanic Acid",
                   "Piperacillin-Tazobactam", "Ciprofloxacin", "Gentamicin", "Multidrug Resistance")

amr_long <- amr_long %>%
  mutate(
    antibiotic = recode(antibiotic, !!!ab_label_map),  # rename
    antibiotic = factor(antibiotic, levels = desired_order)  # reorder
  )

summary_amr <- amr_long %>% 
  filter(result %in% c("S", "R", "I")) %>% 
  group_by(year_period, datacollection, antibiotic) %>% 
  summarise(
    tested = n(),
    resistant = sum(result == "R"),
    resistance_rate = round(resistant/tested *100, 1),
    .groups = "drop"
  ) %>% 
  mutate(datacollection = recode(datacollection,
                                 "E. COLI" = "E. coli",
                                 "KLEBSIELLA SPP" = "Klebsiella spp.",
                                 "PSEUDOMONAS AERUGINOSA" = "P. aeruginosa"))


###now for the table
table_amr <- amr_long %>% 
  filter(result %in% c("S", "R", "I")) %>% 
  group_by(datacollection, antibiotic) %>% 
  summarise(
    tested = n(),
    resistant = sum(result == "R"),
    resistance_rate = round(resistant/tested *100, 1),
    display = paste0(resistant, " (",resistance_rate, "%)"),
    .groups = "drop"
  )

res_table <- table_amr %>% 
  pivot_wider(
    id_cols = c(antibiotic),
    names_from = datacollection,
    values_from = display
  ) %>% 
  arrange(antibiotic)

amr_paper_table <- flextable(res_table) %>%
  set_header_labels(
    antibiotic = "Antibiotic resistance",
    "E. COLI" = "Escherichia coli resistant specimens (%)", 
    "KLEBSIELLA SPP" = "Klebsiella spp. resistant specimens (%)", 
    "PSEUDOMONAS AERUGINOSA" = "Pseudomonas aeruginosa resistant specimens (%)"
    # Pathogen columns will keep their names unless you rename them here
  ) %>%
  # add_header_row(
  #   x = amr_paper_table,
  #   values = c("",
  #             "Escherichia coli",
  #              "Klebsiella spp.",
  #              "Pseudomonas aeruginosa")
  # ) %>% 
  autofit() %>%
  theme_box() %>%
  bold(part = "header")

# amr_paper_table <- add_header_row(
#     x = amr_paper_table,
#     values = c("",
#               "Escherichia coli",
#                "Klebsiella spp.",
#                "Pseudomonas aeruginosa")
#    )


save_as_docx(
  "Resistance profiles of specimes of E coli (2015-2019), Klebsiella spp, and P. aeruginosa(2017-2019) from infants in England" =amr_paper_table,
  path = here("outputs", "20250701_all_amr_data.docx")
)

###now the line graph for the antibiotics over time


ggplot(summary_amr, aes(x = year_period, y = resistance_rate, color = antibiotic)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ datacollection) +
  labs(
    title = "Antibiotic Resistance Trends by Pathogen",
    x = "Year",
    y = "Resistant specimens (%)",
    color = "Antibiotic"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

amr_plot_manuscript <- ggplot(summary_amr, 
       aes(x = year_period,
           y = resistance_rate,
           color = antibiotic,
           group = antibiotic)) +
  geom_line(linewidth = 1,
            linetype = "solid",
            alpha = 1
            ) +
  geom_point(size = 3,
             shape = 16,
             alpha = 1
             ) +
  facet_wrap(~ datacollection
  )+
  labs(
    title = "",
    x = "",
    y = "Resistant specimens (%)",
    color = "Antibiotic"
  ) +
  scale_color_manual(name = "Legend",
                     values = c(
                       "Cephalosporin" = "#00AB8E",
                       "Amoxicillin-Clavulanic Acid" = "#582C83",
                       "Carbapenem" = "#84BD00",
                       "Piperacillin-Tazobactam" = "#FF7F32",
                       "Ciprofloxacin" = "#FFB81C",
                       "Gentamicin" = "#E40046", 
                       "Multidrug Resistance" = "#D5CB9F"
                     )) +
  theme_ukhsa() +
  theme(
    plot.title = element_text(face = "italic", size = 14),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    legend.title =  element_text(size = 9, face= "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(face = "italic")
  )
ggsave(
  filename = "amr_plot_manuscript.jpeg",  # file name
  plot = amr_plot_manuscript,           # your ggplot object
  #width = 8,                  # width in inches
  #height = 6,                 # height in inches
  dpi = 300                   # minimum 300 dpi
)


## table to report results in paper

table_period_amr <- amr_long %>% 
  filter(result %in% c("S", "R", "I")) %>% 
  group_by(datacollection,
           year_period,
           antibiotic) %>% 
  summarise(
    tested = n(),
    resistant = sum(result == "R"),
    resistance_rate = round(resistant/tested *100, 1),
    display = paste0(resistant, " (",resistance_rate, "%)"),
    .groups = "drop"
  )
