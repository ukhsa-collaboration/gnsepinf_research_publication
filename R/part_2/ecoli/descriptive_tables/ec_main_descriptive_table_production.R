###merging descriptive tables and making them into flex tables

border_style = officer::fp_border(color="black", width = 1)

destabl1 <- tbl_merge(
  tbls = list(ectable1a_90, ectable1a_30, ectable1a_7),
  tab_spanner = c("**Death 90d**", "**Death 30d**", "**Death 7d**")
) %>% 
  as_flex_table()

destabl1<- destabl1 %>% 
  vline(part= "all", j=1, border=border_style) %>% 
  vline(part= "all", j=6, border=border_style) %>% 
  vline(part= "all", j=11, border=border_style)
  
destabl2 <- tbl_merge(
  tbls = list(ectable2a_90, ectable2a_30, ectable2a_7),
  tab_spanner = c("**Death 90d**", "**Death 30d**", "**Death 7d**")
) %>% 
  as_flex_table()

destabl2<- destabl2 %>% 
  vline(part= "all", j=1, border=border_style) %>% 
  vline(part= "all", j=6, border=border_style) %>% 
  vline(part= "all", j=11, border=border_style)

destabl3 <- tbl_merge(
  tbls = list(ectable3a_90, ectable3a_30, ectable3a_7),
  tab_spanner = c("**Death 90d**", "**Death 30d**", "**Death 7d**")
) %>% 
  as_flex_table()

destabl3<- destabl3 %>% 
  vline(part= "all", j=1, border=border_style) %>% 
  vline(part= "all", j=6, border=border_style) %>% 
  vline(part= "all", j=11, border=border_style)


destabl4 <- tbl_merge(
  tbls = list(ectable4a_90, ectable4a_30, ectable4a_7),
  tab_spanner = c("**Death 90d**", "**Death 30d**", "**Death 7d**")
) %>% 
  as_flex_table()

destabl4<- destabl4 %>% 
  vline(part= "all", j=1, border=border_style) %>% 
  vline(part= "all", j=6, border=border_style) %>% 
  vline(part= "all", j=11, border=border_style)

destabl5 <- tbl_merge(
  tbls = list(ectable5a_90, ectable5a_30, ectable5a_7),
  tab_spanner = c("**Death 90d**", "**Death 30d**", "**Death 7d**")
) %>% 
  as_flex_table()

destabl5 <- destabl5 %>% 
  vline(part= "all", j=1, border=border_style) %>% 
  vline(part= "all", j=6, border=border_style) %>% 
  vline(part= "all", j=11, border=border_style)


destabl6 <- tbl_merge(
  tbls = list(ectable6a_90, ectable6a_30, ectable6a_7),
  tab_spanner = c("**Death 90d**", "**Death 30d**", "**Death 7d**")
) %>% 
  as_flex_table()

destabl6 <- destabl6 %>% 
  vline(part= "all", j=1, border=border_style) %>% 
  vline(part= "all", j=6, border=border_style) %>% 
  vline(part= "all", j=11, border=border_style)


save_as_html(
  "Table 1. Sociodemographic characteristics of mothers and infants with E. coli bacteremia in England 2011-2019" = destabl1,
  "Table 2a. Birth characteristics by outcome of mothers and infants with an E. coli episode in England 2011-2019" = destabl2,
  "Table 3a. Comorbidity characteristics of infants with an E. coli bacteremia episode in England 2011-2019" = destabl3,
  "Table 4a. Procedures performed on infants 28 days before their first E. coli bacteremia episode in England 2011-2019 (90d for surgeries)" = destabl4,
  "Table 5a. Procedures performed on infants 1-28 days after their first E. coli bacteremia episode in England 2011-2019" = destabl5,
  "Table 6a. Other information from mothers and infants with an E.coli bacteremia in England 2011-2019" = destabl6,
  path = here("outputs", "ec_all_descriptive_tables_90d_30d_7d.html"))



