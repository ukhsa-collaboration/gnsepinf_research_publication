##correlation between gestational age, catheter and ventilation

ec_univariate_paper %>% 
  select(gestage_grp, umbilical_central_catheter_28d_prior) %>% 
  tbl_summary(by=umbilical_central_catheter_28d_prior) %>% 
  add_p()


ec_univariate_paper %>% 
  select(gestage_grp, ventilation_28d_prior) %>% 
  tbl_summary(by=ventilation_28d_prior) %>% 
  add_p()
