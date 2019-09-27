## ----species-groupings----
# new table with all species listed by management entity
df <- ecodata::species_groupings %>%
  dplyr::select(SOE_18, COMNAME, Fed_Managed) %>%
  filter(SOE_18 != "Other") %>%
  distinct() %>%
  group_by(SOE_18, Fed_Managed) %>%
  summarize_all(funs(paste(na.omit(.), collapse = ", "))) %>%
  spread(Fed_Managed, COMNAME) %>%
  arrange(factor(SOE_18, levels = c("Apex Predator", "Piscivore", "Planktivore", "Benthivore", "Benthos")))
df<-df[-6,c(1,4,3,5,6)] %>%
  mutate_all(tolower)


knitr::kable(df, booktabs = TRUE, caption = 'Feeding guilds and management bodies.', 
             col.names = c("Guild", "MAFMC", "Joint", "NEFMC", "State or Other")) %>%
  kable_styling(font_size=10, latex_options=c("repeat_header", "scale_down", "hold_position")) %>%
  row_spec(0,bold=TRUE) %>%
  column_spec(1, width="2cm") %>%
  column_spec(2, width="4cm") %>%
  column_spec(3, width="2cm") %>%
  column_spec(4, width="5cm") %>%
  column_spec(5, width="6cm") %>%
  #column_spec(3, width="7.5cm") #%>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle")
