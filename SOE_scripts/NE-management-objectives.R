## ----management-objectives----
mng_obj <- data.frame("Objective Categories" = c("Seafood Production",
                                                 "Profits","Recreation",
                                                 "Stability","Social & Cultural",
                                                 "Biomass","Productivity",
                                                 "Trophic structure","Habitat"),
                      "Indicators reported here" = c("Landings by feeding guild",
                                                     "Revenue by feeding guild",
                                                     "Number of anglers and trips; recreational catch",
                                                     "Diversity indices (fishery and species)",
                                                     "Commercial and recreational reliance",
                                                     "Biomass or abundance by feeding guild from surveys",
                                                     "Condition and recruitment of MAFMC managed species",
                                                     "Relative biomass of feeding guilds, primary productivity",
                                                     "Estuarine and offshore habitat conditions"))

knitr::kable(mng_obj,
      col.names = c("Objective Categories","Indicators reported here"),
      caption = "Established ecosystem-scale objectives in New England",
      #align = aca,
      booktabs = T) %>%
  kable_styling(latex_options = "hold_position","scale_down") %>%
 # column_spec(c(2), width = c("25em")) %>%
  row_spec(0, bold = TRUE)
