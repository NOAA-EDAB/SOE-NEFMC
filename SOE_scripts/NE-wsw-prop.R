## ----wsw-prop----
sw.df <- slopewater %>% 
  mutate(Var, Var = plyr::mapvalues(Var, from = c("WSW proportion ne channel",
                                                  "LSLW proportion ne channel"),
                                    to = c("WSW","LSLW"))) %>% 
  dplyr::rename(Origin  = Var) %>% 
  group_by(Origin) %>% 
  mutate(hline = mean(Value)) 

sw.df$Origin <- factor(sw.df$Origin, levels = c("WSW","LSLW"))

ggplot(data = sw.df) +
  geom_line(aes(x = Time, y = Value, color = Origin))+
  geom_point(aes(x = Time, y = Value, color = Origin)) +
  ylab("Percent of Total Slopewater") +
  ggtitle("Slopewater Proportions in NE Channel")+
    scale_x_continuous(expand = c(0.01, 0.01))+
      geom_hline(aes(yintercept = hline,
                     color = Origin),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_ts() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))
