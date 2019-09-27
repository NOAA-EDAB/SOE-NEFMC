## ----MA-inshore----
mass <- mass_inshore_survey %>% 
  filter(str_detect(Var, "Index")) %>% 
  mutate(feeding.guild = str_extract(Var, paste(feeding.guilds, collapse = "|")),
         Season = str_extract(Var, "Spring|Fall")) %>%
  filter(!is.na(feeding.guild)) %>% 
  unite(.,Var, c("feeding.guild","Season"),sep = " ") %>% 
  group_by(Var) %>% 
  mutate(hline = mean(Value))
mass$Var <- factor(mass$Var,levels = c("Piscivore Fall",
                                                   "Piscivore Spring",
                                                    "Benthivore Fall",
                                                    "Benthivore Spring",
                                                    "Planktivore Fall",
                                                    "Planktivore Spring",
                                                    "Benthos Fall",
                                                    "Benthos Spring"))

mass %>% 
  ggplot(aes(x = Time, y = Value, color = Var)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Value,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  geom_line(size = lwd-0.5) +
  geom_point(size = pcex-0.5) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  facet_wrap(Var~.,scales = "free_y", ncol = 2) +
  
  #Axis and theme
  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ylab(expression("Biomass (kg tow"^-1*")")) +
  ggtitle("MA Inshore BTS") +
  theme_facet()+
  theme(strip.text=element_text(hjust=0))
