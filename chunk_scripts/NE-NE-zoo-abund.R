
zoo_abund <- ecodata::zoo_anom_sli %>% 
  filter(EPU %in% c("GOM","GB"),
         !str_detect(Var, "small-large")) %>% 
  mutate(hline = 0,
         Var = str_to_title(str_remove(Var, "anomaly"))) 

gom_zoo_abund <- zoo_abund %>% 
  filter(EPU == "GOM") %>% 
  ggplot(aes(x = Time, y = Value, group = Var)) +
         annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls() +
  geom_line() +
  geom_point() +
  ylab("Abundance anomaly") +
  ggtitle("GOM Zooplankton abundance anomaly") +
  facet_wrap(Var~., ncol = 3, scales = "free_y") +
  scale_x_continuous(expand = c(0.01, 0.01))+
      geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))

gb_zoo_abund <- zoo_abund %>% 
  filter(EPU == "GB") %>% 
  ggplot(aes(x = Time, y = Value, group = Var)) +
         annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls() +
  geom_line() +
  geom_point() +
  ylab("Abundance anomaly") +
  ggtitle("GB Zooplankton abundance anomaly") +
  facet_wrap(Var~., ncol = 3, scales = "free_y") +
  scale_x_continuous(expand = c(0.01, 0.01))+
      geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))
  
gom_zoo_abund + gb_zoo_abund + plot_layout(ncol = 1)
