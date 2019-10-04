
sli <- ecodata::zoo_anom_sli %>% 
  filter(EPU %in% c("GOM","GB"),
         str_detect(Var, "small-large")) %>% 
  mutate(hline = 0) 
sli$EPU <- factor(sli$EPU, levels = c("GOM","GB"))

pp_anom <- ecodata::chl_pp %>% 
  filter(str_detect(Var, "ANNUAL_PPD_RATIO_ANOMALY"),
         EPU %in% c("GOM","GB")) %>% 
  mutate(hline = 1,
         Time = as.numeric(as.character(Time)),
         Var = "ANNUAL_PPD_RATIO_ANOMALY")
pp_anom$EPU <- factor(pp_anom$EPU, levels = c("GOM","GB"))

sli_plt <- sli %>% 
  ggplot(aes(x = Time, y = Value, group = Var)) +
         annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line() +
  geom_point() +
  guides(color = F) +
  facet_wrap(EPU~.,ncol = 2)+
  xlab("")+
  ylab("Small-large abundance") +
  ggtitle("Small-large copepod abundance") +
    scale_x_continuous(expand = c(0.01, 0.01), limits = c(1988, 2018))+
      geom_hline(aes(yintercept = hline,
                     group = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0))

pp_anom_plt <- pp_anom %>% 
    ggplot(aes(x = Time, y = Value, group = Var)) +
         annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line() +
  geom_point() +
  guides(color = F) +
  ylab("Anomaly ratio") +
  facet_wrap(EPU~.,ncol = 2)+
  ggtitle("Primary production anomaly ratio") +
    scale_x_continuous(expand = c(0.01, 0.01), limits = c(1988, 2018))+
    scale_y_continuous(limits = c(0.6,1.4)) +
      geom_hline(aes(yintercept = hline,
                     group = Var),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0))


sli_plt + pp_anom_plt + plot_layout(ncol = 1) & theme(plot.margin = margin(t = 0))
