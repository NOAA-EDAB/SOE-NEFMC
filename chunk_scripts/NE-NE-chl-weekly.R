
interp_chl_pp <- function(epu, year = 2018, Variable){
  out <- ecodata::chl_pp %>% 
    filter(str_detect(Var,Variable),
           EPU == epu) %>% 
    separate(.,Time, c("Year","Week"),sep = 4) %>% 
    filter(Year == year) %>% 
    group_by(EPU) %>% 
    mutate(Time = 1:length(Year))
  
  ltm_out <- ecodata::chl_pp %>% 
    filter(str_detect(Var,Variable),
           EPU == epu) %>% 
    separate(.,Time, c("Year","Week"),sep = 4) %>% 
    group_by(Week) %>% 
    dplyr::summarise(LTM = mean(Value, na.rm = T),
                     SD = sd(Value, na.rm = T)) %>% 
    mutate(Time = 1:length(Week),
           sd.low = LTM - SD,
           sd.high = LTM + SD) %>% 
    left_join(.,out, by = c("Time")) %>% 
    mutate(status = ifelse(Value < sd.high & Value > sd.low, "near_mean",
                           ifelse(Value > sd.high, "high",
                                  ifelse(Value < sd.low,"low",NA))),
           group = "PLOT")
  
  return(ltm_out)
}

GB_chl_weekly <- interp_chl_pp(epu = "GB", year = 2018,Variable = "WEEKLY_CHLOR_A_MEDIAN")

GB_chl <- ggplot(data = GB_chl_weekly) +
  geom_line(aes(x = Time, y = LTM)) +
  geom_ribbon(aes(x = Time, ymin = pmax(sd.low,0), ymax = sd.high), 
              alpha = 0.1,
              fill = "grey1") +
  geom_line(aes(x = Time, y = Value),
            size = 1,color = "#33a02c") +
  ggtitle(expression("GB chlorophyll"~italic(a)~"")) +
  guides(color = F) +
  xlab("")+
  ylab("") +
  scale_x_continuous(breaks = seq(1,52,10),
                   labels = c("Jan.","Mar.","May","July","Oct.","Dec."),
                   expand = c(0.01,0.01)) +
  scale_color_manual(values = c("#ef8a62","#2c7fb8","#a1d99b"))+
  theme_ts()


GOM_chl_weekly <- interp_chl_pp(epu = "GOM", year = 2018,Variable = "WEEKLY_CHLOR_A_MEDIAN")

GOM_chl <- ggplot(data = GOM_chl_weekly) +
  geom_line(aes(x = Time, y = LTM)) +
  geom_ribbon(aes(x = Time, ymin = sd.low, ymax = sd.high), 
              alpha = 0.1,
              fill = "grey1") +
  geom_line(aes(x = Time, y = Value),
            size = 1,color = "#33a02c") +
  ggtitle(expression("GOM chlorophyll"~italic(a)~"")) +
  guides(color = F) +
  xlab("")+
  ylab(expression("CHL (mg m"^-3*")")) +
  scale_x_continuous(breaks = seq(1,52,10),
                   labels = c("Jan.","Mar.","May","July","Oct.","Dec."),
                   expand = c(0.01,0.01)) +
  scale_color_manual(values = c("#ef8a62","#2c7fb8","#a1d99b"))+
  theme_ts()

GB_ppd_weekly <- interp_chl_pp(epu = "GB",  year = 2018,Variable = "WEEKLY_PPD_MEDIAN")

GB_ppd <- ggplot(data = GB_ppd_weekly) +
  geom_line(aes(x = Time, y = LTM)) +
  geom_ribbon(aes(x = Time, ymin = pmax(sd.low,0), ymax = sd.high), 
              alpha = 0.1,
              fill = "grey1") +
  geom_line(aes(x = Time, y = Value),
            size = 1,color = "#33a02c") +
  ggtitle(expression("GB primary production")) +
  guides(color = F) +
  xlab("")+
  ylab("") +
  scale_x_continuous(breaks = seq(1,52,10),
                   labels = c("Jan.","Mar.","May","July","Oct.","Dec."),
                   expand = c(0.01,0.01)) +
  scale_color_manual(values = c("#ef8a62","#2c7fb8","#a1d99b"))+
  theme_ts()


GOM_ppd_weekly <- interp_chl_pp(epu = "GOM",  year = 2018,Variable = "WEEKLY_PPD_MEDIAN")

GOM_ppd <- ggplot(data = GOM_ppd_weekly) +
  geom_line(aes(x = Time, y = LTM)) +
  geom_ribbon(aes(x = Time, ymin = sd.low, ymax = sd.high), 
              alpha = 0.1,
              fill = "grey1") +
  geom_line(aes(x = Time, y = Value),
            size = 1,color = "#33a02c") +
  guides(color = F) +
  xlab("")+
  ggtitle(expression("GOM primary production")) +
  scale_x_continuous(breaks = seq(1,52,10),
                   labels = c("Jan.","Mar.","May","July","Oct.","Dec."),
                   expand = c(0.01,0.01)) +
  ylab((expression("PP (gC m"^-2*" d"^-1*")"))) +
  scale_color_manual(values = c("#ef8a62","#2c7fb8","#a1d99b"))+
  theme_ts()


#GOM_chl + GB_chl + GOM_ppd + GB_ppd + plot_layout(ncol = 2) & theme(plot.margin = margin(r = 0.5))
GOM_chl + GB_chl + plot_layout(ncol = 2) & theme(plot.margin = margin(r = 0.5))
