
aggregate_prod <- ecodata::common_tern %>% 
    filter(!str_detect(Var, "Diet|Sum"))  %>% 
  mutate(Island = word(Var, 1),
         Var = word(Var, 3),
         Island = plyr::mapvalues(Island, 
                                  from = c("EER","JI","MR","OGI","PINWR","SINWR","STI"),
                                  to = c("Eastern Egg Rock", "Jenny Island", 
                                         "Matinicus Rock", "Outer Green Island", 
                                         "Pond Island", "Seal Island","Stratton Island"))) %>%
  group_by(Time) %>% 
  dplyr::summarise(Mean = mean(Value, na.rm = T),
                   SE = sd(Value, na.rm = T)/sqrt(n()),
                   SD = sd(Value, na.rm = T),
                   n = n()) %>% 
  mutate(Mean = ifelse(is.na(SE),NA,Mean),
         se.low = Mean - SE,
         se.high = Mean + SE,
         hline = mean(Mean, na.rm = T))

prodplot <- aggregate_prod %>% ggplot() +
#Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = Mean), size = lwd-0.75) +
  geom_point(aes(x = Time, y = Mean), size = pcex-0.75) +
  geom_gls(aes(x = Time, y = Mean)) +
  geom_errorbar(aes(x = Time,
                    ymin = se.low,
                  ymax = se.high), 
                width = 0.25) +
  scale_x_continuous(expand = c(0.01, 0.01),limits = c(1991,2018)) +
  guides(color = FALSE) +
  ggtitle("Common tern productivity") +
  ylab(expression("Fledged chicks per nest")) +
  xlab("Time")+
  geom_hline(aes(yintercept = hline),
           color = "black",
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  labs(tag = "a")  +
  theme_ts()

diet_div <- ecodata::common_tern %>% 
  filter(str_detect(Var, "Diet"),
         !str_detect(Var, "Sum")) %>% 
  mutate(Island = word(Var, 1),
         Var = word(Var, 4)) %>% 
  group_by(Island, Time) %>%
  dplyr::summarise(evenness = diversity(Value)/log(specnumber(Value)),
                   shannon = diversity(Value),
                   simpson = diversity(Value, index = "simpson")) %>% 
  gather(.,Var,Value,-Island, -Time) %>% 
  group_by(Var, Time) %>%
  dplyr::summarize(Value = mean(Value, na.rm = T),
                   sd = sd(Value, na.rm = T),
                   n = n()) %>%
  group_by(Var) %>% 
  mutate(hline = mean(Value, na.rm = T))

# evenness <- diet_div %>% 
#   filter(Var == "evenness") %>% 
# ggplot(aes(x = Time, y = Value)) +
#       annotate("rect", fill = shade.fill, alpha = shade.alpha,
#       xmin = x.shade.min , xmax = x.shade.max,
#       ymin = -Inf, ymax = Inf) +
#   geom_line() +
#   geom_point() +
#  # geom_gls() +
#   scale_x_continuous(expand = c(0.01, 0.01),limits = c(1992,2018)) +
#   ggtitle("Evenness")+
#   ylab(expression("Evenness")) +
#   xlab("")+
#   geom_hline(aes(yintercept = hline),
#            size = hline.size,
#            alpha = hline.alpha,
#            linetype = hline.lty) +
#   theme_ts() 

shannon <- diet_div %>% 
  filter(Var == "shannon") %>% 
ggplot(aes(x = Time, y = Value)) +
      annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line() +
  geom_point() +
  #geom_gls() +
  scale_x_continuous(expand = c(0.01, 0.01),limits = c(1992,2018)) +
  ggtitle("Common tern diet diversity")+
  ylab(expression("Shannon Diversity")) +
  xlab("")+
  geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  labs(tag = "b")  +
  theme_ts() 

prodplot + shannon + plot_layout(ncol = 2, widths=c(4,4))
