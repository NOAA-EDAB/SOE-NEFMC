
#Get data for plotting

#Managed landings
managed_landings <- ecodata::comdat  %>%
  filter(str_detect(Var, paste0(council_abbr," managed species - Landings weight|JOINT managed species - Landings weight")),
         !str_detect(Var, "Other"),
         Time >= 1986,
         EPU %in% epu_abbr)

#Total landings
total_landings <- ecodata::comdat  %>%
  filter(!str_detect(Var, "managed species"),
         !str_detect(Var, "Other"),
         str_detect(Var, "Landings"),
         Time >= 1986,
         EPU %in% epu_abbr)

#Assign feeding guild column for plotting with ggplot
landings <- rbind(managed_landings, total_landings) %>%
  mutate(feeding.guild = str_extract(Var,paste(feeding.guilds, collapse = "|")),
         grouping = factor(ifelse(str_detect(Var,council_abbr), "managed",
                                  ifelse(str_detect(Var, "JOINT"), "joint","total"))),
         Var = paste(word(feeding.guild), grouping)) %>% 
  mutate(feeding.guild = factor(feeding.guild, levels = feeding.guilds))

#Add JOINT landings to MANAGED landings and remove
landings[landings$Var ==  "Piscivore managed" & landings$EPU == "GOM",]$Value <- landings[landings$Var ==  "Piscivore managed" & landings$EPU == "GOM",]$Value + landings[landings$Var ==  "Piscivore joint" & landings$EPU == "GOM",]$Value

landings[landings$Var ==  "Piscivore managed" & landings$EPU == "GB",]$Value <- landings[landings$Var ==  "Piscivore managed" & landings$EPU == "GB",]$Value + landings[landings$Var ==  "Piscivore joint" & landings$EPU == "GB",]$Value

landings <- landings %>%
  filter(Var != "Piscivore joint")  %>% 
  group_by(Var, EPU) %>% 
  mutate(hline = mean(Value))
  
series.col <- c("indianred","black")

#Create dataframe for label locations
label_loc <- landings %>%
  group_by(feeding.guild) %>%
  dplyr::summarise(yloc = max(Value)*0.95,
                   xloc = min(Time)) %>% 
  mutate(label = LETTERS[1:5])

#facet names for titles
facet_names <- list("Apex predators" = expression("Apex predators"),
                    "Piscivores" = expression("Piscivores"),
                    "Planktivores" = expression("Planktivores"),
                    "Benthivores" = expression("Benthivores"),
                    "Benthos" = expression("Benthos"))

gom_landings <- landings %>% filter(EPU == "GOM") %>% 
  ggplot(aes(x = Time, y = Value, color = grouping)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Value,
               group = Var)) +
  
  #Add time series
  geom_line(size = lwd) +
  geom_point(size = pcex) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 color = grouping,
                 size = grouping),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  facet_wrap(feeding.guild~.,scales = "free_y", labeller = label, ncol = 1) +
  
  #Axis and theme
  scale_y_continuous(labels = function(l){trans = l / 1000})+
  scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  ylab(expression("Landings, 10"^3*"metric tons")) +
  theme_facet() +
  theme(strip.text=element_text(hjust=0)) +
  ggtitle("Gulf of Maine")

gb_landings <- landings %>% filter(EPU == "GB") %>% 
  ggplot(aes(x = Time, y = Value, color = grouping)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Value,
               group = Var)) +
  
  #Add time series
  geom_line(size = lwd) +
  geom_point(size = pcex) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 color = grouping,
                 size = grouping),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+

  #Facet 
  facet_wrap(feeding.guild~.,scales = "free_y", labeller = label, ncol = 1) +
  
  #Axis and theme
  scale_y_continuous(labels = function(l){trans = l / 1000})+
  scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  ylab(expression("Landings, 10"^3*"metric tons")) +
  theme_facet() +
  theme(strip.text=element_text(hjust=0)) +
  ggtitle("Georges Bank")

cowplot::plot_grid(gom_landings, gb_landings, ncol = 2)
