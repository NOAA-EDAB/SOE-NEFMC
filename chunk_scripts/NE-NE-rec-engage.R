
(rec <- 
  eng_rel %>% 
  dplyr::select(receng, recrel, Latitude, Longitude) %>% 
  filter(receng != 0,
         recrel != 0) %>% 
ggplot() +
  geom_sf(data = coast, size = map.lwd) +
  geom_sf(data  = ne_states, size = map.lwd) +
  coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  geom_point(aes(x = Longitude, y = Latitude,
                 fill = receng, size = recrel),
             color = "black",pch = 21) +
  guides(fill = guide_legend(override.aes = list(size = 5),
                            reverse = T,
                            title = "Engagement"),
         size = guide_legend(reverse = T,
                            title = "Reliance")) +
  scale_fill_manual(values = biv_col) +
  theme_map() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = c(0.8, 0.25), 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.key=element_blank(),
        legend.key.width = unit(0, "cm"))+
  ggtitle("Recreational Reliance & Engagement"))
