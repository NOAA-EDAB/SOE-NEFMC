
# Set lat/lon window for maps
xmin = -76
xmax = -66
ymin = 40
ymax = 46
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)

biv_col <- c("4" = "#ca0020", "3" = "#f4a582", "2" = "grey", "1" = "#92c5de", "0" = "#0571b0")

eng_rel <- ecodata::eng_rel %>%
    filter(PRIMARY_LATITUDE > 40) %>% 
  dplyr::rename(comeng = ComEng_NE16_ct,
                comrel = ComRel_NE16_ct,
                receng = RecEng_NE16_ct,
                recrel = RecRel_NE16_ct,
                Latitude = PRIMARY_LATITUDE,
                Longitude = PRIMARY_LONGITUDE,
                State = STATEABBR) %>% 
  mutate(comsum = sum(as.numeric(comeng), as.numeric(comrel))) %>% 
  mutate(comeng = factor(comeng, ordered = TRUE, levels = 1:4),
         comrel = factor(comrel, ordered = TRUE, levels = 1:4),
         receng = factor(receng, ordered = TRUE, levels = 1:4),
         recrel = factor(recrel, ordered = TRUE, levels = 1:4))



(com <- eng_rel %>% 
  dplyr::select(comeng, comrel, comsum, Latitude, Longitude) %>% 
  filter(comeng != 0 &
         comrel != 0) %>% 
ggplot() +
  geom_sf(data = coast, size = map.lwd) +
  geom_sf(data  = ne_states, size = map.lwd) +
  coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  geom_point(aes(x = Longitude, y = Latitude,
                 fill = comeng, size = comrel),
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
  ggtitle("Commercial Reliance & Engagement"))

# com + rec + plot_layout(ncol = 2) &
#   theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
