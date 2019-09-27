## ----harbor-porpoise-bars----
ecodata::harborporpoise %>% 
  spread(.,Var,Value) %>% 
ggplot() +
    annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line(aes(x = Time, y = `harbor porpoise bycatch estimate`, color = "Harbor Porpoise Bycatch"), size = lwd) +
  geom_point(aes(x = Time, y = `harbor porpoise bycatch estimate`, color = "Harbor Porpoise Bycatch"), size = pcex) +
  geom_errorbar(aes(x = Time,
                    ymin = `harbor porpoise bycatch lo95ci`,
                  ymax = `harbor porpoise bycatch up95ci`,
                  color = "Harbor Porpoise Bycatch"), 
                width = 0.25)+
  geom_line(aes(x = Time, y = `harbor porpoise bycatch pbr`, color = "PBR"), size = lwd-0.1) +
  scale_color_manual("", values = c("Harbor Porpoise Bycatch" = "black", "PBR" = "red")) +
  guides(color = guide_legend(override.aes = list(shape = c(19,NA)))) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  ggtitle("Harbor porpoise bycatch") +
  ylab("Bycatch Estimate (n)") +
  theme_ts() +
  theme(legend.position = c(0.4, 0.85), legend.background = element_rect(
    fill = "transparent"), legend.text = element_text(size = 10))
