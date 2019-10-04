
#fig.subcap= c(aKey to figures.a, aThe Northeast Large Marine Ecosystem.a), out.width = a.49\\linewidtha, fig.show="hold" 
# FIgure orientation subfigure
m <- 0.1
x <- 1985:2018
y <-  m*x + rnorm(30, sd = 0.35)

data <- data.frame(x = x,
                  y = y)

#Define constants for figure plot
x.shade.max <- max(x)
x.shade.min <- x.shade.max - 9 
hline = mean(y)

#Plot series with trend 
psample <- ggplot2::ggplot(data = data,aes(x = x, y = y)) +
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_point(size = pcex) +
  scale_color_manual(aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline),
              size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  geom_line() +
  geom_gls() +
  scale_y_continuous(labels = function(l){trans = l / 1000})+
  scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  ylab(expression("Invented Index, 10"^3*"widgets")) +
  xlab("Time") +
  labs(tag = "a")  +
  theme_ts()


# EPU map subfigure
# Set lat/lon window for map
xmin = -78
xmax = -65
ymin = 36
ymax = 45
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)

#CRS
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#Specify data frame with lat/lon locations for labels
epu_labels <- data.frame(EPU = c("Mid-Atlantic\n Bight",
                                 "Gulf of Maine",
                                 "Georges Bank"),
                         latitude = c(40,42.85,41),
                         longitude = c(-72.7,-69,-68.5))

#Map of NE LME
epumap <- ggplot() +
  geom_sf(data = coast, size = map.lwd) +
  geom_sf(data = epu_sf, fill = "transparent", size = map.lwd) +
  coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
  geom_text(data = epu_labels, aes(x = longitude,
                                    y = latitude,
                                    label = EPU),
            size = 1.7)+
  theme_map() +
  scale_x_continuous(breaks = seq(-78, -65, by = 4), expand = c(0.01, 0.01)) +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(tag = "b")

psample + plot_spacer() + epumap + plot_layout(ncol = 3, widths=c(3,.5,4.5))
