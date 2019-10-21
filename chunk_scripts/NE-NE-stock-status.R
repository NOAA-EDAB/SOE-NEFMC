
#Get data, spread for plotting, and filter
stock_status <- ecodata::stock_status %>%
  spread(.,Var,Value) %>% 
  filter(Council %in% c(council_abbr,"Both"))

#Plot constants
y.max <- 4.5
x.max <- 7.5

all_missing <- stock_status %>%
  filter(is.na(B.Bmsy),is.na(F.Fmsy)) %>% 
  dplyr::select(Code, Council)

b_missing <- stock_status %>%
  filter(is.na(B.Bmsy), !is.na(F.Fmsy)) %>% 
  dplyr::select(Code, Council)

f_missing <- stock_status %>%
  filter(is.na(F.Fmsy), !is.na(B.Bmsy)) %>% 
  dplyr::select(Code, Council)

#A dataframe that defines custom legend for stocks with unknown status

all.df <- tibble(text = all_missing$Code,
                    x = rep(x.max,length(all_missing$Code)),
                    y = seq(4.35,2.85,-0.22),
                    color = all_missing$Council)

b.df <- tibble(text = b_missing$Code,
                    x = rep(x.max*0.8,length(b_missing$Code)),
                    y = c(4.35,4.14),
                    color = b_missing$Council)

f.df <- tibble(text = f_missing$Code,
                    x = rep(x.max*0.6,length(f_missing$Code)),
                    y = seq(4.35,2.85,-0.22),
                    color = f_missing$Council)


#Plotting code
ggplot(data = stock_status) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "grey60")+
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60")+
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  geom_point(aes(x = B.Bmsy,
                 y = F.Fmsy,
                 color = Council,
                 shape = Council)) +
  geom_text_repel(aes(x = B.Bmsy, #geom_text_repel auto-jitters text around points
                      y = F.Fmsy,
                      label = Code,
                      color = Council), show.legend = FALSE,nudge_y = 0.05, nudge_x = 0.05) +
  ylim(0,y.max) +
  xlim(0,x.max*1.1) +
  geom_text(data = all.df, aes(x = x, y = y, label = text, color = color),show.legend = FALSE, size = 3)+
  geom_text(data = b.df, aes(x = x, y = y, label = text, color = color),show.legend = FALSE, size = 3)+
  geom_text(data = f.df, aes(x = x, y = y, label = text, color = color),show.legend = FALSE, size = 3)+
  scale_color_manual(values = c("purple","blue"),#c("purple","blue"), #Change legend labels for clarity
                   name = "Managed by",
                   breaks = c("Both","NEFMC"),
                   labels = c("MAFMC/NEFMC","NEFMC"))+
  scale_shape_manual(values = c(1, 19), #Change legend labels for clarity
                     name = "Managed by",
                     breaks = c("Both","NEFMC"),
                     labels = c("MAFMC/NEFMC","NEFMC"))+
  annotate("rect", xmin = 0.924*x.max,
           xmax = 1.08*x.max,
           ymin = 0.645*y.max,
           ymax = 0.98*y.max,
           alpha = 0.01) +
  annotate("text", x = 7.5, y = 4.5, label = "F and B missing", fontface =2, size = 3)+
    annotate("rect", 
             xmin = 0.729*x.max,
           xmax = 0.871*x.max,
           ymin = 0.905*y.max,
           ymax = 0.98*y.max,
           alpha = 0.01) +
  annotate("text", x = 6, y = 4.5, label = "B missing", fontface =2, size = 3)+
    annotate("rect", xmin = 0.509*x.max,
           xmax = 0.681*x.max,
           ymin = 0.65*y.max,
           ymax = 0.98*y.max,
           alpha = 0.01) +
  annotate("text", x = 4.5, y = 4.5, label = "F missing", fontface =2, size = 3)+
  xlab(expression(~B/B[msy])) +
  ylab(expression(~F/F[msy])) +
  theme_ts()
