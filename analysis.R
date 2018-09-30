library(reshape2)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(cowplot)

dat <- read.table("./data/stany.tsv")

seq <- readLines("./data/seq.txt") %>% 
  strsplit("") %>% 
  unlist %>% 
  data.frame(pos = 1L:length(.), aa = .)

mdat <- mutate(dat, 
               pos = 1L:nrow(dat),
               facet_id = factor(floor(pos/200)),
               pos = pos,
               change = V1 - V2) %>% 
  melt(id.vars = c("pos", "facet_id"), variable.name = "type") %>% 
  mutate(type = as.character(type), 
         type = factor(ifelse(type == "V1", "SCS (protein reference)", ifelse(type == "V2", "SCS (in complex with Snrk26K50)", type))),
         type = relevel(type, ref = "SCS (protein reference)")) %>% 
  inner_join(seq, by = c("pos" = "pos")) %>% 
  mutate(value_disc = cut(value, breaks = c(min(value), -50, -25, 0, 25, 50, max(value)), include.lowest = TRUE),
         ef_domains = pos %in% c(29L:64, 94L:110, 254L:289, 300L:335),
         zinc_binding = pos %in% 43L:53)



p1 <- ggplot(filter(mdat, type != "change"), aes(x = pos, y = value, group = type, color = type)) +
  geom_line(size = 1.2) +
  scale_y_continuous("Exchange rate [1/s]") +
  scale_x_continuous("", expand = c(0, 0)) +
  scale_color_manual("", values = c("red", "blue")) +
  theme_bw(base_size = 24) +
  theme(legend.position = "top",
        legend.key.width = unit(6, "line"),
        #legend.key.height = unit(2, "line"),
        strip.background = element_blank(),
        strip.text.x = element_blank())

p3 <- ggplot(filter(mdat, type == "change"), 
             aes(x = pos, y = 1, label = aa, fill = ef_domains)) +
  geom_tile() +
  geom_line() +
  annotate("text", x = 10, y = 1.5, label = "EF domain", color = "black", size = 6, position = "left") +
  scale_x_continuous("", expand = c(0, 0)) +
  scale_y_discrete("") +
  scale_fill_manual("Region", values = c(NA, "green"), guide = FALSE) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = NA, color = NA),
        axis.line = element_blank(),
        plot.margin = unit(c(-1, 1, -1, 1), "lines"))

p4 <- ggplot(filter(mdat, type == "change"), 
             aes(x = pos, y = factor(1), label = aa, fill = zinc_binding)) +
  geom_tile() +
  geom_line() +
  annotate("text", x = 14, y = 1.3, label = "Zinc binding domain", color = "black", size = 6, position = "left") +
  scale_x_continuous("", expand = c(0, 0)) +
  scale_y_discrete("") +
  scale_fill_manual("Region", values = c(NA, "orange"), guide = FALSE) +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = NA, color = NA),
        axis.line = element_blank(),
        plot.margin = unit(c(-1, 1, -1, 1), "lines"))


p2 <- ggplot(filter(mdat, type == "change"), aes(x = pos, y = value)) +
  geom_line(size = 1.2) +
  #geom_tile(data = data.frame(x = c(1, 2), 
  #                            y = c(1, 1), 
  #                            z = c("SCS (protein reference)", "SCS (in complex with Snrk26K50)")), 
  #          aes(x = x, y = y, fill = z)) +
  annotate("rect", xmin = 0, xmax = 375,  ymin = -100, ymax = 0, fill = "blue", colour = NA, alpha = 0.3) +
  annotate("text", x = 205, y = 75, label = "Increased labeling of the SCS protein upon binding with Snrk26K50", color = "red", size = 9) +
  annotate("text", x = 205, y = -75, label = "Decreased labeling of the SCS protein upon binding with Snrk26K50", color = "blue", size = 9) +
  annotate("rect", xmin = 0, xmax = 375,  ymin = -0, ymax = 100, fill = "red", colour = NA, alpha = 0.3) +
  #scale_y_continuous("Change in the exchange rate [1/s]\n(difference of protein reference and in complex with Snrk26K50") +
  scale_y_continuous("Change in the exchange rate [1/s]") +
  scale_x_continuous("Position", expand = c(0, 0)) +
  theme_bw(base_size = 24) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

pdf("comparison1.pdf", height = 14, width = 44)
plot_grid(p1, p3, p4, p2, align = "v", ncol = 1, rel_heights = c(0.4, 0.05, 0.05, 0.4))
dev.off()


tiff("comparison1.tiff", height = 14, width = 44, units = "in", res = 150, compression = "zip")
plot_grid(p1, p3, p4, p2, align = "v", ncol = 1, rel_heights = c(0.4, 0.05, 0.05, 0.4))
dev.off()
