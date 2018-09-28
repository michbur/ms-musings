library(reshape2)
library(ggplot2)
library(dplyr)
library(gridExtra)

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
         type = ifelse(type == "V1", "SCS_A", ifelse(type == "V2", "SCS_A_Snrk26K50", type))) %>% 
  inner_join(seq, by = c("pos" = "pos"))


p1 <- ggplot(filter(mdat, type != "change", pos %in% 1L:200), aes(x = pos, y = value, group = type, color = type)) +
  geom_line() +
  scale_y_continuous("Exchange rate [1/s]") +
  scale_x_continuous("") +
  scale_color_discrete("") +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(),
        strip.text.x = element_blank())

p3 <- ggplot(filter(mdat, type == "change", pos %in% 1L:200), aes(x = pos, y = 1, label = aa)) +
  geom_text() +
  scale_x_continuous("") +
  scale_y_continuous("") +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(-15, 1, -15, 1), "lines"))

p2 <- ggplot(filter(mdat, type == "change", pos %in% 1L:200), aes(x = pos, y = value)) +
  geom_line() +
  scale_y_continuous("Change in the exchange rate [1/s]") +
  scale_x_continuous("Position") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

pdf("comparison1.pdf", height = 7, width = 33)
grid.arrange(p1, p3, p2, ncol = 1, heights = c(0.46, 0.08, 0.46))
dev.off()


p1 <- ggplot(filter(mdat, type != "change", pos %in% 201L:375), aes(x = pos, y = value, group = type, color = type)) +
  geom_line() +
  scale_y_continuous("Exchange rate [1/s]") +
  scale_x_continuous("") +
  scale_color_discrete("") +
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank(),
        strip.text.x = element_blank())

p3 <- ggplot(filter(mdat, type == "change", pos %in% 201L:375), aes(x = pos, y = 1, label = aa)) +
  geom_text() +
  scale_x_continuous("") +
  scale_y_continuous("") +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(-15, 1, -15, 1), "lines"))

p2 <- ggplot(filter(mdat, type == "change", pos %in% 201L:375), aes(x = pos, y = value)) +
  geom_line() +
  scale_y_continuous("Change in the exchange rate [1/s]") +
  scale_x_continuous("Position") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

pdf("comparison2.pdf", height = 7, width = 33)
grid.arrange(p1, p3, p2, ncol = 1, heights = c(0.46, 0.08, 0.46))
dev.off()
