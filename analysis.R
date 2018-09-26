library(reshape2)
library(ggplot2)
library(dplyr)
library(gridExtra)

dat <- read.table("./data/stany.tsv")

mdat <- mutate(dat, 
               pos = 1L:nrow(dat),
               facet_id = factor(floor(pos/200)),
               pos = pos,
               change = V1 - V2) %>% 
  melt(id.vars = c("pos", "facet_id"), variable.name = "type") %>% 
  mutate(type = as.character(type), 
         type = ifelse(type == "V1", "SCS_A", ifelse(type == "V2", "SCS_A_Snrk26K50"))) 


p1 <- ggplot(filter(mdat, type != "change"), aes(x = pos, y = value, group = type, color = type)) +
  geom_line() +
  scale_y_continuous("Exchange rate [1/s]") +
  scale_x_continuous("Position") +
  scale_color_discrete("") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank())


p2 <- ggplot(filter(mdat, type == "change"), aes(x = pos, y = value)) +
  geom_line() +
  scale_y_continuous("Change in the exchange rate [1/s]") +
  scale_x_continuous("Position") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank())

pdf("comparison.pdf", height = 7, width = 11)
grid.arrange(p1, p2, ncol = 1)
dev.off()
