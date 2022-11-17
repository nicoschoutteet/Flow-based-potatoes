# libraries ---------------------------------------------------------------
library(tidyverse)

Core_map <- map_data("world") %>% 
  filter(region %in% c("Belgium","Netherlands", "Luxembourg", "Germany", "Austria", "Czech Republic", "Hungary", "Slovakia", "Slovenia", "Croatia", "Romania", "Poland") |
           (region == "France" & is.na(subregion)))

ggplot(data = Core_map,
       mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "white", fill = "#4794a8", size = .1) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("images/Core map.png", width = 6, height = 4, units = "cm", dpi = 500)
