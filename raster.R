
# List all bidding zones to include in raster -----------------------------
BiddingZones <- c("Austria", "Belgium", "Croatia", "Czech Republic", "France", "Germany/Luxembourg",
                  "Hungary", "Netherlands", "Poland", "Romania", "Slovakia", "Slovenia",
                  "ALEGrO Belgium", "ALEGrO Germany")
# define which our to visualize -------------------------------------------
DateTime = as.POSIXct("2022-11-01 20:00", "CET")


# empty dataframe to hold domains for each combination of bidding  --------

df_domains <- data.frame()

# loop through all bidding zones twice ------------------------------------
# excluding identical combination (e.g. "Austria - "Austria")
for (i in seq_along(BiddingZones)) {
  for (j in seq_along(BiddingZones)) {
    
    if(i != j) {
      JAOPuTo_domainvisualization_positions(DateTime, BiddingZones[i], BiddingZones[j], TRUE)
      df_domains <- df_domains %>% 
        rbind(df_domain %>%
                mutate(BiddingZone1 = BiddingZones[i],
                       BiddingZone2 = BiddingZones[j]))
    }
  }
}

df_domains$BiddingZone1[df_domains$BiddingZone1 == "Germany/Luxembourg"] <- "Germany/\nLuxembourg"
df_domains$BiddingZone2[df_domains$BiddingZone2 == "Germany/Luxembourg"] <- "Germany/\nLuxembourg"

df_domains$BiddingZone1[df_domains$BiddingZone1 == "Czech Republic"] <- "Czech\nRepublic"
df_domains$BiddingZone2[df_domains$BiddingZone2 == "Czech Republic"] <- "Czech\nRepublic"

df_domains$BiddingZone1[df_domains$BiddingZone1 == "ALEGrO Belgium"] <- "ALEGro\nBelgium"
df_domains$BiddingZone2[df_domains$BiddingZone2 == "ALEGrO Belgium"] <- "ALEGro\nBelgium"

df_domains$BiddingZone1[df_domains$BiddingZone1 == "ALEGrO Germany"] <- "ALEGro\nGermany"
df_domains$BiddingZone2[df_domains$BiddingZone2 == "ALEGrO Germany"] <- "ALEGro\nGermany"



# visualize with small multiples (polygons only, no CNECs) ----------------
ggplot(data = df_domains,
       mapping = aes(x = x, y = y, fill = Type)) +
  geom_vline(xintercept = 0, size = .1, colour = "grey") +
  geom_hline(yintercept = 0, size = .1, colour = "grey") +
  geom_polygon(alpha = .4) +
  coord_fixed(ratio = 1,
              xlim = c(-7500, 7500),
              ylim = c(-7500, 7500)) +
  scale_fill_manual(name = "Domain",
                    values = c("#6A994E", "#DE8F6E"),
                    labels = c("presolved, flow-based", "long-term allocations")) +  
  facet_grid(BiddingZone1 ~ BiddingZone2) +
  labs(title = "Every possible way to slice the flow-based potato",
       subtitle = paste0("Presolved and LTA domains for all combinations of the 14 Core bidding zones' net positions\non ",
                         format(DateTime, "%d %B %Y from %H:%M"),
                         " to ",
                         format(DateTime + hours(1), "%H:%M")),
       caption = "Figure by Nico Schoutteet | Data by Core TSOs via JAO Publication Tool") +
  theme_minimal() +
  theme(text = element_text(size = 8, family = "serif"),
        legend.title = element_text(face = "bold"),
        legend.key.size = unit(.25, "cm"),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(colour = NA, fill = "white"),
        plot.margin = margin(.25, .25, .25, .25, "cm"),
        panel.spacing = unit(.1, "cm"))

ggsave("images/raster.png",
       width = 24, height = 27, units = "cm", dpi = 400)
