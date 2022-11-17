library(tidyverse)
library(lubridate)
library(scales)

# example -----------------------------------------------------------------
BiddingZone1 = "Belgium"
BiddingZone2 = "France"
DateTime = as.POSIXct("2022-11-01 20:00", "CET")

JAOPuTo_domainvisualization_positions(DateTime, BiddingZone1, BiddingZone2, TRUE)

# without LTA -------------------------------------------------------------
ggplot() +
  geom_hline(yintercept = 0, size = .1, colour = "grey") +
  geom_vline(xintercept = 0, size = .1, colour = "grey") +
  geom_polygon(data = filter(df_domain, Type == "FB"),
               mapping = aes(x = x, y = y, fill = "presolved, flow-based"),
               alpha = .4) +
  geom_abline(data = df_CNECs,
              mapping = aes(intercept = Intercept, 
                            slope = Slope,
                            colour = Type),
              size = .25) +
  geom_point(data = df_netpos,
             mapping = aes(x = NetPosition[BiddingZone == BiddingZone1],
                           y = NetPosition[BiddingZone == BiddingZone2]),
             size = 2.5, stroke = .25, shape = 21, colour = "white", fill = "#BB3E03") +
  scale_fill_manual(name = "Domain",
                    values = c("#6A994E")) +
  scale_colour_manual(name = "CNEC",
                      values = c("#BB3E03", "#598392")) +
  scale_x_continuous(name = element_blank(),
                     breaks = seq(-10000, 10000, 2500),
                     labels = c("-10.000 MW\nBelgium\n(import)",
                                format(seq(-7500, 7500, 2500), big.mark = ".", decimal.mark = ","),
                                "10.000 MW\nBelgium\n(export)")) +
  scale_y_continuous(name = element_blank(),
                     breaks = seq(-10000, 10000, 2500),
                     labels = c("-10.000 MW\nFrance\n(import)",
                                format(seq(-7500, 7500, 2500), big.mark = ".", decimal.mark = ","),
                                "10.000 MW\nFrance\n(export)")) +
  labs(title = "How much electricity can Belgium and France exchange with other Core bidding zones?",
       subtitle = paste0("Presolved domain, active constraints and Core net positions for Belgium (horizontally) and France (vertically)\non ",
                         format(DateTime, "%d %B %Y from %H:%M"),
                         " to ",
                         format(DateTime + hours(1), "%H:%M")),
       caption = "Figure by Nico Schoutteet | Data by Core TSOs via JAO Publication Tool") +
  coord_fixed(ratio = 1,
              xlim = c(-10000, 10000),
              ylim = c(-10000, 10000)) +
  theme_minimal() +
  theme(text = element_text(size = 8, family = "serif"),
        legend.title = element_text(face = "bold"),
        legend.key.size = unit(.25, "cm"),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        axis.ticks = element_line(colour = "grey", size = .1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dotted", colour = "grey", size = .1),
        plot.background = element_rect(colour = NA, fill = "white"),
        plot.margin = margin(.25, .25, .25, .25, "cm"))

ggsave("images/example domain without LTA.png",
       width = 16, height = 18, units = "cm", dpi = 500)

# with LTA -------------------------------------------------------------
ggplot() +
  geom_hline(yintercept = 0, size = .1, colour = "grey") +
  geom_vline(xintercept = 0, size = .1, colour = "grey") +
  geom_polygon(data = df_domain,
               mapping = aes(x = x, y = y, fill = Type),
               alpha = .4) +
  geom_abline(data = df_CNECs,
              mapping = aes(intercept = Intercept, 
                            slope = Slope,
                            colour = Type),
              size = .25) +
  geom_point(data = df_netpos,
             mapping = aes(x = NetPosition[BiddingZone == BiddingZone1],
                           y = NetPosition[BiddingZone == BiddingZone2]),
             size = 2.5, stroke = .25, shape = 21, colour = "white", fill = "#BB3E03") +
  scale_fill_manual(name = "Domain",
                    values = c("#6A994E", "#DE8F6E"),
                    labels = c("presolved, flow-based", "long-term allocations")) +
  scale_colour_manual(name = "CNEC",
                      values = c("#BB3E03", "#598392")) +
  scale_x_continuous(name = element_blank(),
                     breaks = seq(-10000, 10000, 2500),
                     labels = c("-10.000 MW\nBelgium\n(import)",
                                format(seq(-7500, 7500, 2500), big.mark = ".", decimal.mark = ","),
                                "10.000 MW\nBelgium\n(export)")) +
  scale_y_continuous(name = element_blank(),
                     breaks = seq(-10000, 10000, 2500),
                     labels = c("-10.000 MW\nFrance\n(import)",
                                format(seq(-7500, 7500, 2500), big.mark = ".", decimal.mark = ","),
                                "10.000 MW\nFrance\n(export)")) +
  labs(title = "Are capacities allocated in the long-term timeframe covered by the presolved domain?",
       subtitle = paste0("Presolved domain, active constraints, LTA domain and Core net positions for Belgium (horizontally) and France (vertically)\non ",
                         format(DateTime, "%d %B %Y from %H:%M"),
                         " to ",
                         format(DateTime + hours(1), "%H:%M")),
       caption = "Figure by Nico Schoutteet | Data by Core TSOs via JAO Publication Tool") +
  coord_fixed(ratio = 1,
              xlim = c(-10000, 10000),
              ylim = c(-10000, 10000)) +
  theme_minimal() +
  theme(text = element_text(size = 8, family = "serif"),
        legend.title = element_text(face = "bold"),
        legend.key.size = unit(.25, "cm"),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        axis.ticks = element_line(colour = "grey", size = .1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dotted", colour = "grey", size = .1),
        plot.background = element_rect(colour = NA, fill = "white"),
        plot.margin = margin(.25, .25, .25, .25, "cm"))

ggsave("images/example domain with LTA.png",
       width = 16, height = 18, units = "cm", dpi = 500)

# minRAM check without LTA -------------------------------------------------------------
ggplot() +
  geom_hline(yintercept = 0, size = .1, colour = "grey") +
  geom_vline(xintercept = 0, size = .1, colour = "grey") +
  geom_polygon(data = filter(df_domain, Type == "FB"),
               mapping = aes(x = x, y = y, fill = "presolved, flow-based"),
               alpha = .4) +
  geom_abline(data = df_CNECs,
              mapping = aes(intercept = Intercept, 
                            slope = Slope,
                            colour = RAM < .2),
              size = .25) +
  geom_point(data = df_netpos,
             mapping = aes(x = NetPosition[BiddingZone == BiddingZone1],
                           y = NetPosition[BiddingZone == BiddingZone2]),
             size = 2.5, stroke = .25, shape = 21, colour = "white", fill = "#BB3E03") +
  scale_fill_manual(name = "Domain",
                    values = c("#6A994E")) +
  scale_colour_manual(name = "CNEC",
                      values = c("grey", "#BB3E03"),
                      labels = c("with RAM above 20%", "with RAM below 20%")) +
  scale_x_continuous(name = element_blank(),
                     breaks = seq(-10000, 10000, 2500),
                     labels = c("-10.000 MW\nBelgium\n(import)",
                                format(seq(-7500, 7500, 2500), big.mark = ".", decimal.mark = ","),
                                "10.000 MW\nBelgium\n(export)")) +
  scale_y_continuous(name = element_blank(),
                     breaks = seq(-10000, 10000, 2500),
                     labels = c("-10.000 MW\nFrance\n(import)",
                                format(seq(-7500, 7500, 2500), big.mark = ".", decimal.mark = ","),
                                "10.000 MW\nFrance\n(export)")) +
  labs(title = "Which network elements have low margins for cross-zonal exchanges?",
       subtitle = paste0("Presolved domain and Core net positions for Belgium (horizontally) and France (vertically)\non ",
                         format(DateTime, "%d %B %Y from %H:%M"),
                         " to ",
                         format(DateTime + hours(1), "%H:%M")),
       caption = "Figure by Nico Schoutteet | Data by Core TSOs via JAO Publication Tool") +
  coord_fixed(ratio = 1,
              xlim = c(-10000, 10000),
              ylim = c(-10000, 10000)) +
  theme_minimal() +
  theme(text = element_text(size = 8, family = "serif"),
        legend.title = element_text(face = "bold"),
        legend.key.size = unit(.25, "cm"),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        axis.ticks = element_line(colour = "grey", size = .1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dotted", colour = "grey", size = .1),
        plot.background = element_rect(colour = NA, fill = "white"),
        plot.margin = margin(.25, .25, .25, .25, "cm"))

ggsave("images/example minRAM check.png",
       width = 16, height = 18, units = "cm", dpi = 500)



# visualization per border instead of per zone ----------------------------
Border1 = "BE - NL"
Border2 = "BE - FR"
DateTime = as.POSIXct("2022-11-01 20:00", "CET")

JAOPuTo_domainvisualization_exchanges(DateTime, Border1, Border2, TRUE)


# without LTA -------------------------------------------------------------
ggplot() +
  geom_hline(yintercept = 0, size = .1, colour = "grey") +
  geom_vline(xintercept = 0, size = .1, colour = "grey") +
  geom_polygon(data = filter(df_domain, Type == "FB"),
               mapping = aes(x = x, y = y, fill = "presolved, flow-based"),
               alpha = .4) +
  geom_abline(data = df_CNECs,
              mapping = aes(intercept = Intercept, 
                            slope = Slope,
                            colour = Type),
              size = .25) +
  geom_point(data = df_schexch,
             mapping = aes(x = SchExchanges[Border == Border1],
                           y = SchExchanges[Border == Border2]),
             size = 2.5, stroke = .25, shape = 21, colour = "white", fill = "#BB3E03") +
  scale_fill_manual(name = "Domain",
                    values = c("#6A994E")) +
  scale_colour_manual(name = "CNEC",
                      values = c("#BB3E03", "#598392")) +
  scale_x_continuous(name = element_blank(),
                     breaks = seq(-10000, 10000, 2500),
                     labels = c("-10.000 MW\nBelgium\n(import)",
                                format(seq(-7500, 7500, 2500), big.mark = ".", decimal.mark = ","),
                                "10.000 MW\nBelgium\n(export)")) +
  scale_y_continuous(name = element_blank(),
                     breaks = seq(-10000, 10000, 2500),
                     labels = c("-10.000 MW\nFrance\n(import)",
                                format(seq(-7500, 7500, 2500), big.mark = ".", decimal.mark = ","),
                                "10.000 MW\nFrance\n(export)")) +
  labs(title = "How much electricity can Belgium exchange directly with the Netherlands and France?",
       subtitle = paste0("Presolved domain, active constraints and scheduled exchanges for BE - NL (horizontally) and BE - FR (vertically)\non ",
                         format(DateTime, "%d %B %Y from %H:%M"),
                         " to ",
                         format(DateTime + hours(1), "%H:%M")),
       caption = "Figure by Nico Schoutteet | Data by Core TSOs via JAO Publication Tool") +
  coord_fixed(ratio = 1,
              xlim = c(-10000, 10000),
              ylim = c(-10000, 10000)) +
  theme_minimal() +
  theme(text = element_text(size = 8, family = "serif"),
        legend.title = element_text(face = "bold"),
        legend.key.size = unit(.25, "cm"),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        axis.ticks = element_line(colour = "grey", size = .1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dotted", colour = "grey", size = .1),
        plot.background = element_rect(colour = NA, fill = "white"),
        plot.margin = margin(.25, .25, .25, .25, "cm"))

ggsave("images/example domain per border.png",
       width = 16, height = 18, units = "cm", dpi = 500)
