# libraries ----------------------------------------------------------------
library(magick)

# define bidding zones to visualize ---------------------------------------
BiddingZone1 = "Belgium"
BiddingZone2 = "France"

# loop througth 10 and 11 Nov, creating 48 domains and save as png --------
for (i in seq_along(seq.POSIXt(as.POSIXct("2022-11-10 00:00", "CET"),
                               as.POSIXct("2022-11-11 23:00", "CET"),
                               by = "hour"))) {
  
  tryCatch
  DateTime = seq.POSIXt(as.POSIXct("2022-11-10 00:00", "CET"),
                        as.POSIXct("2022-11-11 23:00", "CET"),
                        by = "hour")[i]
  
  JAOPuTo_domainvisualization_positions(DateTime, BiddingZone1, BiddingZone2, TRUE)
  
  ggplot() +
    geom_hline(yintercept = 0, size = .1, colour = "grey") +
    geom_vline(xintercept = 0, size = .1, colour = "grey") +
    geom_polygon(data = df_domain,
                 mapping = aes(x = x, y = y, fill = Type),
                 alpha = .4) +
    geom_point(data = df_netpos,
               mapping = aes(x = NetPosition[BiddingZone == BiddingZone1],
                             y = NetPosition[BiddingZone == BiddingZone2]),
               size = 2.5, stroke = .25, shape = 21, colour = "white", fill = "#BB3E03") +
    scale_fill_manual(name = "Domain",
                      values = c("#6A994E", "#DE8F6E"),
                      labels = c("presolved, flow-based", "long-term allocations")) +
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
    labs(title = "How do these domains evolve through time?",
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
          legend.position = "none",
          plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0),
          plot.caption.position = "plot",
          axis.ticks = element_line(colour = "grey", size = .1),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linetype = "dotted", colour = "grey", size = .1),
          plot.background = element_rect(colour = NA, fill = "white"),
          plot.margin = margin(.25, .25, .25, .25, "cm"))
  
  ggsave(paste0("images/GIF/",
                format(DateTime, "%Y%m%d_%H"),
                ".png"),
         width = 16, height = 17, units = "cm", dpi = 500)
  
}

# create GIF based on 48 png's---------------------------------------------

## list file names and read in
imgs <- list.files("GIF", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 10)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "GIF test.gif")