# libraries ---------------------------------------------------------------
library(tidyverse)
library(scales)
library(lubridate)
library(ggtern)

# custom colour palette to improve readability of plot
customcolours <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

# import data via JAO API (first load helper functions from "helper functions - download.R), define start and end date according to own needs
df_finaldomain <- JAOPuTo_finaldomain(as.POSIXct("2022-06-09 00:00", "CET"),
                                      as.POSIXct("2022-06-09 23:00", "CET")) %>% 
  filter(!is.na(tso), tso != "NA") 


# aggregate values per TSO
df_pertso <- df_finaldomain %>% 
  group_by(tso) %>% 
  summarize(ram = mean(ram / fmax, na.rm = TRUE),
            frm = mean(frm / fmax, na.rm = TRUE)) %>% 
  mutate(fref = 1 - ram - frm)

# construct ternary plot
ggtern(data = df_pertso,
       mapping = aes(x = fref, y = frm, z = ram, fill = tso)) +
  geom_point(size = 3, shape = 21, 
             colour = "white") +
  geom_crosshair_tern(data = filter(df_pertso, tso == "ELIA")) +
  geom_Rline(Rintercept = .2,
             linetype = "dashed", colour = "red") +
  scale_L_continuous(breaks = seq(0, 1, .1)) +
  scale_T_continuous(breaks = seq(0, 1, .1)) +
  scale_R_continuous(breaks = seq(0, 1, .1)) +
  scale_fill_manual(name = element_blank(), values = customcolours) +
  labs(title = "Decomposition of margins on network elements",
       subtitle = "Average RAM, FRM and Fref per TSO on all network elements in presolved final domains, in % of Fmax",
       caption = "Source: calculations Nico Schoutteet based on data JAO Publication Tool",
       xarrow = "Average flow on network element before market coupling (Fref)\n",
       yarrow = "Average security margin (FRM)\n",
       zarrow = "\nAverage margin available for cross-zonal exchanges (RAM)\n") +
  theme_bw() +
  theme_hidetitles() +
  theme_arrowsmall() +
  theme(panel.grid.minor = element_line(linetype = "dotted"),
        legend.position = "right",
        legend.key.size = unit(.25, "cm"),
        plot.title = element_text(size = 10, hjust = 0, face = "bold"), 
        plot.subtitle = element_text(size = 8, hjust = 0, margin = margin(0, 0, 0.5, 0, "cm")), plot.title.position = "plot", 
        plot.caption = element_text(hjust = 0, size = 5, margin = margin(0, 0, 0.5, 0, "cm")), plot.caption.position = "plot", 
        plot.margin = margin(0, 1, 0, 0, "cm"))

ggsave("images/ternary per TSO.png",
       width = 24, height = 15, units = "cm", dpi = 900)



