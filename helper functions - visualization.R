
# Domain visualization ----------------------------------------------------

# These two functions create the dataframes and basic layout for visualizing the flow-based domains

# Per bidding zone

JAOPuTo_domainvisualization_positions <- function(DateTime, BiddingZone1, BiddingZone2, return_dataframes = FALSE) {
  
  #define which hour to download
  start <- DateTime
  end <- start + hours(1)
  
  #from bidding zone to abbreviation
  BiddingZoneAb1 = case_when(BiddingZone1 == "Austria" ~ "AT",
                             BiddingZone1 == "Belgium" ~ "BE",
                             BiddingZone1 == "Croatia" ~ "HR",
                             BiddingZone1 == "Czech Republic" ~ "CZ",
                             BiddingZone1 == "France" ~ "FR",
                             BiddingZone1 == "Germany/Luxembourg" ~ "DE",
                             BiddingZone1 == "Hungary" ~ "HU",
                             BiddingZone1 == "Netherlands" ~ "NL",
                             BiddingZone1 == "Poland" ~ "PL",
                             BiddingZone1 == "Romania" ~ "RO",
                             BiddingZone1 == "Slovakia" ~ "SK",
                             BiddingZone1 == "Slovenia" ~ "SI",
                             BiddingZone1 == "ALEGrO Belgium" ~ "ALBE",
                             BiddingZone1 == "ALEGrO Germany" ~ "ALDE")
  
  BiddingZoneAb2 = case_when(BiddingZone2 == "Austria" ~ "AT",
                             BiddingZone2 == "Belgium" ~ "BE",
                             BiddingZone2 == "Croatia" ~ "HR",
                             BiddingZone2 == "Czech Republic" ~ "CZ",
                             BiddingZone2 == "France" ~ "FR",
                             BiddingZone2 == "Germany/Luxembourg" ~ "DE",
                             BiddingZone2 == "Hungary" ~ "HU",
                             BiddingZone2 == "Netherlands" ~ "NL",
                             BiddingZone2 == "Poland" ~ "PL",
                             BiddingZone2 == "Romania" ~ "RO",
                             BiddingZone2 == "Slovakia" ~ "SK",
                             BiddingZone2 == "Slovenia" ~ "SI",
                             BiddingZone2 == "ALEGrO Belgium" ~ "ALBE",
                             BiddingZone2 == "ALEGrO Germany" ~ "ALDE")
  
  #download presolved domain with this package's wrapper
  df_finaldomain <- JAOPuTo_finaldomain(start, end) %>%
    filter(cneEic != "NA")
  
  #calculate positions based on ptdf values corresponding to BiddingZone1 and BiddingZone2
  df_finaldomain <- df_finaldomain %>%
    mutate(Intercept = ram / df_finaldomain[[paste0("ptdf_", BiddingZoneAb2)]],
           Slope = - df_finaldomain[[paste0("ptdf_", BiddingZoneAb1)]] /
             df_finaldomain[[paste0("ptdf_", BiddingZoneAb2)]]) %>%
    mutate(CNEC = paste0(tso, ": ", cneName, " | ", contName, " - ", direction),
           Type = "Presolved",
           Location = ifelse(hubFrom == hubTo, "Internal", "Cross-border"),
           RAM = ram / fmax) %>%
    select(CNEC, TSO = tso, Location, Type, Intercept, Slope, RAM) %>%
    filter(!Intercept %in% c("Inf", "NaN", NA))
  
  #include dummy active constraint in case there are none (i.e. price convergence)
  df_shadowprices <- data.frame(CNEC = "DummyAC",
                                TSO = "DummyTSO",
                                Location = "Cross-border",
                                Type = "Active",
                                Intercept = Inf,
                                Slope = Inf,
                                RAM = 1,
                                ShadowPrice = 999)
  
  tryCatch({
    #download active constraints with this package's wrapper, escape in case there are none (i.e. price convergence)
    df_shadowprices <- JAOPuTo_shadowprices(start, end)
    
    #calculate positions based on ptdf values corresponding to BiddingZone1 and BiddingZone2
    df_shadowprices <- df_shadowprices %>%
      mutate(Intercept = ram / df_shadowprices[[paste0("hub_", BiddingZoneAb2)]],
             Slope = - df_shadowprices[[paste0("hub_", BiddingZoneAb1)]] /
               df_shadowprices[[paste0("hub_", BiddingZoneAb2)]]) %>%
      mutate(CNEC = paste0(tso, ": ", cnecName, " | ", contName, " - ", direction),
             Type = "Active",
             Location = ifelse(hubFrom == hubTo, "Internal", "Cross-border"),
             RAM = ram / fmax) %>%
      select(CNEC, TSO = tso, Location, Type, Intercept, Slope, RAM, ShadowPrice = shadowPrice)
    
  }, error = function(e) {})
  
  #combine presolved and active CNECs
  df_CNECs <- bind_rows(df_finaldomain, df_shadowprices) %>%
    filter(Intercept != "Inf")
  
  #download net positions with this package's wrapper
  df_netpos <- JAOPuTo_netpositions(start, end)
  
  #download LTA domain
  df_LTA <- JAOPuTo_lta(start, end) %>%
    filter(substr(Border, 1, 2) %in% c(BiddingZoneAb1,
                                       BiddingZoneAb2) |
             substr(Border, 6, 7) %in% c(BiddingZoneAb1,
                                         BiddingZoneAb2))
  
  df_LTAdomain <- data.frame(Type = rep("LTA", 5),
                             x = c(sum(df_LTA$LTA[substr(df_LTA$Border, 1, 2) == BiddingZoneAb1], na.rm = TRUE),
                                   sum(df_LTA$LTA[substr(df_LTA$Border, 1, 2) == BiddingZoneAb1], na.rm = TRUE),
                                   -sum(df_LTA$LTA[substr(df_LTA$Border, 6, 7) == BiddingZoneAb1], na.rm = TRUE),
                                   -sum(df_LTA$LTA[substr(df_LTA$Border, 6, 7) == BiddingZoneAb1], na.rm = TRUE),
                                   sum(df_LTA$LTA[substr(df_LTA$Border, 1, 2) == BiddingZoneAb1], na.rm = TRUE)),
                             y = c(-sum(df_LTA$LTA[substr(df_LTA$Border, 6, 7) == BiddingZoneAb2], na.rm = TRUE),
                                   sum(df_LTA$LTA[substr(df_LTA$Border, 1, 2) == BiddingZoneAb2], na.rm = TRUE),
                                   sum(df_LTA$LTA[substr(df_LTA$Border, 1, 2) == BiddingZoneAb2], na.rm = TRUE),
                                   -sum(df_LTA$LTA[substr(df_LTA$Border, 6, 7) == BiddingZoneAb2], na.rm = TRUE),
                                   -sum(df_LTA$LTA[substr(df_LTA$Border, 6, 7) == BiddingZoneAb2], na.rm = TRUE)))
  
  # functions to define x,y coordinates of innermost polygon (around origin)
  # explanation: https://stackoverflow.com/questions/74204298/how-do-i-find-the-surface-between-lines-based-on-intercept-and-slope-which-inclu/74350172#74350172  
  innermost <- function(slopes, intercepts) {
    
    meetings <- function(slopes, intercepts) {
      meets_at <- function(i1, s1, i2, s2) {
        ifelse(s1 - s2 == 0, NA, (i2 - i1)/(s1 - s2))
      }
      xvals <- outer(seq_along(slopes), seq_along(slopes), function(i, j) {
        meets_at(intercepts[i], slopes[i], intercepts[j], slopes[j])
      })
      
      yvals <- outer(seq_along(slopes), seq_along(slopes), function(i, j) {
        intercepts + slopes *
          meets_at(intercepts[i], slopes[i], intercepts[j], slopes[j])
      })
      
      cbind(x = xvals[lower.tri(xvals)], y = yvals[lower.tri(yvals)])
    }
    xy <- meetings(slopes, intercepts)
    is_cut <- function(x, y, slopes, intercepts) {
      d <- sqrt(x^2 + y^2)
      slope <- y / x
      xvals <- intercepts / (slope - slopes)
      yvals <- xvals * slopes + intercepts
      ds <- sqrt(xvals^2 + yvals^2)
      any(d - ds > 1e-6 & sign(xvals) == sign(x) & sign(yvals) == sign(y))
    }
    xy <- xy[sapply(seq(nrow(xy)), function(i) {
      !is_cut(xy[i, 1], xy[i, 2], slopes, intercepts)
    }),]
    xy <- xy[order(atan2(xy[,2], xy[,1])),]
    as.data.frame(rbind(xy, xy[1,]))
  }
  
  df_domain <- innermost(df_finaldomain$Slope, df_finaldomain$Intercept) %>%
    mutate(Type = "FB") %>%
    na.omit() %>%
    rbind(df_LTAdomain)
  
  
  #calculate size of presolved and LTA domains
  FBdomainsize <- st_as_sf(filter(df_domain, Type == "FB"), coords = c("x", "y")) %>%
    summarize(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>%
    st_area()
  
  LTAdomainsize <- st_as_sf(filter(df_domain, Type == "LTA"), coords = c("x", "y")) %>%
    summarize(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>%
    st_area()
  
  #return dataframes when specified TRUE in argument (default is FALSE)
  if (return_dataframes == TRUE) {
    df_netpos <<- df_netpos
    df_CNECs <<- df_CNECs
    df_domain <<- df_domain
    df_LTA <<- df_LTA
    FBdomainsize <<- FBdomainsize
    LTAdomainsize <<- LTAdomainsize
  }
  
  #visualize combined dataframe (presolved and active CNECs) and net positions
  ggplot() +
    geom_polygon(data = df_domain,
                 mapping = aes(x = x, y = y, fill = Type),
                 alpha = .2) +
    geom_hline(yintercept = 0, colour = "black", size = .25) +
    geom_vline(xintercept = 0, colour = "black", size = .25) +
    geom_abline(data = df_CNECs,
                mapping = aes(slope = Slope, intercept = Intercept, colour = Type)) +
    geom_point(data = df_netpos,
               mapping = aes(x = NetPosition[BiddingZone == BiddingZone1],
                             y = NetPosition[BiddingZone == BiddingZone2]),
               size = 3, shape = 21,
               fill = "#C5003E") +
    scale_x_continuous(name = element_blank(),
                       breaks = seq(-10000, 10000, 2500),
                       labels = c(paste0("-10.000 MW\n", BiddingZone1),
                                  format(seq(-7500, 7500, 2500), big.mark = ".", decimal.mark = ","),
                                  paste0("10.000 MW\n", BiddingZone1))) +
    scale_y_continuous(name = element_blank(),
                       breaks = seq(-10000, 10000, 2500),
                       labels = c(paste0("-10.000MW\n", BiddingZone2),
                                  format(seq(-7500, 7500, 2500), big.mark = ".", decimal.mark = ","),
                                  paste0("10.000 MW\n", BiddingZone2))) +
    scale_fill_manual(name = "Domain",
                      values = c("#53ad27", "#2753ad"),
                      labels = c("Flow-based", "Long-Term Allocations")) +
    scale_colour_manual(name = "CNEC",
                        values = c("#C5003E",
                                   "grey"),
                        labels = c("Active",
                                   "Presolved")) +
    coord_cartesian(xlim = c(-10000, 10000),
                    ylim = c(-10000, 10000)) +
    labs(title = "Presolved domain, active constraints and Core net positions",
         subtitle = paste0("on ", format(start, "%d %B %Y"),
                           " from ", format(start, "%H:%M"),
                           " to ", format(end, "%H:%M"),
                           " for the bidding zones ",
                           BiddingZone1, " (horizontally) and ",
                           BiddingZone2, " (vertically)"),
         caption = paste0("Figure by Nico Schoutteet | Data Core TSOs (JAO Publication Tool)\nSize of the presolved domain for selected directions: ",
                          format(round(FBdomainsize, 0), big.mark = ".", decimal.mark = ","),
                          " MW\u00B2\nSize of the LTA domain for selected directions:",
                          format(round(LTAdomainsize, 0), big.mark = ".", decimal.mark = ","),
                          "MW\u00B2")) +
    theme_classic() %+replace%
    theme(plot.title = element_text(size = 10, hjust = 0, colour = "#00B0B9"),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 8, hjust = 0, margin = margin(0, 0, 0.5, 0, "cm")),
          plot.caption = element_text(size = 5, hjust = 0, margin = margin(0, 0, .1, 0, "cm")),
          plot.caption.position = "plot",
          plot.margin = unit(c(.25, 1, .25, .25), "cm"),
          panel.grid = element_line(size = .25),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 8),
          axis.line = element_blank(),
          legend.title = element_text(size = 9, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          legend.key.size = unit(.25, "cm"),
          panel.grid.major = element_line(size = .35, colour = "grey", linetype = "dotted"))
  
}

# Per bidding zone border

JAOPuTo_domainvisualization_exchanges <- function(DateTime, Border1, Border2, return_dataframes = FALSE) {
  
  #define which hour to download
  start <- DateTime
  end <- start + hours(1)
  
  #download presolved domain with the helper function
  df_finaldomain <- JAOPuTo_finaldomain(start, end) %>%
    filter(cneEic != "NA")
  
  #calculate exchanges based on ptdf values corresponding to Border1 and Border2
  df_finaldomain <- df_finaldomain %>%
    mutate(Intercept = ram / (df_finaldomain[[paste0("ptdf_", substr(Border2, 1, 2))]] -
                                df_finaldomain[[paste0("ptdf_", substr(Border2, 6, 7))]]),
           Slope = - (df_finaldomain[[paste0("ptdf_", substr(Border1, 1, 2))]] -
                        df_finaldomain[[paste0("ptdf_", substr(Border1, 6, 7))]]) /
             (df_finaldomain[[paste0("ptdf_", substr(Border2, 1, 2))]] -
                df_finaldomain[[paste0("ptdf_", substr(Border2, 6, 7))]])) %>%
    mutate(CNEC = paste0(tso, ": ", cneName, " | ", contName, " - ", direction),
           Type = "Presolved",
           Location = ifelse(hubFrom == hubTo, "Internal", "Cross-border"),
           RAM = ram / fmax) %>%
    select(CNEC, TSO = tso, Location, Type, Intercept, Slope, RAM) %>%
    filter(!Intercept %in% c("Inf", "NaN", NA))
  
  #include dummy active constraint in case there are none (i.e. price convergence)
  df_shadowprices <- data.frame(CNEC = "DummyAC",
                                TSO = "DummyTSO",
                                Location = "Cross-border",
                                Type = "Active",
                                Intercept = Inf,
                                Slope = Inf,
                                RAM = 1,
                                ShadowPrice = 999)
  
  tryCatch({
    #download active constraints with this package's wrapper, escape in case there are none (i.e. price convergence)
    df_shadowprices <- JAOPuTo_shadowprices(start, end)
    
    #calculate exchanges based on ptdf values corresponding to Border1 and Border2
    df_shadowprices <- df_shadowprices %>%
      mutate(Intercept = ram / (df_shadowprices[[paste0("hub_", substr(Border2, 1, 2))]] -
                                  df_shadowprices[[paste0("hub_", substr(Border2, 6, 7))]]),
             Slope = - (df_shadowprices[[paste0("hub_", substr(Border1, 1, 2))]] -
                          df_shadowprices[[paste0("hub_", substr(Border1, 6, 7))]]) /
               (df_shadowprices[[paste0("hub_", substr(Border2, 1, 2))]] -
                  df_shadowprices[[paste0("hub_", substr(Border2, 6, 7))]])) %>%
      mutate(CNEC = paste0(tso, ": ", cnecName, " | ", contName, " - ", direction),
             Type = "Active",
             Location = ifelse(hubFrom == hubTo, "Internal", "Cross-border"),
             RAM = ram / fmax) %>%
      select(CNEC, TSO = tso, Location, Type, Intercept, Slope, RAM, ShadowPrice = shadowPrice)
    
  }, error = function(e) {})
  
  #combine presolved and active CNECs
  df_CNECs <- bind_rows(df_finaldomain, df_shadowprices) %>%
    filter(Intercept != "Inf")
  
  #download scheduled exchanges with this package's wrapper
  df_schexch <- JAOPuTo_schexch(start, end)
  
  #download LTA domain
  df_LTA <- JAOPuTo_lta(start, end) %>%
    filter(Border %in% c(Border1,
                         Border2,
                         paste0(substr(Border1, 6, 7), " - ", substr(Border1, 1, 2)),
                         paste0(substr(Border2, 6, 7), " - ", substr(Border2, 1, 2))))
  
  df_LTAdomain <- data.frame(Type = rep("LTA", 5),
                             x = c(df_LTA$LTA[df_LTA$Border == Border1],
                                   df_LTA$LTA[df_LTA$Border == Border1],
                                   - df_LTA$LTA[df_LTA$Border == paste0(substr(Border1, 6, 7), " - ", substr(Border1, 1, 2))],
                                   - df_LTA$LTA[df_LTA$Border == paste0(substr(Border1, 6, 7), " - ", substr(Border1, 1, 2))],
                                   df_LTA$LTA[df_LTA$Border == Border1]),
                             y = c(- df_LTA$LTA[df_LTA$Border == paste0(substr(Border2, 6, 7), " - ", substr(Border2, 1, 2))],
                                   df_LTA$LTA[df_LTA$Border == Border2],
                                   df_LTA$LTA[df_LTA$Border == Border2],
                                   - df_LTA$LTA[df_LTA$Border == paste0(substr(Border2, 6, 7), " - ", substr(Border2, 1, 2))],
                                   - df_LTA$LTA[df_LTA$Border == paste0(substr(Border2, 6, 7), " - ", substr(Border2, 1, 2))]))
  
  # functions to define x,y coordinates of innermost polygon (around origin)
  # explanation: https://stackoverflow.com/questions/74204298/how-do-i-find-the-surface-between-lines-based-on-intercept-and-slope-which-inclu/74350172#74350172
  innermost <- function(slopes, intercepts) {
    
    meetings <- function(slopes, intercepts) {
      meets_at <- function(i1, s1, i2, s2) {
        ifelse(s1 - s2 == 0, NA, (i2 - i1)/(s1 - s2))
      }
      xvals <- outer(seq_along(slopes), seq_along(slopes), function(i, j) {
        meets_at(intercepts[i], slopes[i], intercepts[j], slopes[j])
      })
      
      yvals <- outer(seq_along(slopes), seq_along(slopes), function(i, j) {
        intercepts + slopes *
          meets_at(intercepts[i], slopes[i], intercepts[j], slopes[j])
      })
      
      cbind(x = xvals[lower.tri(xvals)], y = yvals[lower.tri(yvals)])
    }
    xy <- meetings(slopes, intercepts)
    is_cut <- function(x, y, slopes, intercepts) {
      d <- sqrt(x^2 + y^2)
      slope <- y / x
      xvals <- intercepts / (slope - slopes)
      yvals <- xvals * slopes + intercepts
      ds <- sqrt(xvals^2 + yvals^2)
      any(d - ds > 1e-6 & sign(xvals) == sign(x) & sign(yvals) == sign(y))
    }
    xy <- xy[sapply(seq(nrow(xy)), function(i) {
      !is_cut(xy[i, 1], xy[i, 2], slopes, intercepts)
    }),]
    xy <- xy[order(atan2(xy[,2], xy[,1])),]
    as.data.frame(rbind(xy, xy[1,]))
  }
  
  #
  df_domain <- innermost(df_finaldomain$Slope, df_finaldomain$Intercept) %>%
    mutate(Type = "FB") %>%
    na.omit() %>%
    rbind(df_LTAdomain)
  
  #calculate size of presolved and LTA domains
  FBdomainsize <- st_as_sf(filter(df_domain, Type == "FB"), coords = c("x", "y")) %>%
    summarize(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>%
    st_area()
  
  LTAdomainsize <- st_as_sf(filter(df_domain, Type == "LTA"), coords = c("x", "y")) %>%
    summarize(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>%
    st_area()
  
  #return dataframes when specified TRUE in argument (default is FALSE)
  if (return_dataframes == TRUE) {
    df_schexch <<- df_schexch
    df_CNECs <<- df_CNECs
    df_domain <<- df_domain
    df_LTA <<- df_LTA
    FBdomainsize <<- FBdomainsize
    LTAdomainsize <<- LTAdomainsize
    
  }
  
  #visualize combined dataframe (presolved and active CNECs) and scheduled exchanges
  ggplot() +
    geom_polygon(data = df_domain,
                 mapping = aes(x = x, y = y, fill = Type),
                 alpha = .2) +
    geom_hline(yintercept = 0, colour = "black", size = .25) +
    geom_vline(xintercept = 0, colour = "black", size = .25) +
    geom_abline(data = df_CNECs,
                mapping = aes(slope = Slope, intercept = Intercept, colour = Type)) +
    geom_point(data = df_schexch,
               mapping = aes(x = SchExchanges[Border == Border1],
                             y = SchExchanges[Border == Border2]),
               size = 3, shape = 21,
               fill = "#C5003E") +
    scale_x_continuous(name = element_blank(),
                       breaks = seq(-10000, 10000, 2500),
                       labels = c(paste0("-10.000 MW\n", substr(Border1, 6, 7), " - ", substr(Border1, 1, 2)),
                                  format(seq(-7500, 7500, 2500), big.mark = ".", decimal.mark = ","),
                                  paste0("10.000 MW\n", Border1))) +
    scale_y_continuous(name = element_blank(),
                       breaks = seq(-10000, 10000, 2500),
                       labels = c(paste0("-10.000MW\n", substr(Border2, 6, 7), " - ", substr(Border2, 1, 2)),
                                  format(seq(-7500, 7500, 2500), big.mark = ".", decimal.mark = ","),
                                  paste0("10.000 MW\n", Border2))) +
    scale_fill_manual(name = "Domain",
                      values = c("#53ad27", "#2753ad"),
                      labels = c("Flow-based", "Long-Term Allocations")) +
    scale_colour_manual(name = "CNEC",
                        values = c("#C5003E",
                                   "grey"),
                        labels = c("Active",
                                   "Presolved")) +
    coord_cartesian(xlim = c(-10000, 10000),
                    ylim = c(-10000, 10000)) +
    labs(title = "Presolved domain, active constraints and scheduled exchanges",
         subtitle = paste0("on ", format(start, "%d %B %Y"),
                           " from ", format(start, "%H:%M"),
                           " to ", format(end, "%H:%M"),
                           " on the borders ",
                           Border1, " (horizontally) and ",
                           Border2, " (vertically)"),
         caption = paste0("Figure by Nico Schoutteet | Data Core TSOs (JAO Publication Tool)\nSize of the presolved domain for selected directions: ",
                          format(round(FBdomainsize, 0), big.mark = ".", decimal.mark = ","),
                          " MW\u00B2\nSize of the LTA domain for selected directions:",
                          format(round(LTAdomainsize, 0), big.mark = ".", decimal.mark = ","),
                          "MW\u00B2")) +
    theme_classic() %+replace%
    theme(plot.title = element_text(size = 10, hjust = 0, colour = "#00B0B9"),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 8, hjust = 0, margin = margin(0, 0, 0.5, 0, "cm")),
          plot.caption = element_text(size = 5, hjust = 0, margin = margin(0, 0, .1, 0, "cm")),
          plot.caption.position = "plot",
          plot.margin = unit(c(.25, 1, .25, .25), "cm"),
          panel.grid = element_line(size = .25),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 8),
          axis.line = element_blank(),
          legend.title = element_text(size = 9, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          legend.key.size = unit(.25, "cm"),
          panel.grid.major = element_line(size = .35, colour = "grey", linetype = "dotted"))
  
}
  
