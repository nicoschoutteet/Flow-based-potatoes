# libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(lubridate)
library(scales)

# JAO Publication Tool API ------------------------------------------------

# These functions connect to the JAO PuTo API to download the source data

# Final presolved domains

JAOPuTo_finaldomain <- function(start_DateTime,
                                end_DateTime) {
  
  df_finaldomain <- data.frame()
  
  FinalDomain <- httr::GET("https://publicationtool.jao.eu/core/api/data/finalComputation?Filter=%7B'Presolved'%3Atrue%7D&",
                           query = list(FromUtc = format(with_tz(start_DateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                        ToUtc = format(with_tz(end_DateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"))) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()
  
  df_finaldomain <- FinalDomain$data %>%
    mutate(DateTime = with_tz(as.POSIXct(dateTimeUtc,
                                         format = "%Y-%m-%dT%H:%M:%SZ",
                                         tz = "UTC"),
                              "CET"),
           tso = recode(tso,
                        "50HERTZ" = "50Hertz",
                        "AMPRION" = "Amprion",
                        "ELIA" = "Elia",
                        "TENNETBV" = "TenneT BV",
                        "TENNETGMBH" = "TenneT GmbH",
                        "TRANSELECTRICA" = "Transelectrica",
                        "TRANSNETBW" = "TransnetBW")) %>%
    select(id, DateTime, everything(), -dateTimeUtc) %>%
    return()
}

# shadow prices

JAOPuTo_shadowprices <- function(start_DateTime,
                                 end_DateTime) {
  
  df_shadowprices <- data.frame()
  
  ShadowPrices <- httr::GET("https://publicationtool.jao.eu/core/api/data/shadowPrices",
                            query = list(Filter = {},
                                         Skip = "",
                                         Take = "",
                                         FromUtc = format(with_tz(start_DateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                         ToUtc = format(with_tz(end_DateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()
  
  df_shadowprices <- ShadowPrices$data %>%
    mutate(DateTime = with_tz(as.POSIXct(dateTimeUtc,
                                         format = "%Y-%m-%dT%H:%M:%S",
                                         tz = "UTC"),
                              "CET"),
           tso = recode(tso,
                        "50HERTZ" = "50Hertz",
                        "Apg" = "APG",
                        "Ceps" = "CEPS",
                        "Eles" = "ELES",
                        "Hops" = "HOPS",
                        "Mavir" = "MAVIR",
                        "Pse" = "PSE",
                        "Rte" = "RTE",
                        "Seps" = "SEPS",
                        "TennetBv" = "TenneT BV",
                        "TennetGmbh" = "TenneT GmbH",
                        "TransnetBw" = "TransnetBW")) %>%
    select(DateTime, everything(), -id, -dateTimeUtc) %>%
    return()
}

# Core net positions

JAOPuTo_netpositions <- function(start_DateTime,
                                 end_DateTime) {
  
  df_netpositions <- data.frame()
  
  NetPositions <- httr::GET("https://publicationtool.jao.eu/core/api/data/netPos",
                            query = list(FromUtc = format(with_tz(start_DateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                         ToUtc = format(with_tz(end_DateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()
  
  df_netpositions <- NetPositions$data %>%
    mutate(DateTime = with_tz(as.POSIXct(dateTimeUtc,
                                         format = "%Y-%m-%dT%H:%M:%S",
                                         tz = "UTC"),
                              "CET")) %>%
    select(DateTime, everything(), -id, -dateTimeUtc) %>%
    tidyr::gather(-DateTime, key = "BiddingZone", value = "NetPosition") %>%
    mutate(BiddingZone = recode(BiddingZone,
                                "hub_ALBE" = "ALEGrO Belgium",
                                "hub_ALDE" = "ALEGrO Germany",
                                "hub_AT" = "Austria",
                                "hub_BE" = "Belgium",
                                "hub_CZ" = "Czech Republic",
                                "hub_DE" = "Germany/Luxembourg",
                                "hub_FR" = "France",
                                "hub_HR" = "Croatia",
                                "hub_HU" = "Hungary",
                                "hub_NL" = "Netherlands",
                                "hub_PL" = "Poland",
                                "hub_RO" = "Romania",
                                "hub_SI" = "Slovenia",
                                "hub_SK" = "Slovakia")) %>%
    return()
}

# Scheduled exchanges

JAOPuTo_schexch <- function(start_DateTime,
                            end_DateTime) {
  
  df_schexch <- data.frame()
  
  SchExch <- httr::GET("https://publicationtool.jao.eu/core/api/data/scheduledExchanges",
                       query = list(FromUtc = format(with_tz(start_DateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                    ToUtc = format(with_tz(end_DateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()
  
  df_schexch <- SchExch$data %>%
    mutate(DateTime = with_tz(as.POSIXct(dateTimeUtc,
                                         format = "%Y-%m-%dT%H:%M:%S",
                                         tz = "UTC"),
                              "CET")) %>%
    select(DateTime, everything(), -id, -dateTimeUtc) %>%
    tidyr::gather(-DateTime, key = "Border", value = "SchExchanges") %>%
    mutate(Border = paste0(str_split(Border, "_", simplify = TRUE)[,2], " - ", str_split(Border, "_", simplify = TRUE)[,3])) %>%
    filter(!Border %in% c("DE - DK1", "DK1 - DE", "ES - FR", "FR - ES")) %>%
    tidyr::separate(Border, into = c("b1", 'b2'), sep = "\\s+-\\s+", remove = FALSE) %>%
    group_by(DateTime, b1new = pmin(b1, b2), b2new = pmax(b1, b2)) %>%
    mutate(SchExchanges = replace(SchExchanges, SchExchanges == 0, -SchExchanges[SchExchanges != 0])) %>%
    ungroup %>%
    select(DateTime, Border, SchExchanges) %>%
    arrange(DateTime) %>%
    return()
}

# Long-term allocations

JAOPuTo_lta <- function(start_DateTime,
                        end_DateTime) {
  
  df_lta <- data.frame()
  
  LTA <- httr::GET("https://publicationtool.jao.eu/core/api/data/lta",
                   query = list(FromUtc = format(with_tz(start_DateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                ToUtc = format(with_tz(end_DateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()
  
  df_lta <- LTA$data %>%
    mutate(DateTime = with_tz(as.POSIXct(dateTimeUtc,
                                         format = "%Y-%m-%dT%H:%M:%S",
                                         tz = "UTC"),
                              "CET")) %>%
    select(DateTime, everything(), -id, -dateTimeUtc) %>%
    tidyr::gather(-DateTime, key = "Border", value = "LTA") %>%
    mutate(Border = paste0(substr(Border, 8, 9), " - ", substr(Border, 11, 12))) %>%
    return()
}
