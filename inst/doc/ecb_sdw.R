## ----options, echo=FALSE------------------------------------------------------
knitr::opts_chunk$set(fig.path = "", fig.width = 6, fig.height = 5, 
                      cache = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
library(ecb)
library(ggplot2)

## ----hicp_plot, eval=FALSE----------------------------------------------------
#  key <- "ICP.M.DE+FR+ES+IT+NL+U2.N.000000+XEF000.4.ANR"
#  filter <- list(lastNObservations = 12, detail = "full")
#  
#  hicp <- get_data(key, filter)
#  
#  hicp$obstime <- convert_dates(hicp$obstime)
#  
#  ggplot(hicp, aes(x = obstime, y = obsvalue, color = title)) +
#    geom_line() +
#    facet_wrap(~ref_area, ncol = 3) +
#    theme_bw(8) +
#    theme(legend.position = "bottom") +
#    labs(x = NULL, y = "Percent per annum\n", color = NULL,
#         title = "HICP - headline and core\n")

## ----get_dimensions_example, eval=FALSE---------------------------------------
#  dims <- get_dimensions("ICP.M.DE.N.000000+XEF000.4.ANR")
#  lapply(dims, head)

## ----retrieve_data, eval=FALSE------------------------------------------------
#  
#  unemp <- get_data("LFSI.M..S.UNEHRT.TOTAL0.15_74.T",
#                   filter = list(startPeriod = "2000"))
#  
#  wages <- get_data("MNA.A.N..W2.S1.S1._Z.COM_HW._Z._T._Z.IX.V.N",
#                   filter = list(startPeriod = "2000"))
#  
#  head(unemp)
#  head(wages)

## ----get_description_example, eval=FALSE--------------------------------------
#  desc <- head(get_description("LFSI.M..S.UNEHRT.TOTAL0.15_74.T"), 3)
#  strwrap(desc, width = 80)

## ----join_data, eval=FALSE----------------------------------------------------
#  suppressPackageStartupMessages(library(dplyr))
#  suppressPackageStartupMessages(library(lubridate))
#  
#  unemp <- unemp %>%
#    mutate(obstime = convert_dates(obstime)) %>%
#    group_by(ref_area, obstime = year(obstime)) %>%
#    summarise(obsvalue = mean(obsvalue)) %>%
#    ungroup() %>%
#    select(ref_area, obstime, "unemp" = obsvalue)
#  
#  wages <- wages %>%
#    mutate(obstime = as.numeric(obstime)) %>%
#    select(ref_area, obstime, "wage" = obsvalue)
#  
#  df <- left_join(unemp, wages)
#  head(df)

## ----phillips_plot, fig.width = 7, fig.height = 6, eval=FALSE-----------------
#  library(ggplot2)
#  
#  df %>%
#    filter(complete.cases(.)) %>%
#    group_by(ref_area) %>%
#    mutate(d_wage = c(NA, diff(wage)) / lag(wage),
#           d_unemp = c(NA, diff(unemp))) %>%
#    ggplot(aes(x = d_unemp, y = d_wage)) +
#    geom_point() +
#    facet_wrap(~ref_area, scales = "free") +
#    theme_bw(8) +
#    theme(strip.background = element_blank()) +
#    geom_smooth(method = "lm") +
#    labs(x = "\nAnnual change in unemployment", y = "Annual change in wages\n",
#         title = "Relationship between wages and unemployment\n")

