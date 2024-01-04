library(ggplot2)
library(dplyr)
library(tidyverse)
require(maptools)
library(mapdata)
library(maps)
library(mapproj)
library(gridExtra)
library(grid)
library(caret)
library(TMB)
library(INLA)
library(raster)
library(sf)
library(sdmTMB)
library(rgdal)
library(ggpubr)
library(ggmap)
library(corrplot )

rm(list = ls(all.names = TRUE))

# Table 4.1: All potential model MSPEs of the HMC.

# Run  hspm_cv, hstm_cv,...,hstmDTSS_cv R files first ! ! ! ! !

# create a table for all potential model MSPEs of the HMC
h0=read.csv("hspm_cvdata.csv")
h1=read.csv("hstm_cvdata.csv")
h2=read.csv("hstmD_cvdata.csv")
h3=read.csv("hstmT_cvdata.csv")
h4=read.csv("hstmDT_cvdata.csv")
h5=read.csv("hstmDTSS_cvdata.csv")

hres <- data.frame(
  "lon"=h1$lon,
  "lat"=h1$lat,
  "Year"=as.factor(h1$year),
  "SM"=h0$ height-h0$cv_predicted,
  "STM"=h1$ height-h1$cv_predicted,
  "STM-D"=h2$ height-h2$cv_predicted,
  "STM-T"=h3$ height-h3$cv_predicted,
  "STM-DT"=h4$ height-h4$cv_predicted,
  "STM-DTSS"=h5$ height-h5$cv_predicted
) %>%
  tidyr::gather(model,resid,-lon,-lat,-Year) %>%
  mutate(model = factor(model, ordered = T))

res.sp <- hres %>%
  group_by(Year, lon, lat, model) %>%
  summarise(m.resid = mean(resid), sd.resid = sd(resid),m.abs.resid = mean(abs(resid)),m.sq.resid = mean((resid)^2)) %>% 
  ungroup()
bind_rows(
  hres %>%
    group_by(Year, model) %>% 
    summarise(indiv.resid.mean = paste0(format(round(mean((resid^2)),4),nsmall=4, scientific=F))) %>%
    spread(model, indiv.resid.mean),
  hres %>%
    group_by(model) %>% 
    summarise(indiv.resid.mean = paste0(format(round(mean((resid^2)),4),nsmall=4, scientific=F))) %>%
    spread(model, indiv.resid.mean) %>% mutate(Year = "2012-2019")
) %>%
  xtable::xtable() %>%
  print(include.rownames=F) # Reorder the table in latex manually





# Table 4.2: All potential model MSPEs of the WMC.

# Run  spm_cv, stm_cv,...,stmDTSS_cv R files first ! ! ! ! !


w0=read.csv("spm_cvdata.csv")
w1=read.csv("stm_cvdata.csv")
w2=read.csv("stmD_cvdata.csv")
w3=read.csv("stmT_cvdata.csv")
w4=read.csv("stmDT_cvdata.csv")
w5=read.csv("stmDTSS_cvdata.csv")

res <- data.frame(
  "lon"=w1$lon,
  "lat"=w1$lat,
  "Year"=as.factor(w1$year),
  "SM"=w0$ weight-w0$cv_predicted,
  "STM"=w1$ weight-w1$cv_predicted,
  "STM-D"=w2$ weight-w2$cv_predicted,
  "STM-T"=w3$ weight-w3$cv_predicted,
  "STM-DT"=w4$ weight-w4$cv_predicted,
  "STM-DTSS"=w5$ weight-w5$cv_predicted
) %>%
  tidyr::gather(model,resid,-lon,-lat,-Year) %>%
  mutate(model = factor(model, ordered = T))

# WMC MSPE table
res.sq <- res %>%
  dplyr::group_by(Year, lon, lat, model) %>%
  dplyr::summarise(m.resid = mean(resid), m.abs.resid = mean(abs(resid)),m.sq.resid = mean((resid)^2)) %>% 
  ungroup()

bind_rows(
  res %>%
    dplyr::group_by(Year, model) %>% 
    dplyr::summarise(indiv.resid.mean = paste0(format(round(mean((resid^2)),4),nsmall=4, scientific=F))) %>%
    spread(model, indiv.resid.mean),
  res %>%
    group_by(model) %>% 
    dplyr::summarise(indiv.resid.mean = paste0(format(round(mean((resid^2)),4),nsmall=4, scientific=F))) %>%
    spread(model, indiv.resid.mean) %>% mutate(Year = "2012-2019")
) %>%
  xtable::xtable() %>%
  print(include.rownames=F)