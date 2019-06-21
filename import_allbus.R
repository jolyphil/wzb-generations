# ******************************************************************************
# Project: Generations, article for the WZB-Mitteilungen
# Task:    Import and Clean Allbus Data
# Author:  Philippe Joly, WZB & HU-Berlin
# ******************************************************************************

# 1. Download as registered user:
# https://dbk.gesis.org/DBKSearch/download.asp?db=D&id=64352
# "ZA4586_v1-0-0.dta"
# unzip and save to ./data/

library(tidyverse)
library(haven)
library(magrittr)
allbus <- read_dta("./data/ZA4586_v1-0-0.dta")

# pa02a       Interest in Politics (1 = very strong, 5 = not at all)
# pv01        Vote choice
# wghtptew    Person weight for east/west combined analysis
# wghtpt      Person weight for east/west seperated analysis

allbus %>%
  select(year, yborn, respid, pa02a, pv01, wghtptew, wghtpt) %>% 
  mutate(
    # Age and Generations
    yborn      = ifelse(yborn < 0, NA, yborn),
    age_doi    = year - yborn,
    generation = case_when(
      yborn >= 1925 & yborn <= 1939 ~ "Sceptical",
      yborn >= 1940 & yborn <= 1954 ~ "68er",
      yborn >= 1955 & yborn <= 1969 ~ "Boomer",
      yborn >= 1970 & yborn <= 1984 ~ "Xer",
      yborn >= 1985 ~ "Millennial"),
    generation = factor(
      generation, 
      levels = c("Sceptical", "68er", "Boomer", "Xer", "Millennial")
    ),
    # Interest in Politics
    polintr    = case_when(
      pa02a %in% c(1,2) ~ 1,
      pa02a %in% c(3:5) ~ 2,
      pa02a < 0 ~ NA_real_
    ),
    # Weight
    dweight = wghtptew
  ) %>% 
  saveRDS(file = "./data/allbus-reduced.rds")

rm(allbus)

