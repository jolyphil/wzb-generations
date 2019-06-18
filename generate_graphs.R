# ******************************************************************************
# Project: Generations, article for the WZB-Mitteilungen
# Task:    Generate graphs 
# Author:  Philippe Joly, WZB & HU-Berlin
# ******************************************************************************

library(dplyr) # Used for data wrangling
library(ggplot2) # Used to generate graphs

# ______________________________________________________________________________
# Load data ====

ess <- readRDS("data/ess.rds")

# ______________________________________________________________________________
# Recode variables ====

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Age ----

ess <- ess %>%
  mutate(age = year - yrbrn)

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Generations ----

var_levels <- c("Boomer", "Xer", "Millennial")

ess <- ess %>%
  mutate(generation = case_when(yrbrn >= 1945 & yrbrn <= 1964 ~ "Boomer",
                                yrbrn >= 1965 & yrbrn <= 1984 ~ "Xer",
                                yrbrn >= 1985 ~ "Millennial"),
         generation = factor(generation, levels = var_levels))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Political participation ----

recode_action_var <- function(var) {
  r <- case_when(var == 1 ~ 1,
                 var == 2 ~ 0)
  return(r)
}

ess <- ess %>%
  mutate(vote = recode_action_var(vote),
         sgnptit = recode_action_var(sgnptit),
         bctprd = recode_action_var(bctprd),
         pbldmn = recode_action_var(pbldmn),
         clsprty = recode_action_var(clsprty))

# ______________________________________________________________________________
# Collapse at the generation-round ====

ess_gr <- ess %>%
  filter(!is.na(generation)) %>%
  group_by(essround, generation) %>%
  summarize(demonstration = mean(demonstration, na.rm = T),
            year = round(mean(year, na.rm = T)))

p <- ggplot(ess_gr, aes(x = year, y = demonstration, group = generation)) +
  geom_line(aes(color = generation))+
  geom_point(aes(color = generation))
p


