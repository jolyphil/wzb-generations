# ******************************************************************************
# Project: Generations, article for the WZB-Mitteilungen
# Task:    Generate graphs 
# Author:  Philippe Joly, WZB & HU-Berlin
# ******************************************************************************

library(tibble) # Dataframes
library(dplyr) # Data wrangling
library(ggplot2) # Graphs

# ______________________________________________________________________________
# Load data ====

ess <- readRDS("data/ess.rds")

# ______________________________________________________________________________
# Recode variables ====

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Generations ----

var_levels <- c("Boomer", "Xer", "Millennial")

ess <- ess %>%
  mutate(generation = case_when(yrbrn >= 1955 & yrbrn <= 1969 ~ "Boomer",
                                yrbrn >= 1970 & yrbrn <= 1984 ~ "Xer",
                                yrbrn >= 1985 ~ "Millennial"),
         generation = factor(generation, levels = var_levels))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Political interest ----

# Original code: 1	Very interested
#                2	Quite interested
#                3	Hardly interested
#                4	Not at all interested
#                7	Refusal
#                8	Don't know
#                9	No answer

ess <- ess %>%
  mutate(polintr = case_when(polintr %in% c(1,2) ~ 1,
                             polintr %in% c(3,4) ~ 0))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Political participation ----

# Original code: 1	Yes
#                2	No
#                7	Refusal
#                8	Don't know
#                9	No answer

recode_participation_var <- function(var) {
  r <- case_when(var == 1 ~ 1,
                 var == 2 ~ 0)
  return(r)
}

ess <- ess %>%
  mutate_at(vars(vote,
                 contplt,
                 wrkprty,
                 wrkorg,
                 badge,
                 sgnptit,
                 pbldmn,
                 bctprd,
                 clsprty), 
            recode_participation_var)
  
# ______________________________________________________________________________
# Collapse at the generation-round ====

ess_gr <- ess %>%
  filter(!is.na(generation)) %>%
  group_by(essround, generation) %>%
  summarize_at(vars(year,
                    polintr,
                    vote,
                    contplt,
                    wrkprty,
                    wrkorg,
                    badge,
                    sgnptit,
                    pbldmn,
                    bctprd,
                    clsprty), 
               ~weighted.mean(., dweight,  na.rm = T )) %>% 
  mutate(year = round(year))

# ______________________________________________________________________________
# Graphs ====

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Store variable labels ----

varname <- c("polintr",
             "vote",
             "contplt",
             "wrkprty",
             "wrkorg",
             "badge",
             "sgnptit",
             "pbldmn",
             "bctprd",
             "clsprty")
label <- c("Interested in politics (proportion)",
           "Voted in last national election (proportion)",
           "Contacted politician or government official, last 12 months (proportion)",
           "Worked in political party or action group, last 12 months (proportion)",
           "Worked in another organisation or association, last 12 months (proportion)",
           "Worn or displayed campaign badge/sticker, last 12 months (proportion)",
           "Signed petition, last 12 months (proportion)",
           "Taken part in lawful public demonstration, last 12 months (proportion)",
           "Boycotted certain products, last 12 months(proportion)",
           "Feel closer to a particular party (proportion)"
           )

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Loop over variables and export graphs ----

for(i in seq_along(varname)) {
  
  p <- ggplot(ess_gr, aes(x = year,
                          y = ess_gr[[varname[i]]], 
                          group = generation)) +
    geom_line(aes(color = generation)) +
    geom_point(aes(color = generation)) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = label[i]) + 
    scale_color_discrete(name="",
                         labels = c("Baby boomers (1955-69)", 
                                    "Generation X (1970-84)", 
                                    "Millennials (1985-...)"))
  
  filepath <- paste0("figures/", varname[i], ".png")
  ggsave(filepath, p)
  filepath <- paste0("figures/", varname[i], ".pdf")
  ggsave(filepath, p)
}

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Compute and plot "Age of Interview" graphs ----

plot_agedoi_freq <- function(.data, value){
  values <- enquo(value)
  
  .data %>% 
    filter(!is.na(generation)) %>%
    # Age at date of interview
    mutate(age_doi = year - yrbrn) %>% 
    # Compute weighted means
    group_by(generation, age_doi) %>% 
    count(!!values, wt = dweight) %>%
    filter(!is.na(!!values)) %>% 
    mutate(f = n/sum(n)) %>% 
    # Remove when to few cases
    filter(!(sum(n) < 30)) %>%
    # Clean
    filter(!!values == 1) %>% 
    select(-n, -!!values) %>% 
    # Generate plot
    ggplot(aes(
      x = age_doi, y = f,
      group = generation, color = generation
    )) + 
    geom_point() +
    geom_smooth(method = 'loess', formula = 'y ~ x') +
    labs(x = "Age at date of interview",
         y = tibble(varname, label) %>% 
           filter(varname == quo_name(values)) %>% .$label
    ) +
    scale_x_continuous()
  
    ggsave(paste0("./figures/agedoi-",quo_name(values),".png"))
}

plot_agedoi_freq(ess, polintr)
plot_agedoi_freq(ess, vote)
plot_agedoi_freq(ess, contplt)
plot_agedoi_freq(ess, wrkprty)
plot_agedoi_freq(ess, wrkorg)
plot_agedoi_freq(ess, badge)
plot_agedoi_freq(ess, sgnptit)
plot_agedoi_freq(ess, pbldmn)
plot_agedoi_freq(ess, bctprd)
plot_agedoi_freq(ess, clsprty)
