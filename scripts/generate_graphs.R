# ******************************************************************************
# Project: Generations, article for the WZB-Mitteilungen
# Task:    Generate graphs 
# ******************************************************************************

library(tibble) # Dataframes
library(rlang)
library(dplyr) # Data wrangling
library(ggplot2) # Graphs

# ______________________________________________________________________________
# Load data ====

ess <- readRDS("data/ess.rds")
allbus <- readRDS("./data/allbus-reduced.rds")

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

plot_agedoi_freq <- function(.data, value, fname = "agedoi"){
  values <- enquo(value)
  
  .data %>% 
    filter(!is.na(generation)) %>%
    # Compute weighted means
    group_by(generation, age_doi) %>% 
    count(!!values, wt = dweight) %>%
    filter(!is.na(!!values)) %>% 
    mutate(f = n/sum(n)) %>% 
    # Remove when too few cases
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
  
    ggsave(paste0("./figures/",fname ,"-",quo_name(values),".png"))
}

c("polintr", "vote", "contplt", "wrkprty", "wrkorg", "badge", "sgnptit",
  "pbldmn", "bctprd", "clsprty") %>% 
  purrr::map(~plot_agedoi_freq(ess, !!parse_quo(.x, env = current_env())))

plot_agedoi_freq(allbus, polintr, fname = "agedoi-allbus")
plot_agedoi_freq(allbus, mmbprty, fname = "agedoi-allbus")
