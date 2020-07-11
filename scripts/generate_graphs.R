# ******************************************************************************
# Project: Generations, article for the WZB-Mitteilungen
# Task:    Generate graphs 
# ******************************************************************************

library(tibble) # Dataframes
library(rlang)
library(dplyr) # Data wrangling
library(ggplot2) # Graphs
library(scales)
library(magrittr)

# ______________________________________________________________________________
# Load data ====

ess_allctries <- readRDS("data/ess_allctries_19.rds")
#ess <- readRDS("data/ess.rds")
#allbus <- readRDS("./data/allbus-reduced.rds")

ess <- ess_allctries %>% 
  filter(!is.na(dweight) & !is.na(year)) %>% 
  filter(age_doi > 15) %>% 
  filter(western_europe == 1)

# ______________________________________________________________________________
# Collapse at the generation-round ====
ess_gr <-
 ess %>%
  filter(!is.na(generation)) %>%
  group_by(essround, cname_en, generation) %>%
  summarize_at(vars(year, polintr, vote, contplt, wrkprty, wrkorg, badge,
                    sgnptit, pbldmn, bctprd, clsprty),
               ~weighted.mean(., dweight,  na.rm = T )) %>% 
  group_by(essround, generation) %>% 
  select(-cname_en) %>% dplyr::summarize_all(mean, na.rm = TRUE) %>% 
  mutate(year = (essround * 2) + 2000)

# ______________________________________________________________________________
# Graphs ====

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Define Langauge and variable labels ----
# 
language <- "de"

vars <- tibble(
  varname = c("polintr", "vote", "contplt", "wrkprty", "wrkorg", "badge",
              "sgnptit", "pbldmn", "bctprd", "clsprty"),
  title_en = c("Interested in politics",
               "Voted in last national election", 
               "Contacted politician or government official, in last 12 months",
               "Worked in political party or action group, in last 12 months",
               "Worked in another organisation or association, in last 12 months",
               "Worn or displayed campaign badge/sticker, in last 12 months",
               "Signed a petition, in last 12 months",
               "Taken part in lawful public demonstration, in last 12 months",
               "Boycotted certain products, in last 12 months",
               "Feel close to a particular party"),
  title_de = c("Politisches Interesse",
               "Wahlteilnahme (letzte nationale Wahlen)",
               "Kontakt zu einem Politiker oder einer Amtsperson aufgenommen",
               "In einer politischen Partei oder Gruppierung mitgearbeitet",
               "In einer anderen Organisation oder Ähnlichem mitgearbeitet",
               "Abzeichen/Aufkleber einer politischen Kampagne getragen",
               "Bürgerbegehren oder Volksbegehren unterschrieben",
               "Teilnahme an genehmigter öffentlicher Demonstration",
               "Boykott bestimmter Produkte",
               "Nähe zu einer Partei"
  )
)

labels <- list(
  yname_en = "Share in generation (in %)",
  yname_de = "Anteil an Generation (in %)",
  xname_en = "Age at date of interview",
  xname_de = "Alter am Tag des Interviews",
  color_en = c("Baby boomers (1955-69)", "Generation X (1970-84)", "Millennials (1985-2000)"),
  color_de = c("Baby Boomer (1955-69)", "Generation X (1970-84)", "Millennials (1985-2000)")
)

# Switch language
vars %<>%
  rename_with(~stringr::str_remove( ., paste0("_",language))) %>% 
  select(!contains("_"))
labels %<>% 
  purrr::set_names(~stringr::str_remove( ., paste0("_",language))) %>% 
  .[!stringr::str_detect(names(.), "_")]

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Loop over variables and export graphs ----

plot_regular_freq <- function(.data, varname, vars, labels ){
  
  gd <- .data %>% mutate(var = !!varname)
  varname_quo  <- as_name({{ varname }})
  labels$title <- vars %>% filter(varname == varname_quo) %>% pull(title)
  
  gd %>%
    ggplot(
      aes(x = year, y = var*100,
      group = generation)) +
    geom_line(aes(color = generation), size = 1.05) +
    geom_point(aes(color = generation), size = 3) +
    scale_x_continuous(
      name = NULL,
      limits = c(2002,2018),
      breaks = seq(2002,2018,2)
    ) +
    scale_y_continuous(
      limits = c(0, ceiling(max(gd$var)*100)),
      breaks=pretty_breaks()
    ) +
    scale_color_manual(
      values = rev(wesanderson::wes_palette("Darjeeling1",3)),
      label = labels$color
    ) +
    labs(
      title = labels$title,
      y = labels$yname,
      subtitle = "European Social Survey (2002-2018)",
      color = "Generation:"
    ) +
    theme_bw() +
    theme(
      text = element_text(family = "Crimson", size = 12),
      plot.subtitle = element_text(face = "italic"),
      legend.position = "bottom"
    )

  ggsave(
    paste0("figures/", quo_name(varname),".png"),
    width = 19, height = 19/1.35,
    dpi = 300, units = "cm"
  )
  #filepath <- paste0("figures/", varname[i], ".pdf")
  #ggsave(filepath, p)
}

purrr::walk(vars$varname, ~plot_regular_freq(ess_gr, parse_quo(.x, current_env()), vars, labels))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Compute and plot "Age of Interview" graphs ----

plot_agedoi_freq <- function(.data, varname, vars, labels, fname = "agedoi"){
  
  varname_quo  <- as_name({{ varname }})
  labels$title <- vars %>% filter(varname == varname_quo) %>% pull(title)
  
  .data %>% 
    filter(!is.na(generation)) %>% 
    # Remove last age_doi of a generation
    filter(!(age_doi %in% c(64,65))) %>%
    filter(!(generation == "Millennial" & age_doi >= 34)) %>%
    filter(!(generation == "Xer" & age_doi >= 49)) %>%
    # Group age_doi due to small n (to be discussed)
    #mutate(
    #  age_doi = cut(age_doi,
    #    breaks = c(14, seq(20,65,3)), 
    #    labels = c(14, seq(20,62,3)
    #  )
    #)) %>% 
    filter(!is.na(age_doi)) %>% 
    # Compute weighted means
    group_by(cname_en, generation, age_doi) %>%
    count(!!varname, wt = dweight) %>%
    filter(!is.na(!!varname)) %>% 
    mutate(f = n/sum(n)) %>%
    filter(!!varname == 1) %>%
    group_by(generation, age_doi) %>%
    summarize(f = mean(f)*100) %>% 
    # Recode age_doi back to numeric and center data point
    #mutate(age_doi = as.numeric(as.character(age_doi))+2) %>%
    # Generate plot
    ggplot(aes(
      x = age_doi, y = f,
      group = generation, color = (generation)
    )) +
    geom_point() +
    geom_smooth(method = 'loess', formula = 'y ~ x', se = TRUE) +
    scale_x_continuous(breaks = c(16, seq(20,60,5), 64)) +
    scale_y_continuous(
      limits = c(0, NA),
      breaks = pretty_breaks()
    ) +
    scale_color_discrete(
      labels = labels$color
    ) +
    coord_cartesian(xlim = c(16,64), expand = TRUE) +
    labs(
      title = labels$title, subtitle = "European Social Survey (2002-2018)",
      x = labels$xname, y = labels$yname,
      color = "Generation:"
    ) +
    theme_bw() +
    theme(
      text = element_text(family = "Crimson", size = 12),
      plot.subtitle = element_text(face = "italic"),
      legend.position = "bottom"
    )

  ggsave(
    paste0("figures/", fname, "-", quo_name(varname),".png"),
    width = 19, height = 19/1.35,
    dpi = 300, units = "cm"
  )
}

purrr::walk(
  vars$varname,
  ~plot_agedoi_freq(ess, parse_quo(.x, env = current_env()), vars, labels)
)

a

#plot_agedoi_freq(allbus, polintr, fname = "test")
#plot_agedoi_freq(allbus, mmbprty, fname = "agedoi-allbus")
