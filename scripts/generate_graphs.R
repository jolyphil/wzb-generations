# ******************************************************************************
# Project: Generations, article for the WZB-Mitteilungen
# Task:    Generate graphs 
# ******************************************************************************

library(tibble) # Dataframes
library(rlang)
library(dplyr) # Data wrangling
library(ggplot2) # Graphs
library(scales)

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
label <- c("Interested in politics",
           "Voted in last national election",
           "Contacted politician or government official, in last 12 months",
           "Worked in political party or action group, in last 12 months",
           "Worked in another organisation or association, in last 12 months",
           "Worn or displayed campaign badge/sticker, in last 12 months",
           "Signed a petition, in last 12 months",
           "Taken part in lawful public demonstration, in last 12 months",
           "Boycotted certain products, in last 12 months",
           "Feel close to a particular party"
           )

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Loop over variables and export graphs ----

plot_regular_freq <- function(.data, varname , label){
  
  gd <- .data %>% mutate(var = !!varname)
  
  gd %>%     
    ggplot(
      aes(x = year, y = var*100,
      group = generation)) +
    geom_line(aes(color = generation), size = 1.05) +
    geom_point(aes(color = generation), size = 3) +
    scale_x_continuous(
      name = NULL,
      limits = c(2003,2016),
      breaks = c(2003, seq(2004,2016,2))
    ) +
    scale_y_continuous(
      name = "Share in generation (in %)", limits = c(0, ceiling(max(gd$var)*100)),
      breaks=pretty_breaks()
    ) +
    scale_color_manual(
      values = rev(wesanderson::wes_palette("Darjeeling1",3)),
      labels = c("Baby boomers (1955-69)", "Generation X (1970-84)", "Millennials (1985-2000)")
    ) +
    labs(title = label, subtitle = "European Social Survey (2003-2016)", color = "Generation:") +
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

purrr::walk2(varname, label, ~plot_regular_freq(ess_gr, parse_quo(.x, current_env()), .y))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Compute and plot "Age of Interview" graphs ----

plot_agedoi_freq <- function(.data, value, label, fname = "agedoi"){
  values <- enquo(value)
  
  .data %>% 
    filter(!is.na(generation)) %>%
    # Compute weighted means
    group_by(generation, age_doi) %>% 
    count(!!values, wt = dweight) %>%
    filter(!is.na(!!values)) %>%
    mutate(f = n/sum(n)*100) %>%
    # Remove when too few cases
    filter(!(sum(n) < 30)) %>%
    # Clean
    filter(!!values == 1) %>%
    select(-n, -!!values) %>% 
    # Generate plot
    ggplot(aes(
      x = age_doi, y = f,
      group = rev(generation), color = rev(generation)
    )) +
    geom_point() +
    geom_smooth(method = 'loess', formula = 'y ~ x', se = FALSE) +
    scale_x_continuous(breaks = c(16,seq(20,60,10))) +
    scale_y_continuous(
      limits = c(0, NA),
      breaks = pretty_breaks()
    ) +
    scale_color_discrete(
      labels = c("Millennials (1985-2000)", 
                 "Generation X (1970-84)",
                 "Boomers (1955-69)")
    ) +
    coord_cartesian(xlim = c(16,60), expand = TRUE) +
    xlab("Age at date of interview") + ylab("Share in generation (in %)") +
    labs(
      title = label, subtitle = "European Social Survey (2003-2016)",
      color = "Generation:"
    ) +
    theme_bw() +
    theme(
      text = element_text(family = "Crimson", size = 12),
      plot.subtitle = element_text(face = "italic"),
      legend.position = "bottom"
    )

  ggsave(
    paste0("figures/", fname, "-", quo_name(values),".png"),
    width = 19, height = 19/1.35,
    dpi = 300, units = "cm"
  )
}

purrr::walk2(
  varname, label, 
  ~plot_agedoi_freq(ess, !!parse_quo(.x, env = current_env()), .y)
)

#plot_agedoi_freq(allbus, polintr, fname = "test")
#plot_agedoi_freq(allbus, mmbprty, fname = "agedoi-allbus")
