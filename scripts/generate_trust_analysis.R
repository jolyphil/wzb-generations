# ******************************************************************************
# Project: Fork from Generations, article for the WZB-Mitteilungen
# Task:    Generate Graphs similar to article, but for trust items and Germany
# ******************************************************************************

library(tibble) # Dataframes
library(rlang)
library(dplyr) # Data wrangling
library(ggplot2) # Graphs
library(scales)

# ______________________________________________________________________________
# Load data ====
ess_allctries <- readRDS("data/ess_allctries.rds")

ess_ger <- 
  ess_allctries %>%
  select(cntry:idno,trstprl:trstun,dweight, year, yrbrn, age_doi, generation) %>% 
  filter(cname_en == "Germany") %>% 
  filter(!is.na(dweight) & !is.na(generation)) %>% 
  mutate_at(vars(matches("trst")), .funs = list(binary = function(x) ifelse(x > 6, 1, 0)  ))

# ______________________________________________________________________________
# Collapse at the generation-round ====
ess_gr <-
  ess_ger %>%
  #mutate(dweight = 1) %>% 
  group_by(essround, cname_en, generation) %>%
  summarize_at(vars(year, matches("_binary")), 
               ~weighted.mean(., dweight,  na.rm = T )) %>% 
  group_by(essround, generation) %>% 
  select(-cname_en) %>% dplyr::summarize_all(mean, na.rm = TRUE) %>% 
  mutate(year = round(year,0))

# ______________________________________________________________________________
# Graphs ====

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Store variable labels ----

varname <- colnames(ess_ger %>% select(matches("binary")))
label <- c("Vertrauen in den Bundestag",
           "Vertrauen in Gerichte und Rechtsprechung",
           "Vertrauen in Polizei",
           "Vertrauen in Politikerinnen und Politiker",
           "Vertrauen in die Europäische Union",
           "Vertrauen in die Vereinigten Nationen"
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
      limits = c(2003,2018),
      breaks = c(seq(2002,2018,2))
    ) +
    scale_y_continuous(
      name = "Anteil (in %)", limits = c(0, ceiling(max(gd$var)*100)),
      breaks=pretty_breaks()
    ) +
    scale_color_manual(
      values = rev(wesanderson::wes_palette("Darjeeling1",3)),
      labels = c("Baby-Boomer (1955-69)", "Generation X (1970-84)", "Millennials (1985-2000)")
    ) +
    labs(title = label, subtitle = "European Social Survey (2002-2018)", color = "Generation:") +
    theme_bw() +
    theme(
      text = element_text(family = "Crimson", size = 12),
      plot.subtitle = element_text(face = "italic"),
      legend.position = "bottom"
    )
  
  ggsave(
    filename = paste0("figures/", quo_name(varname),".png"),
    width = 19, height = 19/1.35,
    dpi = 300, units = "cm"
  )
}

purrr::walk2(varname, label, ~plot_regular_freq(ess_gr, parse_quo(.x, current_env()), .y))

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Compute and plot "Age of Interview" graphs ----

plot_agedoi_freq <- function(.data, value, label, fname = "agedoi"){
  values <- enquo(value)
  
  .data %>% 
    filter(!is.na(generation)) %>%
    # Compute weighted means
    group_by(cname_en, generation, age_doi) %>%
    count(!!values, wt = dweight) %>%
    filter(!is.na(!!values)) %>% 
    mutate(f = n/sum(n)) %>%
    filter(!!values == 1) %>%
    group_by(generation, age_doi) %>%
    summarize(f = mean(f)*100) %>%
    filter(age_doi > 15) %>% 
    # Generate plot
    ggplot(aes(
      x = age_doi, y = f,
      group = generation, color = (generation)
    )) +
    geom_point() +
    geom_smooth(method = 'loess', formula = 'y ~ x', se = TRUE) +
    scale_x_continuous(breaks = c(16,seq(20,60,10))) +
    scale_y_continuous(
      limits = c(0, NA),
      breaks = pretty_breaks()
    ) +
    scale_color_discrete(
      labels = c("Boomers (1955-69)", "Generation X (1970-84)", "Millennials (1985-2000)")
    ) +
    coord_cartesian(xlim = c(16,60), expand = TRUE) +
    xlab("Age at date of interview") + ylab("Share in generation (in %)") +
    labs(
      title = label, subtitle = "European Social Survey (2002-2016)",
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
  ~plot_agedoi_freq(ess_ger, !!parse_quo(.x, env = current_env()), .y)
)

#plot_agedoi_freq(allbus, polintr, fname = "test")
#plot_agedoi_freq(allbus, mmbprty, fname = "agedoi-allbus")


## Trust in Parliament as slide ------------
library(ggpubr)

plot1 <- 
  ess_gr %>% 
  mutate(var = trstprl_binary) %>% 
  ggplot(
    aes(x = year, y = var*100,
        group = generation)) +
  geom_line(aes(color = generation), size = 1.05) +
  geom_point(aes(color = generation), size = 3) +
  scale_x_continuous(
    name = "Befragungsjahr",
    limits = c(2003,2018),
    breaks = c(seq(2002,2018,2))
  ) +
  scale_y_continuous(
    name = "Anteil (in %)", limits = c(0, 50),
    breaks=pretty_breaks()
  ) +
  scale_color_manual(
    values = rev(wesanderson::wes_palette("Darjeeling1",3)),
    labels = c("Baby-Boomer (1955-69)", "Generation X (1970-84)", "Millennials (1985-2000)")
  ) +
  labs(color = "Generation:")
plot1

plot2 <- 
  ess_ger %>% 
  filter(!is.na(generation)) %>%
  # Compute weighted means
  group_by(cname_en, generation, age_doi) %>%
  count(trstprl_binary, wt = dweight) %>%
  filter(n >= 3) %>% 
  mutate(f = n/sum(n)) %>%
  filter(trstprl_binary == 1) %>%
  group_by(generation, age_doi) %>%
  summarize(f = mean(f)*100) %>%
  filter(age_doi > 15) %>% 
  # Generate plot
  ggplot(aes(
    x = age_doi, y = f,
    group = generation, color = (generation)
  )) +
  geom_point() +
  geom_smooth(method = 'loess', formula = 'y ~ x', se = TRUE) +
  scale_x_continuous(breaks = c(16,seq(20,60,10))) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = pretty_breaks()
  ) +
  scale_color_manual(
    values = rev(wesanderson::wes_palette("Darjeeling1",3)),
    labels = c("Baby-Boomer (1955-69)", "Generation X (1970-84)", "Millennials (1985-2000)")
  ) +
  coord_cartesian(xlim = c(16,60), expand = TRUE) +
  xlab("Alter zum Zeitpunkt des Interviews") + ylab("") +
  labs(
    color = "Generation:"
  )
plot2

addTheme <- function(plot){
  plot + theme_bw() +
    theme(
      text = element_text(family = "Crimson", size = 12),
      axis.title.x = element_text(size = 8, margin = margin(.3,0,0,0,"cm")),
      axis.title.y = element_text(size = 8),
      axis.text = element_text(size = 7),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7),
      legend.position = "bottom",
      legend.box.margin = margin(-3,0,0,0)
    )
}

addTheme(plot1)

ggarrange(
  addTheme(plot1), addTheme(plot2),
  font.label = list(
    size = 11, color = "black", face =
      "plain", family = NULL
  ),
  common.legend = TRUE, legend = "bottom",
  ncol = 2, nrow = 1
)

ggsave(
  "./figures/trstprl_binary_slides.png",
  width = 16, height = 10, units = "cm",
  scale = 1, dpi = 600
)




