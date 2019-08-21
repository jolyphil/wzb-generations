library(tidyverse) # Used for data wrangling
library(essurvey) # Downloads main ESS datafiles

# Import all rounds -----------------------------------------------------------------
a <- import_all_rounds("ess@byom.de") # <-- Replace: your email has to be registred

# Limit variables and combine dataset -----------------------------------------------
b <- 
  a %>% 
  purrr::map(
    ~select(.x, essround, idno, matches("inwyys"), matches("inwyr"),
            cntry, dweight, polintr, vote, contplt, wrkprty, 
      wrkorg, badge, sgnptit, pbldmn, bctprd, clsprty, lrscale, stfdem, yrbrn, gndr,
      eisced, mnactic, mbtru, domicil, brncntr, intewde)
  ) %>% 
  purrr::map(recode_missings) %>% 
  bind_rows()

# Recode variables ------------------------------------------------------------------
recode_participation_var <- function(var) {
  case_when(
    var == 1 ~ 1,
    var == 2 ~ 0,
    var == 3 ~ NA_real_)
}

c <- 
  b %>% 
  mutate(
    # Fix year of interview
    year = ifelse(essround >= 3, inwyys, inwyr),
    # Age at date of interview
    age_doi = year - yrbrn,
    # Generation
    generation = case_when(
      yrbrn >= 1955 & yrbrn <= 1969 ~ "Boomer",
      yrbrn >= 1970 & yrbrn <= 1984 ~ "Xer",
      yrbrn >= 1985 ~ "Millennial"
    ),
    generation = factor(generation),
    # Political Interest
    polintr = case_when(
      polintr %in% c(1,2) ~ 1,
      polintr %in% c(3,4) ~ 0
    )
  ) %>% 
  select(-inwyys, -inwyr) %>% 
  mutate_at(
    vars(vote, contplt, wrkprty, wrkorg, badge, sgnptit, pbldmn, bctprd, clsprty), 
    recode_participation_var
  )

# Reduce country list ---------------------------------------------------------------
d <-  
  c %>% right_join(
  tibble(
    cntry = c(
      c("AT", "BE", "DE", "DK", "ES", "FI", "FR", "GB", "GR", "IE", "IT",
        "NL", "PT", "SE",
        "CZ", "BG", "HR", "HU", "LT", "PL", "SI", "SK")
      # EE, LV, and RO removed due to missing data in weights
    ),
    western_europe = c(rep(1, 14), rep(0, 8))
  ) %>% 
  mutate(cname_en = countrycode::countrycode(cntry, "iso2c", "country.name")),.,
  by = "cntry") %>% 
  filter(!is.na(cname_en))

# Country fluctuation
d %>% group_by(cname_en) %>% count(essround) %>% select(-n) %>% {table(.$essround)}

# Save ------------------------------------------------------------------------------
d %>% saveRDS(file.path("data", "ess_allctries.rds"))



