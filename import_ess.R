# ******************************************************************************
# Project: Generations, article for the WZB-Mitteilungen
# Task:    Import and merge ESS data
# Author:  Philippe Joly, WZB & HU-Berlin
# ******************************************************************************

library(dplyr) # Used for data wrangling
library(essurvey) # Downloads main ESS datafiles

# ______________________________________________________________________________
# Save ESS email ====

ess_email <- "ess@byom.de" # <-- Replace: your email has to be
#                                            registered on the ESS
#                                            website.
# ______________________________________________________________________________
# Download and save ESS datasets ====

for (i in 1:8) {
  rootname <- paste0("ESS", i)
  rdafilepath <- file.path("data", "temp", paste0(rootname, ".rds"))
  import_country(country = "Germany", rounds = i, ess_email = ess_email) %>%
    saveRDS(file = rdafilepath)
}

# ______________________________________________________________________________
# Append datasets ====

for (i in 1:8) {
  
  # _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  # Import rds file ----
  
  round_i <- file.path("data", "temp", paste0("ESS", i, ".rds")) %>% 
    readRDS() %>%
    recode_missings()
  
  # _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  # year, harmonize variable name ----
  
  if (i < 3) { # inwyr, "year of interview" --> only in ESS1 and ESS2
    round_i <- round_i %>%
      mutate(year = inwyr)
    
  } else { # inwyys, "Start of interview, year" --> in essround > 2
    round_i <- round_i %>%
      mutate(year = inwyys)
  } 
  
  # Add old education var (in ESS 1 to 4) if missing
  if (any(names(round_i) == "edulvla") == F) { 
    round_i <- round_i %>%
      mutate(edulvla = NA)
  }
  
  # _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  # Select variables ----
  
  round_i <- round_i %>%
    dplyr::select(essround, 
                  idno, 
                  cntry, 
                  year,
                  dweight,
                  polintr,
                  vote,
                  contplt,
                  wrkprty,
                  wrkorg,
                  badge,
                  sgnptit,
                  pbldmn,
                  bctprd,
                  clsprty,
                  lrscale,
                  stfdem,
                  yrbrn,
                  gndr,
                  eisced,
                  edulvla,
                  mnactic,
                  mbtru,
                  domicil,
                  brncntr,
                  intewde)
  
  if (i == 1) {
    ess <- round_i
  } else {
    ess <- rbind(ess, round_i)
  }
  
}

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Remove temporary dataset ----
rm(round_i, i)

# ______________________________________________________________________________
# Save ESS data ====

ess %>% 
  saveRDS(file = file.path("data", "ess.rds"))
