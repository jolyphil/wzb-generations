# ******************************************************************************
# Project: Generations, article for the WZB-Mitteilungen
# Task:    Function to obtain subpopulation means with correct SEs 
# ******************************************************************************

# Note:    So far we have used the weighted arithmetic mean -- weighted.mean()
#          to obtain our data points for each generation.
#          This works well as long as we ignore the standard errors on the mean. 
#          If we ever need proper confidence intervals, we might consider
#          using this function which takes into account the survey design.

subpop_mean_ess <- function(df, subpop, var){
  
  require(dplyr) # data wrangling
  require(survey) # complex survey operations
  
  # Save survey design
  dfpc <- svydesign(id = ~essround, weights= ~dweight, data = df)
  
  # Save subpopulation
  dsub <- subset(dfpc, eval(parse(text = subpop)))
  
  # Extract mean of subpopulation
  svymean(~eval(parse(text = var)), design=dsub, na.rm=T)
}

# ______________________________________________________________________________
# Example ====

# ess <- readRDS("data/ess.rds") %>%
#   mutate(genX = case_when(yrbrn >= 1970 & yrbrn <= 1984 ~ T,
#                                                       T ~ F))
# subpop_mean_ess(ess, "genX", "yrbrn")