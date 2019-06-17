library(dplyr) # Used for data wrangling
library(essurvey) # Downloads main ESS datafiles
library(ggplot2)

set_email("jolyphil@hu-berlin.de")

ess <- import_country("Germany", 8)

ess <- ess %>% 
  mutate(org = case_when(wrkorg == 1 ~ 1,
                         wrkorg == 2 ~ 0),
         intr = case_when(polintr %in% c(1,2) ~ 1,
                          polintr %in% c(3,4) ~ 0))

g <- ggplot(ess, aes(x = agea, y = imptrad)) +
  stat_smooth(method = "loess", formula = y ~ x, size = 1)
g
