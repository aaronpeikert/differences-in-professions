library(tidyverse)
library(here)

#----load----
data <- haven::read_sav(here("data", "Diplomarbeit_Welz_2011.sav"))
data <- rename(data, gesamt = gesamtbeurteilung_mean_gerundet_je_mehr_desto_mehr)
data <- mutate_all(data, as.vector) # remove spss atributes, they make some problems in modelling
data <- mutate(data, Berufsgruppe = as.factor(Berufsgruppe))

#----predictors----
OCEAN <- c("O", "C", "E", "A", "N")
predictors <- c("Berufsgruppe",
                "verbale_Intelligenz_HIT",
                "numerische_Intelligenz_HIT",
                "Diktat",
                paste0(OCEAN, rep(1:6, length(OCEAN)))) # Personality Facets

model_formula <- as.formula(paste0("gesamt ~ ", paste(predictors, collapse = " + ")))

