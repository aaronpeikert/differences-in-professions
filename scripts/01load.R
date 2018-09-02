library(tidyverse)
library(here)

#----load----
data <- haven::read_sav(here("data", "Diplomarbeit_Welz_2011.sav"))
data <- rename(data, gesamt = gesamtbeurteilung_mean_gerundet_je_mehr_desto_mehr,
               alter = Alter_zum_Testzeitpunkt)
data <- mutate_all(data, as.vector) # remove spss atributes, they make some problems in modelling
data <- mutate(data,
               Berufsgruppe = as.factor(Berufsgruppe),
               gesamt = as.numeric(.data$gesamt))

#----predictors----
OCEAN <- c("O", "C", "E", "A", "N")
predictors_i <- c("alter",
                "Berufsgruppe",
                "verbale_Intelligenz_HIT",
                "numerische_Intelligenz_HIT",
                #"Diktat", # even though diktat was box-cox tranformed, centered and scaled it lowers the performance of svm to zero
                paste0(OCEAN, rep(1:6, length(OCEAN)))) # Personality Facets
predictors_e <- predictors_i[-2]
model_formula_i <- as.formula(paste0("gesamt ~ ", paste(predictors_i, collapse = " + ")))
model_formula_e <- as.formula(paste0("gesamt ~ ", paste(predictors_e, collapse = " + ")))
