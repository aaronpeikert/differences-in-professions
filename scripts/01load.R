library(tidyverse)
library(here)

#----load----
data <- haven::read_sav(here("data", "Diplomarbeit_Welz_2011.sav"))
data <- rename(data, gesamt = gesamtbeurteilung_mean_gerundet_je_mehr_desto_mehr,
               alter = Alter_zum_Testzeitpunkt)
data <- mutate(data,
               Berufsgruppe = as.character(as_factor(Berufsgruppe)),
               Berufsgruppe = recode(Berufsgruppe,
                                     `Metall- und Elektroberufe` = "metal-electrical",
                                     Laborberufe = "lab",
                                     Produktionsberufe = "produktion",
                                     `Kaufmännische Berufe` = "commercial"))
data <- mutate_all(data, as.vector) # remove spss atributes, they make some problems in modelling
data <- rename(data,
               age = alter,
               profession = Berufsgruppe,
               verbal_intelligence = verbale_Intelligenz_HIT,
               numeric_intelligence = numerische_Intelligenz_HIT,
               rating = gesamt)

#----predictors----
OCEAN <- c("O", "C", "E", "A", "N")
predictors <- c(paste0(OCEAN, rep(1:6, length(OCEAN)))) # Personality Facets

model_formula <- as.formula(paste0("profession ~ ", paste(predictors, collapse = " + ")))
