library(tidyverse)
library(here)

#----load----
data <- haven::read_sav(here("data", "Diplomarbeit_Welz_2011.sav"))
data <- rename(data, gesamt = gesamtbeurteilung_mean_gerundet_je_mehr_desto_mehr,
               alter = Alter_zum_Testzeitpunkt)
data <- mutate_all(data, as.vector) # remove spss atributes, they make some problems in modelling
data <- mutate(data,
               Berufsgruppe = as.factor(Berufsgruppe),
               gesamt = as.factor(.data$gesamt))

#----predictors----
OCEAN <- c("O", "C", "E", "A", "N")
predictors1 <- c("alter",
                "Berufsgruppe",
                "verbale_Intelligenz_HIT",
                "numerische_Intelligenz_HIT",
                #"Diktat", # even though diktat was box-cox tranformed, centered and scaled it lowers the performance of svm to zero
                paste0(OCEAN, rep(1:6, length(OCEAN)))) # Personality Facets
predictors2 <- predictors1[-2]
model_formula1 <- as.formula(paste0("gesamt ~ ", paste(predictors1, collapse = " + ")))
model_formula2 <- as.formula(paste0("gesamt ~ ", paste(predictors2, collapse = " + ")))
