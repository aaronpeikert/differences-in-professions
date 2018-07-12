library(tidyverse)
library(here)
#----load----
data1 <- haven::read_sav(here("data", "Diplomarbeit_Welz_2011.sav"))
data2 <- haven::read_sav(here("data", "Provadis.sav"))
data1 <- rename(data1, gesamt = gesamtbeurteilung_mean_gerundet_je_mehr_desto_mehr)
