library(here)
source(here("scripts", "03rpart.R"))

#----save----
fs::dir_create(here("out"))
write_rds(data_cv, here("out", "data_cv.rds"))
