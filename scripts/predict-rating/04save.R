library(here)
source(here("scripts", "predict-rating", "03rpart.R"))

#----save----
fs::dir_create(here("out", "predict-rating"))
write_rds(data_cv, here("out", "predict-rating", "data_cv.rds"))
