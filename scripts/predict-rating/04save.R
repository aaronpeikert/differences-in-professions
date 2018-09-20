library(here)
source(here("scripts", "predict-profession", "03rpart.R"))

#----save----
fs::dir_create(here("out", "predict-profession"))
write_rds(data_cv, here("out", "predict-profession", "data_cv.rds"))
