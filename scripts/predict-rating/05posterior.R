library(here)
library(tidyposterior)
source(here("scripts", "predict-rating", "04measures.R"))

#----acc-stacked----
acc <- select(data_cv, starts_with("id"), starts_with("acc"))
acc_stacked <- gather(acc)

#----acc-model----
acc_model <- perf_mod(acc)
acc_post <- tidy(acc_model)
acc_vs <- contrast_models(acc_model)

#----cramerv-stacked----
cramerv <- select(data_cv, starts_with("id"), starts_with("cramerv"))
cramerv_stacked <- gather(cramerv)

#----cramerv-model----
cramerv_model <- perf_mod(cramerv)
cramerv_post <- tidy(cramerv_model)
cramerv_vs <- contrast_models(cramerv_model)
