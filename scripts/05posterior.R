library(here)
library(tidyposterior)
source(here("scripts", "04measures.R"))

#----acc-model----
acc <- select(data_cv, starts_with("id"), starts_with("acc"))
acc_stacked <- gather(acc)
acc_model <- perf_mod(acc)
acc_post <- tidy(acc_model)
acc_vs <- contrast_models(acc_model)

#----cramerv-model----
cramerv <- select(data_cv, starts_with("id"), starts_with("CramerV"))
cramerv_stacked <- gather(cramerv)
cramerv_model <- perf_mod(cramerv)
cramerv_post <- tidy(cramerv_model)
cramerv_vs <- contrast_models(cramerv_model)
