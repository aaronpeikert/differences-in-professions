library(here)
library(tidyposterior)
source(here("scripts", "04measures.R"))

#----acc_model----
acc <- select(data_cv, starts_with("id"), starts_with("acc"))
acc_stacked <- gather(acc)
ggplot(acc_stacked, aes(x = model, y = statistic, group = id, col = id)) + 
  geom_line(alpha = .75) + 
  theme_minimal() +
  theme(legend.position = "none") +
  NULL
acc_model <- perf_mod(acc)
acc_model$stan

acc_post <- tidy(acc_model)
ggplot(acc_post) + 
  # Add the observed data to check for consistency 
  geom_point(
    data = acc_stacked, 
    aes(x = model, y = statistic), 
    alpha = .5, col = "blue"
  )

acc_vs <- contrast_models(acc_model)
summary(acc_vs)
