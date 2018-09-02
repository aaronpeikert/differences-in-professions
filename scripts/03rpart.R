library(rpartScore)
library(here)
source(here("scripts", "02preprocess.R"))

#----rpart----
fit_rpart <- function(recipe, ...){
  #browser()
  pred <- recipe %>% juice(all_predictors()) %>% names()
  out <- recipe %>% juice(all_outcomes()) %>% names()
  model_formula <- as.formula(paste0(out, " ~ ", paste(pred, collapse = " + ")))
  rpartScore(model_formula,
        data = juice(recipe, everything(), composition = "data.frame"),
        ...)
}
hypermat <- as.data.frame(expand.grid(split = c("abs"),
                                      prune = c("mr"), stringsAsFactors = FALSE))

data_cv <- data_cv %>%
  mutate(hypermat = list(hypermat)) %>% 
  unnest(hypermat, .preserve = c(splits, recipes1, recipes2))

data_cv <- mutate(data_cv,
                  rpart_models1 = pmap(list(recipe = recipes1, split = split, prune = prune), fit_rpart),
                  rpart_models2 = pmap(list(recipe = recipes2, split = split, prune = prune), fit_rpart))
