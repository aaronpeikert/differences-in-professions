library(rpart)
library(here)
source(here("scripts", "02preprocess.R"))

#----rpart----
fit_rpart <- function(recipe, ...){
  #browser()
  pred <- recipe %>% juice(all_predictors()) %>% names()
  out <- recipe %>% juice(all_outcomes()) %>% names()
  model_formula <- as.formula(paste0(out, " ~ ", paste(pred, collapse = " + ")))
  rpart(model_formula,
        data = juice(recipe, everything(), composition = "data.frame"),
        method = "class",
        ...)
}
hypermat <- as.data.frame(expand.grid(minsplit = c(10, 15, 20, 25, 30, 35, 40, 50),
                                      minbucket = c(5, 10, 15, 20, 25, 30)))

data_cv <- data_cv %>%
  mutate(hypermat = list(hypermat)) %>% 
  unnest(hypermat, .preserve = c(splits, recipes))

data_cv <- mutate(data_cv, rpart_models = pmap(list(recipe = recipes, minsplit = minsplit, minbucket = minbucket), fit_rpart))
