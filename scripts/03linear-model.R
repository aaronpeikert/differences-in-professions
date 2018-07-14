library(here)
source(here("scripts", "02preprocess.R"))

#----linear-model----
fit_lm <- function(recipe, ...){
  pred <- recipe %>% juice(all_predictors()) %>% names()
  out <- recipe %>% juice(all_outcomes()) %>% names()
  model_formula <- as.formula(paste0(out, " ~ ", paste(pred, collapse = " + ")))
  lm(model_formula,
     data = juice(recipe, everything(), composition = "data.frame"), ...)
}
data_cv <- mutate(data_cv, lm_models = map(recipes, fit_lm))

pred_lm <- function(split, recipe, model, ...) {
  mod_data <- bake(
    recipe, 
    newdata = assessment(split),
    all_predictors()
  )
  out <- bake(recipe, newdata = assessment(split), all_outcomes())
  out$predicted <- predict(model, newdata = mod_data, ...)
  names(out)[1] <- "true"
  return(out)
}
data_cv <- mutate(data_cv,
                  predicted_lm = pmap(list(splits, recipes, lm_models),
                                   pred_lm),
                  rmse_lm = map_dbl(predicted_lm,
                                 ~sqrt(mean((.x$true - .x$predicted)^2))),
                  r2_lm = map_dbl(predicted_lm,
                                  ~cor(.x$true, .x$predicted))^2)

