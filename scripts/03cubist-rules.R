library(Cubist)
library(here)
source(here("scripts", "02preprocess.R"))

#----cubist-rules----
fit_cubist <- function(recipe, ...){
  cubist(x = juice(recipe, all_predictors(), composition = "matrix"),
         y = juice(recipe, all_outcomes(), composition = "matrix"),
         ...)
}
data_cv <- mutate(data_cv, cubist_models = map(recipes, fit_cubist))

pred_cubist <- function(split, recipe, model, ...) {
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
                  predicted = pmap(list(splits, recipes, cubist_models),
                                   pred_cubist),
                  rmse = map_dbl(predicted,
                                 ~sqrt(mean((.x$true - .x$predicted)^2))),
                  r2 = map_dbl(predicted, ~sqrt(cor(.x$true, .x$predicted))))
