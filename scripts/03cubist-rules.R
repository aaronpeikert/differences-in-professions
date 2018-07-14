library(Cubist)
library(here)
source(here("scripts", "02preprocess.R"))

#----cubist-rules----
fit_cubist <- function(recipe, ...){
  cubist(x = juice(recipe, all_predictors(), composition = "data.frame"),
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
                  predicted_cubist = pmap(list(splits, recipes, cubist_models),
                                   pred_cubist),
                  rmse_cubist = map_dbl(predicted_cubist,
                                 ~sqrt(mean((.x$true - .x$predicted)^2))),
                  r2_cubist = map_dbl(predicted_cubist,
                                      ~cor(.x$true, .x$predicted))^2)
