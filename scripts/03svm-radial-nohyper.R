library(e1071)
library(here)
source(here("scripts", "02preprocess.R"))

#----svm-radial----
fit_svmr <- function(recipe, ...){
  e1071::svm(x = juice(recipe, all_predictors(), composition = "data.frame"),
                 y = juice(recipe, all_outcomes(), composition = "matrix"),
                 ...)
}
data_cv <- mutate(data_cv, svmr_models = map(recipes, fit_svmr, scale = F))

pred_svmr <- function(split, recipe, model, ...) {
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
                  predicted_svmr = pmap(list(splits, recipes, svmr_models),
                                          pred_svmr),
                  rmse_svmr = map_dbl(predicted_svmr,
                                        ~sqrt(mean((.x$true - .x$predicted)^2))),
                  r2_svmr = map_dbl(predicted_svmr,
                                      ~cor(.x$true, .x$predicted))^2)
