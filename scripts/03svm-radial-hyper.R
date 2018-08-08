library(e1071)
library(here)
source(here("scripts", "02preprocess.R"))

#----svm-radial----
fit_svmr <- function(recipe, hypermat, ...){
  x <- juice(recipe, all_predictors(), composition = "data.frame")
  y <- as.vector(juice(recipe, all_outcomes(), composition = "matrix"))
  svm_ <- function(...)e1071::svm(x, y, ...)
  out <- pmap(hypermat, svm_)
  return(out)
}
hypermat <- as.data.frame(expand.grid(cost = c(0.1, .5, 1, seq(1, 100, 10), 500, 1000),
                        gamma = seq(0, 1, 1/10),
                        kernel = "radial",
                        scale = F,
                        stringsAsFactors = FALSE))
data_cv <- mutate(data_cv, svmr_models = map(recipes, fit_svmr, hypermat))

eval_fit_svmr <- function(split, recipe, models, ...) {
  mod_data <- bake(
    recipe, 
    newdata = assessment(split),
    all_predictors()
  )
  true <- bake(recipe, newdata = assessment(split), all_outcomes())
  predicted <- map(models, ~predict(.x, newdata = mod_data))
  rmse <- map_dbl(predicted, ~sqrt(mean((true - .x)^2)))
  r2 <- map_dbl(predicted,
                ~cor(true, .x))^2
  out <- data.frame(r2,
                    rmse,
                    cost = map_dbl(models, "cost"),
                    degree = map_dbl(models, "degree"),
                    gamma = map_dbl(models, "gamma"),
                    coef0 = map_dbl(models, "coef0"),
                    nu = map_dbl(models, "nu"),
                    epsilon = map_dbl(models, "epsilon"))
  return(out)
}

data_cv <- mutate(data_cv,
                  fit_svmr = pmap(list(splits, recipes, svmr_models),
                                          eval_fit_svmr))
