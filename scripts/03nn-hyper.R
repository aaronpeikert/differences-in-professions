library(neuralnet)
library(here)
source(here("scripts", "02preprocess.R"))

#----nn----
fit_nn <- function(recipe, ...){
  pred <- recipe %>% juice(all_predictors()) %>% names()
  out <- recipe %>% juice(all_outcomes()) %>% names()
  model_formula <- as.formula(paste0(out, " ~ ", paste(pred, collapse = " + ")))
  neuralnet(model_formula,
     data = juice(recipe, everything(), composition = "data.frame"), ...)
}
hypermat <- as.data.frame(expand.grid(hidden = 1:30))

data_cv <- data_cv %>%
  mutate(hypermat = list(hypermat)) %>% 
  unnest(hypermat, .preserve = c(splits, recipes))

data_cv <- mutate(data_cv,
                  nn_models = pmap(list(recipe = recipes,
                                          hidden = hidden),
                                     fit_nn))

pred_nn <- function(split, recipe, model, ...) {
  mod_data <- bake(
    recipe, 
    newdata = assessment(split),
    all_predictors()
  )
  out <- bake(recipe, newdata = assessment(split), all_outcomes())
  otherwise <- list(net.result = rep(NA, NROW(out)))
  compute_ <- possibly(neuralnet::compute, otherwise)
  out$predicted <- as.vector(compute_(model, mod_data, ...)$net.result)
  names(out)[1] <- "true"
  return(out)
}
data_cv <- mutate(data_cv,
                  predicted_nn = pmap(list(splits, recipes, nn_models),
                                        pred_nn),
                  rmse_nn = map_dbl(predicted_nn,
                                      ~sqrt(mean((.x$true - .x$predicted)^2))),
                  r2_nn = map_dbl(predicted_nn,
                                    ~suppressWarnings(cor(.x$true, .x$predicted))^2))

best_hyper <- data_cv %>%
  group_by(hidden) %>%
  summarise(r2_nn = mean(r2_nn)) %>%
  ungroup() %>% 
  filter(r2_nn == max(r2_nn, na.rm = TRUE))

data_cv <- filter(data_cv, cost == best_hyper$cost, gamma == best_hyper$gamma)  
  
