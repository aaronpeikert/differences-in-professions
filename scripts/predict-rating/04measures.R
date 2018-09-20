library(here)
library(DescTools)
source(here("scripts", "predict-profession", "03rpart.R"))

#----prediction----
pred_rpart <- function(split, recipe, model, ...){
  mod_data <- bake(
    recipe, 
    newdata = assessment(split),
    all_predictors()
  )
  true <- bake(recipe, newdata = assessment(split), all_outcomes(), composition = "data.frame")
  predicted <- as.character(predict(model, newdata = mod_data, prop = TRUE,...))
  out <- list(true, predicted)
  names(out) <- c("true", "predicted")
  return(out)
}
data_cv <- mutate(data_cv,
                  predicted_rpart_i = pmap(list(splits, recipes_i, rpart_models_i),
                                         pred_rpart),
                  predicted_rpart_e = pmap(list(splits, recipes_e, rpart_models_e),
                                          pred_rpart))

data_cv <- data_cv %>%
  mutate(predicted_class_i = map(predicted_rpart_i, "predicted"),
         predicted_class_e = map(predicted_rpart_e, "predicted"),
         true_class = map(predicted_rpart_i, "true") %>%
           map(1) %>%
           map(as.character))

#----accuracy----
accuracy <- function(x, y){
  correct <- x == y
  mean(correct)
}
data_cv <- mutate(data_cv, acc_i = map2_dbl(predicted_class_i, true_class, accuracy),
                  acc_e = map2_dbl(predicted_class_e, true_class, accuracy))

#----cramerv----
data_cv <- mutate(data_cv,
                  cramerv_i = map2_dbl(predicted_class_i, true_class, CramerV),
                  cramerv_e = map2_dbl(predicted_class_e, true_class, CramerV))
