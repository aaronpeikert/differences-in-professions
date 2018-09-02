library(here)
library(DescTools)
source(here("scripts", "03rpart.R"))

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
                  predicted_rpart1 = pmap(list(splits, recipes1, rpart_models1),
                                         pred_rpart),
                  predicted_rpart2 = pmap(list(splits, recipes2, rpart_models2),
                                          pred_rpart))

data_cv <- data_cv %>%
  mutate(predicted_class1 = map(predicted_rpart1, "predicted"),
         predicted_class2 = map(predicted_rpart2, "predicted"),
         true_class = map(predicted_rpart1, "true") %>%
           map(1) %>%
           map(as.character))

#----accuracy----
accuracy <- function(x, y){
  correct <- x == y
  mean(correct)
}
data_cv <- mutate(data_cv, acc1 = map2_dbl(predicted_class1, true_class, accuracy),
                  acc2 = map2_dbl(predicted_class2, true_class, accuracy))

#----cramerv----
data_cv <- mutate(data_cv,
                  CramerV1 = map2_dbl(predicted_class1, true_class, CramerV),
                  CramerV2 = map2_dbl(predicted_class2, true_class, CramerV))
