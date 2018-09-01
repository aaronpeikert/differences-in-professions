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
                  predicted_rpart = pmap(list(splits, recipes, rpart_models),
                                         pred_rpart))

data_cv <- data_cv %>%
  mutate(predicted_class = map(predicted_rpart, "predicted"),
         true_class = map(predicted_rpart, "true") %>%
           map(1) %>%
           map(as.character))

#----accuracy----
accuracy <- function(x, y){
  correct <- x == y
  mean(correct)
}
data_cv <- mutate(data_cv, acc = map2_dbl(predicted_class, true_class, accuracy))

#----cramerv----
data_cv <- mutate(data_cv, CramerV = map2_dbl(predicted_class, true_class, CramerV))
