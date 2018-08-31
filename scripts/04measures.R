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
  probs <- as.data.frame(predict(model, newdata = mod_data, prop = TRUE,...))
  out <- list(true, probs)
  names(out) <- c("true", "probs")
  return(out)
}
data_cv <- mutate(data_cv,
                  predicted_rpart = pmap(list(splits, recipes, rpart_models),
                                         pred_rpart))

#----decide----
decide <- function(probs, ...){
  decide_ <- function(...){
    probs <- splice(...)
    which <- which.max(probs)
    out <- names(probs[which])
    if(length(out)>1){
      warning("Can't really decide!")
      out <- out[1]
    }
    return(out)
  }
  out <- probs %>% select(...) %>% pmap_chr(decide_)
}

data_cv <- data_cv %>%
  mutate(predicted_probs = map(predicted_rpart, "probs"),
         predicted_class =  map(predicted_probs, decide, num_range("", 1:5)),
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

#----multi-auc----
multAUC <- function(true, probs){
  true <- as.factor(true)
  m = caTools::colAUC(probs, true)
  c = c(combn(1:nlevels(true), 2))
  mean(m[cbind(rep(1:nrow(m), each = 2), c)])
}

data_cv <- mutate(data_cv,
                  multAUC = map2_dbl(true_class, predicted_probs, multAUC))
