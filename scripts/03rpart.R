library(rpart)
library(DescTools)
library(here)
library(rpart.plot)
source(here("scripts", "02preprocess.R"))

#----rpart----
fit_rpart <- function(recipe, ...){
  #browser()
  pred <- recipe %>% juice(all_predictors()) %>% names()
  out <- recipe %>% juice(all_outcomes()) %>% names()
  model_formula <- as.formula(paste0(out, " ~ ", paste(pred, collapse = " + ")))
  rpart(model_formula,
        data = juice(recipe, everything(), composition = "data.frame"),
        method = "class",
        ...)
}
hypermat <- as.data.frame(expand.grid(minsplit = c(10, 15, 20, 25, 30, 35, 40, 50),
                                      minbucket = c(5, 10, 15, 20, 25, 30)))

data_cv <- data_cv %>%
  mutate(hypermat = list(hypermat)) %>% 
  unnest(hypermat, .preserve = c(splits, recipes))

data_cv <- mutate(data_cv, rpart_models = pmap(list(recipe = recipes, minsplit = minsplit, minbucket = minbucket), fit_rpart))

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
  mutate(predicted_class = map(predicted_rpart, "probs") %>%
           map(decide, num_range("", 1:5)),
         true_class = map(predicted_rpart, "true") %>%
           map(1) %>%
           map(as.character))

data_cv <- mutate(data_cv, CramerV = map2_dbl(predicted_class, true_class, CramerV))

best_hyper <- data_cv %>%
  group_by(minsplit, minbucket) %>%
  summarise(CramerV = mean(CramerV)) %>%
  ungroup() %>% 
  filter(CramerV == max(CramerV)) %>% 
  slice(1L)
