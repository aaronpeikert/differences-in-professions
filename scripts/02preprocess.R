library(here)
library(recipes)
library(rsample)
source(here("scripts", "01load.R"))

#----resample----
set.seed(1002)
data_cv <- rsample::vfold_cv(data,
                             v = 10, #ten folds
                             repeats = 10, #ten repeats
                             strata = "rating") #stratified

#----recipe----
recipe_i <- recipe(model_formula_i, 
                 data = data) %>%
  #step_BoxCox(Diktat) %>% #Diktat is skewd/heavy tailed
  # Normalize
  step_center(all_predictors(), -profession) %>%
  step_scale(all_predictors(), -profession)

recipe_e <- recipe(model_formula_e, 
                  data = data) %>%
  #step_BoxCox(Diktat) %>% #Diktat is skewd/heavy tailed
  # Normalize
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

data_cv <- mutate(data_cv,
                  recipes_i = map(splits,
                                prepper,
                                recipe = recipe_i,
                                retain = TRUE,
                                verbose = FALSE),
                  recipes_e = map(splits,
                                prepper,
                                recipe = recipe_e,
                                retain = TRUE,
                                verbose = FALSE))
