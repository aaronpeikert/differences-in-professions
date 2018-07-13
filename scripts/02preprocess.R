library(here)
library(recipes)
library(rsample)
source(here("scripts", "01load.R"))

#----recipe----
recipe <- recipe(model_formula, 
                 data = data) %>%
  # Dummy variables on the qualitative predictors
  step_dummy(all_nominal()) %>%
  step_BoxCox(Diktat) %>% #Diktat is extreme heavy tailed
  # Normalize
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

#----resample----
set.seed(1002)
data_cv <- rsample::vfold_cv(data,
                             v = 10, #ten folds
                             repeats = 10, #ten repeats
                             strata = "Berufsgruppe") #stratified

data_cv <- mutate(data_cv, recipes = map(splits,
                                prepper,
                                recipe = recipe,
                                retain = TRUE,
                                verbose = FALSE))

