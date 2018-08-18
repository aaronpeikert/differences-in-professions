library(here)
library(recipes)
library(rsample)
source(here("scripts", "01load.R"))

#----resample----
set.seed(1002)
data_cv <- rsample::vfold_cv(data,
                             v = 10, #ten folds
                             repeats = 10, #ten repeats
                             strata = "Berufsgruppe") #stratified

#----recipe----
recipe <- recipe(model_formula, 
                 data = data) %>%
  #step_BoxCox(Diktat) %>% #Diktat is skewd/heavy tailed
  # Normalize
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

data_cv <- mutate(data_cv, recipes = map(splits,
                                prepper,
                                recipe = recipe,
                                retain = TRUE,
                                verbose = FALSE))
