library(here)
library(recipes)
library(rsample)
source(here("scripts", "01load.R"))

#----resample----
set.seed(1002)
data_cv <- rsample::vfold_cv(data,
                             v = 10, #ten folds
                             repeats = 10, #ten repeats
                             strata = "gesamt") #stratified

#----recipe----
recipe1 <- recipe(model_formula1, 
                 data = data) %>%
  #step_BoxCox(Diktat) %>% #Diktat is skewd/heavy tailed
  # Normalize
  step_center(all_predictors(), -Berufsgruppe) %>%
  step_scale(all_predictors(), -Berufsgruppe)

recipe2 <- recipe(model_formula2, 
                  data = data) %>%
  #step_BoxCox(Diktat) %>% #Diktat is skewd/heavy tailed
  # Normalize
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

data_cv <- mutate(data_cv,
                  recipes1 = map(splits,
                                prepper,
                                recipe = recipe1,
                                retain = TRUE,
                                verbose = FALSE),
                  recipes2 = map(splits,
                                prepper,
                                recipe = recipe2,
                                retain = TRUE,
                                verbose = FALSE))
