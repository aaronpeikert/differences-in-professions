library(here)
library(recipes)
library(rsample)
source(here("scripts", "predict-profession", "01load.R"))

#----resample----
set.seed(1002)
data_cv <- rsample::vfold_cv(data,
                             v = 10, #ten folds
                             repeats = 10, #ten repeats
                             strata = "profession") #stratified

#----recipe----
recipe <- recipe(model_formula, 
                   data = data)

data_cv <- mutate(data_cv, recipes = map(splits,
                                         prepper,
                                         recipe = recipe,
                                         retain = TRUE,
                                         verbose = FALSE))