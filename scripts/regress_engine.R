library(tidyverse)
library(tidymodels)
tidymodels_prefer()


# submission 01
# random forest
# default settings

train <- read_csv("train.csv")
orders_train <- train %>% select(- order_totals)

set.seed(2024)
orders_folds <- vfold_cv(orders_train, v = 10, strata = log_total)

rf_model <-
  rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_workflow <-
  workflow() %>%
  add_formula(log_total ~ .) %>%
  add_model(rf_model)

rf_res <-
  rf_workflow %>%
  fit_resamples(resamples = orders_folds)

rf_workflow_fit <-
  rf_workflow %>%
  fit(data = orders_train)

test <- read_csv("test.csv")

orders_test_res <-
  rf_workflow_fit %>%
  predict(new_data = test) %>%
  bind_cols(test %>% select(id)) %>%
  select(id, log_total = .pred)

# write_csv(orders_test_res, "submission.csv")


# test
# lm
train <- read_csv("train.csv")
orders_train <- train %>% select(- order_totals)
test <- read_csv("test.csv")

lm_model <-
  linear_reg() %>%
  set_engine("lm")

lm_recipe <-
  recipe(log_total ~ ., data = orders_train)

lm_workflow <-
  workflow() %>%
  add_model(lm_model) %>%
  add_recipe(lm_recipe)

lm_fit <-
  lm_workflow %>%
  fit(data = orders_train)

lm_predict <-
  lm_fit %>%
  predict(new_data = test) %>%
  bind_cols(test %>% select(id)) %>%
  select(id, log_total = .pred)

lm_predict
lm_fit %>% extract_fit_engine() %>% summary()

# write_csv(lm_predict, "submission.csv")

# compare rand_forest to lm
mse <- (orders_test_res$log_total - lm_predict$log_total)^2 %>% mean()
mse


# trying every rand_forest engine
# ranger, aorsf, h2o, partykit, randomForest, spark

# library load
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# ranger is in tidymodels

library(survival) # req for aorsf
library(censored) # aorsf

library(agua) # req for h2o
library(h2o)
h2o.init() # initialize h2o cluster

library(bonsai) # partykit

library(randomForest) # randomForest

library(sparklyr) # spark


train <- read_csv("train.csv")
orders_train <- train %>% select(- order_totals)

test <- read_csv("test.csv")

set.seed(2024)
orders_folds <- vfold_cv(orders_train, v = 10, strata = log_total)

rf_model <-
  rand_forest() %>%
  set_engine("spark") %>% # ranger, aorsf, h2o, partykit, randomForest, spark
  set_mode("regression")

rf_workflow <-
  workflow() %>%
  add_formula(log_total ~ .) %>%
  add_model(rf_model)

# metrics check
rf_res <-
  rf_workflow %>%
  fit_resamples(resamples = orders_folds)

rf_metrics <- rf_res %>% collect_metrics()
rf_metrics

# predictions

rf_workflow_fit <-
  rf_workflow %>%
  fit(data = orders_train)

orders_test_res <-
  rf_workflow_fit %>%
  predict(new_data = test) %>%
  bind_cols(test %>% select(id)) %>%
  select(id, log_total = .pred)
orders_test_res # should be 2952 rows/obs

# write_csv(orders_test_res, "submission.csv")

# sitrep
# ranger OK
# aorsf NG
# h2o OK but slow compute
# partykit NG cuz character data breaks it
# randomForest OK
# spark NG
