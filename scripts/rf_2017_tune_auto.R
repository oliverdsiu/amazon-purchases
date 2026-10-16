library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# load data
train <- read_csv("train.csv")
orders_train <- train %>% select(!order_totals)

test <- read_csv("test.csv")

# setup folds and recipe
set.seed(2017)
orders_folds <- vfold_cv(orders_train, v = 10, strata = log_total)

rf_recipe <-
  recipe(log_total ~ ., data = orders_train) %>%
  step_mutate(q_demos_state = factor(q_demos_state),
              year = factor(year),
              month = factor(month))

# setup parameters
rf_model <-
  rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = tune()
  ) %>%
  set_engine("randomForest") %>%
  set_mode("regression")

model_param <-
  extract_parameter_set_dials(rf_model) %>%
  update(trees = trees(range = c(400, 600)),
         mtry = mtry(range = c(1, 33)))

# grid research
rf_tune <-
  model_param %>%
  grid_random(size = 100)

# fit workflow to tuning grid
rf_workflow <-
  workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_model)

rf_rand_tune <-
  rf_workflow %>%
  tune_grid(
    orders_folds,
    grid = model_param %>% grid_random(size = 10)
  )

best_spec <-
  rf_rand_tune %>%
  select_best(metric = "rmse")

# auto finalize parameters
rf_wf_auto <-
  rf_workflow %>%
  finalize_workflow(best_spec)

rf_res_auto <-
  rf_wf_auto %>%
  fit_resamples(resamples = orders_folds)

rf_wf_auto_fit <-
  rf_wf_auto %>%
  fit(data = orders_train)

orders_test_res <-
  rf_wf_auto_fit %>%
  predict(new_data = test) %>%
  bind_cols(test %>% select(id)) %>%
  select(id, log_total = .pred)

write_csv(orders_test_res, "submission.csv")
