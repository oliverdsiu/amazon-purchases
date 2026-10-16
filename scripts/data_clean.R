# Reconstruction of the competition's raw-data -> state/month panel pipeline.
# Not the official course script -- built afterward to verify how the
# provided train.csv/test.csv were derived from the raw purchase/survey data.

library(tidyverse)
library(tidymodels)
library(janitor)
tidymodels_prefer()

amazon_purchases <- read_csv("amazon-purchases.csv")
amazon <- amazon_purchases %>% filter(`Order Date` < mdy("01-01-2023"))
rm(amazon_purchases)

amazon <- amazon %>% clean_names()
amazon <- amazon %>% mutate(item_cost = purchase_price_per_unit * quantity)

survey <- read_csv("survey.csv")
survey <- survey %>% clean_names()
survey <- survey %>% filter(q_demos_state != "I did not reside in the United States")
survey$q_demos_age <- factor(survey$q_demos_age)

set.seed(2024)
survey_split <- initial_split(survey, strata = q_demos_state, prop = 0.5)
survey_train <- training(survey_split)
survey_test <- testing(survey_split)

# train construct

orders_train <-
  amazon %>%
  filter(!is.na(shipping_address_state)) %>%
  group_by(order_date, survey_response_id) %>%
  summarize(total_cost = sum(item_cost)) %>%
  inner_join(survey_train)

orders_train <- orders_train %>% mutate(month = month(order_date), year = year(order_date))

monthly_by_state_train <-
  orders_train %>%
  group_by(q_demos_state, year, month) %>%
  summarize(
    order_totals = sum(total_cost),
    log_total = log10(order_totals),
    count = n(),
    count_female = sum(q_demos_gender == "Female"),
    count_male = sum(q_demos_gender == "Male"),
    count_less5 = sum(q_amazon_use_how_oft == "Less than 5 times per month"),
    count_5to10 = sum(q_amazon_use_how_oft == "5 - 10 times per month"),
    count_over10 = sum(q_amazon_use_how_oft == "More than 10 times per month"),
    count_hh1 = sum(q_amazon_use_hh_size == "1 (just me!)"),
    count_hh2 = sum(q_amazon_use_hh_size == "2"),
    count_hh3 = sum(q_amazon_use_hh_size == "3"),
    count_hh4 = sum(q_amazon_use_hh_size == "4+"),
    count_howmany1 = sum(q_amazon_use_howmany == "1 (just me!)"),
    count_howmany2 = sum(q_amazon_use_howmany == "2"),
    count_howmany3 = sum(q_amazon_use_howmany == "3"),
    count_howmany4 = sum(q_amazon_use_howmany == "4+"),
    count_1824 = sum(q_demos_age == "18 - 24 years"),
    count_2534 = sum(q_demos_age == "25 - 34 years"),
    count_3544 = sum(q_demos_age == "35 - 44 years"),
    count_4554 = sum(q_demos_age == "45 - 54 years"),
    count_5564 = sum(q_demos_age == "55 - 64 years"),
    count_65up = sum(q_demos_age == "65 and older"),
    count_und25k = sum(q_demos_income == "Less than $25,000"),
    count_2549k = sum(q_demos_income == "$25,000 - $49,999"),
    count_5074k = sum(q_demos_income == "$50,000 - $74,999"),
    count_7599k = sum(q_demos_income == "$75,000 - $99,999"),
    count_100149k = sum(q_demos_income == "$100,000 - $149,999"),
    count_150kup = sum(q_demos_income == "$150,000 or more"),
    count_lessHS = sum(q_demos_education == "Some high school or less"),
    count_HS = sum(q_demos_education == "High school diploma or GED"),
    count_B = sum(q_demos_education == "Bachelor's degree"),
    count_G = sum(q_demos_education == "Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS, etc)"),
  ) %>%
  filter(log_total > 2)

# test construct

orders_test <-
  amazon %>%
  filter(!is.na(shipping_address_state)) %>%
  group_by(order_date, survey_response_id) %>%
  summarize(total_cost = sum(item_cost)) %>%
  inner_join(survey_test)

orders_test <- orders_test %>% mutate(month = month(order_date), year = year(order_date))

monthly_by_state_test <-
  orders_test %>%
  group_by(q_demos_state, year, month) %>%
  summarize(
    order_totals = sum(total_cost),
    log_total = log10(order_totals),
    count = n(),
    count_female = sum(q_demos_gender == "Female"),
    count_male = sum(q_demos_gender == "Male"),
    count_less5 = sum(q_amazon_use_how_oft == "Less than 5 times per month"),
    count_5to10 = sum(q_amazon_use_how_oft == "5 - 10 times per month"),
    count_over10 = sum(q_amazon_use_how_oft == "More than 10 times per month"),
    count_hh1 = sum(q_amazon_use_hh_size == "1 (just me!)"),
    count_hh2 = sum(q_amazon_use_hh_size == "2"),
    count_hh3 = sum(q_amazon_use_hh_size == "3"),
    count_hh4 = sum(q_amazon_use_hh_size == "4+"),
    count_howmany1 = sum(q_amazon_use_howmany == "1 (just me!)"),
    count_howmany2 = sum(q_amazon_use_howmany == "2"),
    count_howmany3 = sum(q_amazon_use_howmany == "3"),
    count_howmany4 = sum(q_amazon_use_howmany == "4+"),
    count_1824 = sum(q_demos_age == "18 - 24 years"),
    count_2534 = sum(q_demos_age == "25 - 34 years"),
    count_3544 = sum(q_demos_age == "35 - 44 years"),
    count_4554 = sum(q_demos_age == "45 - 54 years"),
    count_5564 = sum(q_demos_age == "55 - 64 years"),
    count_65up = sum(q_demos_age == "65 and older"),
    count_und25k = sum(q_demos_income == "Less than $25,000"),
    count_2549k = sum(q_demos_income == "$25,000 - $49,999"),
    count_5074k = sum(q_demos_income == "$50,000 - $74,999"),
    count_7599k = sum(q_demos_income == "$75,000 - $99,999"),
    count_100149k = sum(q_demos_income == "$100,000 - $149,999"),
    count_150kup = sum(q_demos_income == "$150,000 or more"),
    count_lessHS = sum(q_demos_education == "Some high school or less"),
    count_HS = sum(q_demos_education == "High school diploma or GED"),
    count_B = sum(q_demos_education == "Bachelor's degree"),
    count_G = sum(q_demos_education == "Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS, etc)"),
  ) %>%
  filter(log_total > 2)

# write out

monthly_by_state_test <- bind_cols(id = seq_len(nrow(monthly_by_state_test)), monthly_by_state_test)
monthly_by_state_test %>% select(!c(order_totals, log_total)) %>% write_excel_csv("test.csv")

monthly_by_state_train %>% write_excel_csv("train.csv")

# kaggle public/private split simulation

set.seed(123)
usage <- sample(c("Public", "Private"),
                size = nrow(monthly_by_state_test),
                replace = TRUE,
                prob = c(0.8, 0.2))

monthly_by_state_test %>%
  bind_cols(Usage = usage) %>%
  write_excel_csv("solutions.csv")
