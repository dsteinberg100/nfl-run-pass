library(rstan)
library(tidyverse)
library(stringr)
library(lme4)
library(lubridate)
library(glmnet)
library(purrr)

dat <- readRDS('nfl-pbp.RDS') %>%
  tibble()

dat_train <- filter(dat, season >= 2014 & 
                     season_type == 'REG' & 
                     down %in% c(1,2,3) &
                     !is.na(qb_dropback) &
                     !is.na(score_differential))

qb_recipe <- recipe(qb_dropback ~ down + 
                      ydstogo + 
                      yardline_100 + 
                      score_differential + 
                      qtr + 
                      half_seconds_remaining,
    data = dat_train)

qb_model <- 
  boost_tree(
    trees = 1000, 
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

qb_workflow <- workflow() %>%
  add_recipe(qb_recipe) %>%
  add_model(qb_model)

