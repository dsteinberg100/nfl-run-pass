library(rstan)
library(lme4)
library(tidyverse)
library(stringr)
library(vip)
library(parsnip)
library(recipes)
library(workflows)
library(rsample)
library(dials)
library(tune)

dat <- readRDS('nfl-pbp.RDS') %>%
  tibble()

post16 <- filter(dat, season >= 2016 & 
                     season_type == 'REG' & 
                     down %in% c(1,2,3) &
                     !is.na(qb_dropback) &
                     !is.na(score_differential)) %>%
  mutate(qb_dropback = factor(qb_dropback),
         off_to = if_else(posteam_type == 'away', away_timeouts_remaining, home_timeouts_remaining),
         def_to = if_else(posteam_type == 'away', home_timeouts_remaining, away_timeouts_remaining)) %>%
  dplyr::select(qb_dropback, down, ydstogo, yardline_100, score_differential, qtr, half_seconds_remaining, off_to, def_to)

set.seed(123)
dat_split <- initial_split(post16)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

qb_recipe <- recipe(qb_dropback ~ down + 
                      ydstogo + 
                      yardline_100 + 
                      score_differential + 
                      qtr + 
                      half_seconds_remaining +
                      off_to +
                      def_to,
    data = dat_train)

qb_model <- 
  boost_tree(
    mtry = tune(),
    trees = 2000, 
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),                    
    sample_size = tune(),         
    stop_iter = 100
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

qb_workflow <- workflow() %>%
  add_recipe(qb_recipe) %>%
  add_model(qb_model)

xgb_grid <- grid_latin_hypercube(
  finalize(mtry(), dat_train),
  min_n(),
  tree_depth(),
  learn_rate(),
  loss_reduction(),
  sample_size = sample_prop(),
  size = 40
)

qb_folds <- vfold_cv(dat_train)


##Uncomment to redo parameter tuning. 40 evals took about 6-8 hours.
#  xgb_res <- tune_grid(
#    qb_workflow,
#    resamples = qb_folds,
#    grid = xgb_grid,
#    control = control_grid(save_pred = TRUE)
#  )
#  
# saveRDS(xgb_res, 'xgb-grid-search.RDS')


xgb_res <- readRDS('xgb-grid-search.RDS')

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC") +
  theme_minimal()



best_auc <- select_best(xgb_res, "roc_auc")
best_auc

qb_xgb <- finalize_workflow(
  qb_workflow,
  parameters = best_auc
)

qb_xgb %>%
  fit(data = dat_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")


final_mod <- last_fit(qb_xgb, dat_split)

collect_metrics(final_mod)

final_mod %>%
  collect_predictions() %>%
  roc_curve(qb_dropback, .pred_0) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )


final_mod %>%
  collect_predictions() %>%
  ggplot(aes(x = .pred_1, y = as.numeric(qb_dropback) - 1)) +
  geom_point() +
  geom_smooth() +
  geom_abline()

final_qb_mod <- fit(qb_workflow, post16)
post16$dropback_prob <- predict(final_qb_mod, newdata = post16)