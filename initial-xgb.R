library(rstan)
library(tidyverse)
library(stringr)
library(lme4)
library(lubridate)
library(glmnet)
library(purrr)

dat <- readRDS('nfl-pbp.RDS') %>%
  tibble()

traindat <- filter(dat, season >= 2014 & 
                     season_type == 'REG' & 
                     down %in% c(1,2,3) &
                     !is.na(qb_dropback) &
                     !is.na(score_differential))

#home_wp_post
#yd

variables <- c('score_differential','qtr',)