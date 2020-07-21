library(lme4)
library(tidyverse)

dat <- readRDS('nfl-pbp.RDS') %>%
  tibble()

post16 <- filter(dat, season >= 2016 & 
                   season_type == 'REG' & 
                   down %in% c(1,2,3) &
                   !is.na(qb_dropback) &
                   !is.na(score_differential)) %>%
  mutate(qb_dropback = factor(qb_dropback),
         off_to = if_else(posteam_type == 'away', away_timeouts_remaining, home_timeouts_remaining),
         def_to = if_else(posteam_type == 'away', home_timeouts_remaining, away_timeouts_remaining))

dropbacks <- filter(post16, qb_dropback == 1)

rm(dat)
rm(post16)

##Limit data to QB with > 300 attempts

mod <- lmer(qb_epa ~ 1 + (1|posteam:season), data = dropbacks)

ranef_rows <- str_split(rownames(ranef(mod)$'posteam:season'), ':')
season <- sapply(ranef_rows, '[', 2)
posteam <- sapply(ranef_rows, '[', 1)

passing_ranef <- data.frame(passing_re = ranef(mod)$'posteam:season'[,1],
                            season = season,
                            posteam = posteam)
