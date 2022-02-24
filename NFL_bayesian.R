library(nflfastR)
library(nflreadr)
library(lme4)
library(doParallel)
library(rstanarm)
library(stringr)
library(knitr)
library(merTools)
library(tidyverse)

pbp<-load_pbp(seasons = most_recent_season())


#### success lmer modelling for mixed effects
glimpse(pbp)

rush<-pbp %>%
  filter(rush_attempt == 1,
         lateral_rush == 0) %>%
  mutate(post_team_coach = ifelse(
    posteam == home_team,home_coach,away_coach),
    def_team_coach = ifelse(
      posteam == home_team,away_coach,home_coach),
    
  )



#### lmer ####
rush_mle<-lme4::glmer(factor(success) ~ score_differential +
                    ydstogo + down + run_gap + 
                      (1|rusher_player_id) +
                      (1|stadium_id) + 
                      (1|post_team_coach) + 
                      (1|def_team_coach),
                      data = rush,
                    family = binomial(link = "probit"),
                    control = glmerControl(optimizer ="Nelder_Mead"))

#### look at solving the model convergence? 

summary(rush_mle)

#### mcmcm multi-level model



rush.success.mod.mcmc<-stan_glmer(success ~ score_differential +
                             ydstogo + down + run_gap + 
                             (1|rusher_player_id) +
                             (1|stadium_id) + 
                             (1|post_team_coach) + 
                             (1|def_team_coach),
                           data = rush,
                           family=binomial(link="logit"),
                           chains = 4,
                           prior_intercept = normal(0,10),
                           prior = normal(0,1),
                           prior_aux = exponential(1),
                           prior_covariance = decov(1,1,1,1),
                           adapt_delta = .8,
                           iter = 1000,
                           QR = FALSE)

saveRDS(rush.success.mod.mcmc,"rush_success_mcmc.rds")

## view model results ###
summary.vals<-as.data.frame(summary(rush.success.mod.mcmc))
summary.vals$parms<-row.names(summary.vals)


### create rusher dataframes ####
rush.vals<-dplyr::filter(
  summary.vals,stringr::str_detect(
    parms,'rusher_player_id'
  ))

rush.vals$rusher<-stringr::str_sub(
  rush.vals$parms,
)


stadium.vals<-dplyr::filter(
  summary.vals,stringr::str_detect(
    parms,'stadium_id'
  ))

post_team_coach.vals<-dplyr::filter(
  summary.vals,stringr::str_detect(
    parms,'post_team_coach'
  ))




