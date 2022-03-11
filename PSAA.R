library(nflfastR)
library(nflreadr)
library(lme4)
library(doParallel)
library(rstanarm)
library(stringr)
library(knitr)
library(merTools)
library(tidyverse)
library(doRNG)
library(ggimage)
library(ggrepel)
library(ggpmisc)
library(gt)
library(optimx) 
library(caret)


#### calculate pass success above average using R STAN

pbp<-load_pbp(seasons = c(2019:2021))

roster<-load_rosters(seasons = most_recent_season())


pass<-pbp %>%
  filter(pass_attempt == 1) %>%
  mutate(post_team_coach = ifelse(
    posteam == home_team,home_coach,away_coach),
    def_team_coach = ifelse(
      posteam == home_team,away_coach,home_coach),
    
  )


pass_df<-pass %>%
  dplyr::select(play_id,success,score_differential,ydstogo,down,yardline_100,
                game_seconds_remaining,quarter_seconds_remaining,half_seconds_remaining,drive,
                qtr,shotgun,no_huddle,pass_length,first_down_pass,
                passer_id,receiver_id,post_team_coach,def_team_coach) %>%
  dplyr::filter(complete.cases(success),
                down < 4
                )
### rescale & center variables
pass_preprocess <- pass_df %>%
  select_if(is_numeric) %>%  
  select(-play_id,-success,-drive,-qtr,-shotgun,
         -no_huddle,-first_down_pass,-down) %>% 
  preProcess(method = c("center", "scale")) 

pass_df <- predict(pass_preprocess, newdata = pass_df)

summary(pass_df)

pass_df_final<-pass_df
pass_df_final<-na.omit(pass_df_final)


pass_mle<-lme4::glmer(factor(success) ~ 
                        ydstogo + down + yardline_100 + 
                        shotgun  + 
                        (1|passer_id) +
                        (1|receiver_id) +
                        (1|def_team_coach),
                      data = pass_df_final,
                      family = binomial(link = "probit"))

summary(pass_mle)






pass.success.mod.mcmc<-stan_glmer(success ~ 
                                    ydstogo + down + yardline_100 + 
                                    shotgun  + 
                                    (1|passer_id) +
                                    (1|receiver_id) +
                                    (1|def_team_coach),
                           data = pass_df_final,
                           family=binomial(link="probit"),
                           chains = 4,
                           prior_intercept = normal(0,10),
                           prior = normal(0,1),
                           prior_aux = exponential(1),
                           prior_covariance = decov(1,1,1,1),
                           adapt_delta = .8,
                           iter = 1000,
                           QR = FALSE)

saveRDS(pass.success.mod.mcmc,"pass_success_mcmc.rds")

pass.success.mod.mcmc<-readRDS("pass_success_mcmc.rds")


## view model results ###
summary.vals<-as.data.frame(summary(pass.success.mod.mcmc))
summary.vals$parms<-row.names(summary.vals)


### create rusher dataframes ####
passer.vals<-dplyr::filter(
  summary.vals,stringr::str_detect(
    parms,'passer_id'
  ))


passer.vals$passer<-stringr::str_sub(
  passer.vals$parms,25,34
)

passer_names<-pbp %>%
  select(passer_id,passer_player_name) %>%
  distinct() %>%
  left_join(roster,by=c("passer_id" = "gsis_id")) %>%
  select(passer_id,passer_player_name,full_name,headshot_url,years_exp,position,depth_chart_position) %>%
  filter(complete.cases(passer_id))

passer.vals<-passer.vals %>%
  left_join(passer_names,by=c("passer" = "passer_id"))

passer.vals<-passer.vals %>%
  dplyr::select(-parms) %>%
  dplyr::select(full_name,mean,mcse,sd,`10%`,`50%`,`90%`,n_eff,Rhat,headshot_url,years_exp,position) %>%
  dplyr::arrange(desc(mean))

passer.vals<-passer.vals %>%
  distinct() 

#### Query
pass.success.mcmc.preds<-pass.success.mod.mcmc$data

SDs<-data.frame(pass.success.mod.mcmc$ses)
SDs$parms<-row.names(SDs)

passer.SD<-dplyr::filter(SDs,str_detect(
  parms,'passer_id'
))

passer.SD$passer<-stringr::str_sub(
  passer.SD$parms,25,34)

passer.coefs<-ranef(pass.success.mod.mcmc)[[2]]
passer.coefs$passer<-row.names(passer.coefs)

names(passer.coefs)[1]<-"passer.eff"

passer.coefs<-passer.coefs %>%
  left_join(passer.SD,by=c("passer"))

#passer.coefs<-passer.coefs %>%
#  filter(complete.cases(passer_player_name))

names(passer.coefs)[1]<-"passer.eff"
passer.coefs<-passer.coefs %>%
  rename(passer.SD = pass.success.mod.mcmc.ses) %>%
  filter(complete.cases(passer.SD)) %>%
  select(-parms)
  
  
#  passer.SD[,1]

pass.success.mcmc.preds$fitted.values<-pass.success.mod.mcmc$fitted.values
pass.success.mcmc.preds$full.lin.preds<-pass.success.mod.mcmc$linear.predictors

###### Convert group-level intercepts to probability scale ####
pass.success.mcmc.preds<-pass.success.mcmc.preds %>%
  dplyr::left_join(passer.coefs,by=c("passer_id" = "passer"))

pass.success.mcmc.preds$wo_passer<-with(pass.success.mcmc.preds,
                                        pnorm(full.lin.preds - passer.eff)  
)

pass.success.mcmc.preds$wo_passer_plus1SD<-with(pass.success.mcmc.preds,
                                                pnorm(full.lin.preds - passer.eff + passer.SD))

##### summarize predictor values ####
pass.mcmc.final<-pass.success.mcmc.preds %>%
  dplyr::group_by(passer_id)%>%
  dplyr::summarise(
    mcmc.mean = mean(fitted.values - wo_passer),
    mcmc.sd = mean(wo_passer_plus1SD - wo_passer),
    chances = n()
  ) %>%
  dplyr::left_join(roster,by=c("passer_id" = "gsis_id")) %>%
  dplyr::select(passer_id,full_name,position,team,chances,mcmc.mean,mcmc.sd,headshot_url) %>%
  arrange(desc(mcmc.mean))

pass.mcmc.final<-pass.mcmc.final %>% filter(position == "QB")

pass.mcmc.final %>%
  filter(chances > 1000) %>%
  head(10L)



#### simulate posteriors ####
RE.sims<-merTools::REsim(pass_mle,n.sims=10000,seed=1234)

passer.sims<-dplyr::filter(RE.sims,groupFctr == "passer_id")

passer.sims<-passer.sims %>%
  rename(passer = groupID) %>%
  dplyr::select(passer,mean,median,sd)

##### extra coefficients ###
pass.frame.sim.preds<-pass.success.mcmc.preds
pass.frame.sim.preds<-pass.frame.sim.preds %>%
  dplyr::left_join(passer.sims,by=c("passer_id" = "passer"))


pass.frame.sim.preds$all_frame<-predict(pass_mle,type="response")
pass.frame.sim.preds$all_passer_mean<-pnorm(predict(pass_mle,
                                                    type='link',
                                                    re.form = ~
                                                      (1|passer_id) +
                                                      (1|receiver_id) +
                                                      (1|def_team_coach)) +
                                              pass.frame.sim.preds$mean)

pass.frame.sim.preds$wo_passer_plus1SD<-pnorm(predict(
  pass_mle,
  type='link',
  re.form = ~ 
    (1|passer_id) +
    (1|receiver_id) +
    (1|def_team_coach)) +
    pass.frame.sim.preds$passer.SD)

#pass.frame.sim.preds$wo_passer<-predict(pass_mle,
#                                        type="response",
#                                        re.form = ~ 
#                                          (1|passer_id) +
#                                          (1|receiver_id) +
#                                          (1|def_team_coach))

##### Compile and compare simulation results to MCMC ######
pass.sim.final<-pass.frame.sim.preds %>%
  dplyr::group_by(passer_id) %>%
  dplyr::summarise(
    sim.mean = mean(all_frame - wo_passer),
    sim.sd = mean(wo_passer_plus1SD - wo_passer),
    chances = n()
  ) %>%
  dplyr::left_join(roster,by=c("passer_id" = "gsis_id")) %>%
  dplyr::select(passer_id,full_name,position,team,chances,sim.mean,sim.sd,headshot_url) 





pass.sim.mcmc<-pass.sim.final %>%
  dplyr::inner_join(pass.mcmc.final,
                    by=c('full_name','chances')) %>%
  dplyr::select(full_name,passer_id.x,position.x,team.x,chances,
                mcmc.mean,sim.mean,mcmc.sd,sim.sd,headshot_url.x) %>%
  dplyr::rename(position = position.x,
                team = team.x,
                passer_id = passer_id.x,
                headshot_url = headshot_url.x) %>%
  dplyr::arrange(desc(sim.mean))



### compare means ###
pass.sim.mcmc %>%
  ggplot(aes(x=mcmc.mean,y=sim.mean)) +
  geom_point() +
  #geom_image(aes(image = headshot_url), size = 0.1,check_overlap=TRUE) +
  geom_hline(aes(yintercept = 0.0), lty = 2, col = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = 0.0), lty = 2, col = "red", alpha = 0.5) +
  theme_bw() +
  labs(title = "Passing Success Above Expectation Mean Comparison",
       subtitle = 'MCMC mean vs. Mixed Level Effects Mean (1000 sims)',
       x = "MCMC Mean",
       y = "MLE Sims Mean", 
       caption = "Viz: @QuinnsWisdom | Data: nflfastR | Seasons: 2019-2021") + 
  #geom_text(aes(x=win_pct_delta,y=cover_win_pct_prev,label = season),size = 3,nudge_y = -0.02,color="blue") +
  theme(
    axis.title = element_text(size = 7),
    axis.text = element_text(size=7),
    plot.title = element_text(size = 12,hjust=0.5),
    plot.subtitle = element_text(size=9,hjust=0.5))

pass_metrics<-pass %>%
  group_by(passer_id) %>%
  summarize(air_yards = sum(air_yards,na.rm = TRUE),
            mean_cpoe = mean(cpoe,na.rm=TRUE),
            mean_air_yards = sum(air_yards,na.rm = TRUE) / n_distinct(play_id),
            mean_yards_after_catch = sum(yards_after_catch,na.rm=TRUE) / n_distinct(play_id)
            )



tab_data<- pass.sim.mcmc %>%
  left_join(pass_metrics,by=c("passer_id")) %>%
  mutate(mcmc.mean = round(mcmc.mean,3),
         sim.mean = round(sim.mean,3),
         mcmc.sd = round(mcmc.sd,3),
         sim.sd = round(sim.sd,3)) %>%
  arrange(desc(sim.mean)) %>%
  filter(chances > 500) %>%
  select(full_name,team,position,headshot_url,chances,mcmc.mean,sim.mean,mcmc.sd,sim.sd,air_yards,mean_cpoe,mean_air_yards) 

mean(tab_data$mcmc.mean)
mean(tab_data$mean_cpoe)


ggplot(tab_data,aes(x=mcmc.mean,y=mean_cpoe)) +
  geom_point() +
  geom_vline(aes(xintercept = 0.02), lty = 2, col = "red", alpha = 0.5) +
  geom_hline(aes(yintercept = 0.5), lty = 2, col = "red", alpha = 0.5) +
  geom_image(aes(image = headshot_url), size = 0.1,check_overlap=TRUE) +
  labs(
    title = "Comparing Pass Success Above Average vs CPOE",
    subtitle = "Viz: @QuinnsWisdom | Min 500 PA | Seasons: 2019-2021",
    caption = "Data: CPOE - nflfastR | PSAA - Rstan GLMER (@QuinnsWisdom)",
    x = "Passing Success Above Avg (PSAA)",
    y= "CPOE"
  ) + 
  theme_bw() +
  theme(
    axis.title = element_text(size = 7),
    axis.text = element_text(size=7),
    plot.title = element_text(size = 12,hjust=0.5),
    plot.subtitle = element_text(size=9,hjust=0.5))

tab_data<- pass.sim.mcmc %>%
  left_join(pass_metrics,by=c("passer_id")) %>%
  mutate(mcmc.mean = round(mcmc.mean,3),
         sim.mean = round(sim.mean,3),
         mcmc.sd = round(mcmc.sd,3),
         sim.sd = round(sim.sd,3),
         mean_air_yards = round(mean_air_yards,1),
         mean_YAC = round(mean_yards_after_catch,1)) %>%
  arrange(desc(sim.mean)) %>%
  filter(chances > 500) %>%
  select(full_name,team,headshot_url,chances,mcmc.mean,mcmc.sd,mean_air_yards,mean_YAC) 



tab_function <- function(data, ...){
  data %>% 
    gt() %>% 
    text_transform(
      locations = cells_body(vars(headshot_url)),
      fn = function(x){
        web_image(
          url = x,
          height = px(30)
        )
      }
    ) %>% 
    cols_label(
      headshot_url = "",
      full_name = "Player",
      team = "Team",
      chances = "Passes",
      mcmc.mean = "PSAA",
      mcmc.sd = "Sdev",
      mean_air_yards = "Air Yards Avg.",
      mean_YAC = "YAC Avg."
    ) %>% 
    data_color(
      columns = vars(mcmc.mean),
      colors = scales::col_numeric(
        palette = c("#f7f7f7","#7fbf7b"),         
        domain = NULL
      )
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = vars(full_name,team)
      )
    ) %>% 
    tab_options(
      column_labels.background.color = "white",
      column_labels.font.weight = "bold",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 12,
      heading.align = "left",
      ...
    ) %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    )  %>%
    tab_footnote(
      footnote = "Passong Success Above Average (PSAA) computed using Bayesian GLMER; Rstan",
      locations = cells_column_labels(
        columns = mcmc.mean
      )) %>%
    tab_footnote(
      footnote = "standard deviations for RSAA",
      locations = cells_column_labels(
        columns = mcmc.sd
      ))  %>%
    tab_header(
      title = md("Top 10 QBs in Passing Success Above Average (PSAA)"),
      subtitle = md("Viz: @QuinnsWisdom | Data: nflfastR | Seasons: 2019-2021 | Min. 500 passes")
    ) 
}

a<-tab_data %>% 
  filter(complete.cases(team)) %>%
  slice(1:10) %>% 
  tab_function()
a

