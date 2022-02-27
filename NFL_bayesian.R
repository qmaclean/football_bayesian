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

pbp<-load_pbp(seasons = c(2019:2021))

roster<-load_rosters(seasons = most_recent_season())


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
                    ydstogo + down + 
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

rush_df<-rush %>% dplyr::select(success,score_differential,ydstogo,down,rusher_player_id,stadium_id,post_team_coach,def_team_coach)
rush_df<-na.omit(rush_df)

rush.success.mod.mcmc<-stan_glmer(success ~ score_differential +
                             ydstogo + down +
                             (1|rusher_player_id) +
                             (1|stadium_id) + 
                             (1|post_team_coach) + 
                             (1|def_team_coach),
                           data = rush_df,
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

rush.success.mod.mcmc<-readRDS("rush_success_mcmc.rds")

## view model results ###
summary.vals<-as.data.frame(summary(rush.success.mod.mcmc))
summary.vals$parms<-row.names(summary.vals)


### create rusher dataframes ####
rush.vals<-dplyr::filter(
  summary.vals,stringr::str_detect(
    parms,'rusher_player_id'
  ))


rush.vals$rusher<-stringr::str_sub(
  rush.vals$parms,32,41
)


rush_names<-pbp %>%
  select(rusher_player_id,rusher_player_name) %>%
  distinct() %>%
  left_join(roster,by=c("rusher_player_id" = "gsis_id")) %>%
  select(rusher_player_id,rusher_player_name,full_name,headshot_url,years_exp,position,depth_chart_position) %>%
  filter(complete.cases(rusher_player_id))


rush.vals<-rush.vals %>%
  left_join(rush_names,by=c("rusher" = "rusher_player_id"))

rush.vals<-rush.vals %>%
  dplyr::select(-parms) %>%
  dplyr::select(full_name,mean,mcse,sd,`10%`,`50%`,`90%`,n_eff,Rhat,headshot_url,years_exp,position) %>%
  dplyr::arrange(desc(mean))

rush.vals<-rush.vals %>%
  distinct() 

rbs<-rush.vals %>%
  filter(position == "RB")

##### stadium #####
stadium.vals<-dplyr::filter(
  summary.vals,stringr::str_detect(
    parms,'stadium_id'
  ))

post_team_coach.vals<-dplyr::filter(
  summary.vals,stringr::str_detect(
    parms,'post_team_coach'
  ))


#### Query
rush.success.mcmc.preds<-rush.success.mod.mcmc$data

SDs<-data.frame(rush.success.mod.mcmc$ses)
SDs$parms<-row.names(SDs)

rusher.SD<-dplyr::filter(SDs,str_detect(
  parms,'rusher_player_id'
))

rusher.coefs<-ranef(rush.success.mod.mcmc)[[1]]
rusher.coefs$rusher<-row.names(rusher.coefs)

names(rusher.coefs)[1]<-"rusher.eff"
rusher.coefs$rusher.sd<-rusher.SD[,1]

rush.success.mcmc.preds$fitted.values<-rush.success.mod.mcmc$fitted.values
rush.success.mcmc.preds$full.lin.preds<-rush.success.mod.mcmc$linear.predictors

###### Convert group-level intercepts to probability scale ####
rush.success.mcmc.preds<-rush.success.mcmc.preds %>%
  dplyr::left_join(rusher.coefs,by=c("rusher_player_id" = "rusher"))

rush.success.mcmc.preds$wo_rusher<-with(rush.success.mcmc.preds,
                                  pnorm(full.lin.preds - rusher.eff)  
)

rush.success.mcmc.preds$wo_rusher_plus1SD<-with(rush.success.mcmc.preds,
                                          pnorm(full.lin.preds - rusher.eff + rusher.sd))

##### summarize predictor values ####
rusher.mcmc.final<-rush.success.mcmc.preds %>%
  dplyr::group_by(rusher_player_id)%>%
  dplyr::summarise(
    mcmc.mean = mean(fitted.values - wo_rusher),
    mcmc.sd = mean(wo_rusher_plus1SD - wo_rusher),
    chances = n()
  ) %>%
  dplyr::left_join(roster,by=c("rusher_player_id" = "gsis_id")) %>%
  dplyr::select(full_name,position,team,chances,mcmc.mean,mcmc.sd,headshot_url) %>%
  arrange(desc(mcmc.mean))

rb.mcmc.final<-rusher.mcmc.final %>% filter(position == "RB")

rb.mcmc.final %>%
  filter(chances > 100) %>%
  head(10L)

#### simulate posteriors ####
RE.sims<-merTools::REsim(rush_mle,n.sims=1000,seed=1234)

rusher.sims<-dplyr::filter(RE.sims,groupFctr == "rusher_player_id")

rusher.sims<-rusher.sims %>%
  rename(rusher = groupID) %>%
  dplyr::select(rusher,mean,median,sd)

##### extra coefficients ###
rush.frame.sim.preds<-rush.success.mcmc.preds
rush.frame.sim.preds<-rush.frame.sim.preds %>%
  dplyr::left_join(rusher.sims,by=c("rusher_player_id" = "rusher"))


rush.frame.sim.preds$all_frame<-predict(rush_mle,type="response")
rush.frame.sim.preds$all_rusher_mean<-pnorm(predict(rush_mle,
                                                type='link',
                                                re.form = ~
                                                  (1|stadium_id) + 
                                                  (1|post_team_coach) + 
                                                  (1|def_team_coach)) +
                                                  rush.frame.sim.preds$mean)

rush.frame.sim.preds$wo_rusher_plus1SD<-pnorm(predict(
  rush_mle,
  type='link',
  re.form = ~ 
    (1|stadium_id) + 
    (1|post_team_coach) + 
    (1|def_team_coach)) +
    rush.frame.sim.preds$rusher.sd)

rush.frame.sim.preds$wo_rusher<-predict(rush_mle,
                                    type="response",
                                    re.form = ~ 
                                      (1|stadium_id) + 
                                      (1|post_team_coach) + 
                                      (1|def_team_coach))

##### Compile and compare simulation results to MCMC ######
rush.sim.final<-rush.frame.sim.preds %>%
  dplyr::group_by(rusher_player_id) %>%
  dplyr::summarise(
    sim.mean = mean(all_frame - wo_rusher),
    sim.sd = mean(wo_rusher_plus1SD - wo_rusher),
    chances = n()
  ) %>%
  dplyr::left_join(roster,by=c("rusher_player_id" = "gsis_id")) %>%
  dplyr::select(full_name,position,team,chances,sim.mean,sim.sd,headshot_url) 

rb.sim.final<-rush.sim.final %>% filter(position == "RB")

rush.sim.mcmc<-rush.sim.final %>%
  dplyr::inner_join(rusher.mcmc.final,
                    by=c('full_name','chances')) %>%
  dplyr::select(full_name,position.x,team.x,chances,
                mcmc.mean,sim.mean,mcmc.sd,sim.sd,headshot_url.x) %>%
  dplyr::rename(position = position.x,
                team = team.x,
                headshot_url = headshot_url.x) %>%
  dplyr::arrange(desc(sim.mean))

rb.sim.mcmc<-rush.sim.mcmc %>%
  filter(position == "RB")


rb.sim.mcmc.final<-rb.sim.mcmc %>%
  dplyr::filter(chances >= 100)

rb.sim.mcmc.final.top<-rb.sim.mcmc.final %>%
  dplyr::filter(chances >= 500)


### compare means ###
rb.sim.mcmc.final %>%
  ggplot(aes(x=mcmc.mean,y=sim.mean)) +
  geom_point() +
  geom_image(aes(image = headshot_url), size = 0.1,check_overlap=TRUE) +
  geom_hline(aes(yintercept = 0.0), lty = 2, col = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = 0.0), lty = 2, col = "red", alpha = 0.5) +
  theme_bw() +
  labs(title = "Rushing Success Above Expectation Mean Comparison",
       subtitle = 'MCMC mean vs. Mixed Level Effects Mean (1000 sims)',
       x = "MCMC Mean",
       y = "MLE Sims Mean", 
       caption = "Viz: @QuinnsWisdom | Data: nflfastR | Seasons: 2019-2021") + 
   #geom_text(aes(x=win_pct_delta,y=cover_win_pct_prev,label = season),size = 3,nudge_y = -0.02,color="blue") +
  theme(
    axis.title = element_text(size = 7),
    axis.text = element_text(size=7),
    plot.title = element_text(size = 12,hjust=0.5),
    plot.subtitle = element_text(size=9,hjust=0.5)
  ) #+
  #stat_poly_eq(aes(label = ..rr.label..),size = 2,label.x=0.4)

### contract ###
pl<-c("Justin Jackson","Raheem Mostert","Alvin Kamara","James Conner","Dalvin Cook","Tony Pollard","AJ Dillon","Jeff Wilson","D'Ernest Johnson","Aaron Jones","Ronald Jones","Matt Breida","Jonathan Taylor","Latavius Murray","Darrell Henderson")
age<-c("26","30","27","27","27","25","24","27","26","28","25","27","23","32","25")
fa_yr<-c("2022","2022","2026","2022","2026","2023","2024","2022","2022","2025","2022","2022","2024","2022","2023")
fa_type<-c("UFA","UFA","UFA","UFA","UFA","UFA","UFA","UFA","RFA","UFA","UFA","UFA","UFA","UFA","UFA")
prev_cap_hit<-c("$920k","$3.6M","$14.5M","$1.75M","$11.8M","$1.1M","$1.4M","$2M","$850k","$5.8M","$2.25M","$1M","$2.1M","$1M","$1.1M")

#if 2022 list 2021 salary cap

slry<-data.frame(pl,age,fa_yr,fa_type,prev_cap_hit)

############# TABLE OUTPUT of MCMC ####
tab_data<- rb.sim.mcmc.final %>%
  left_join(slry,by=c("full_name" = "pl")) %>%
  mutate(mcmc.mean = round(mcmc.mean,3),
         sim.mean = round(sim.mean,3),
         mcmc.sd = round(mcmc.sd,3),
         sim.sd = round(sim.sd,3)) %>%
  arrange(desc(mcmc.mean)) %>%
  head(15L) %>%
  select(full_name,team,position,age,headshot_url,chances,mcmc.mean,mcmc.sd,fa_yr,fa_type,prev_cap_hit) 

##af8dc3", "#f7f7f7", c("#7fbf7b","#f7f7f7")

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
      position = "Pos.",
      age = "Age",
      chances = "Rushes",
      mcmc.mean = "RSAA",
      mcmc.sd = "Sdev",
      fa_yr = "FA Year",
      fa_type = "Contract Type",
      prev_cap_hit = "Cap Hit*"
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
        columns = vars(full_name,team,position,age)
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
      footnote = "Rushing Success Above Average (RSAA) computed using Monte Carlo Markov Chain LMER",
      locations = cells_column_labels(
        columns = mcmc.mean
      )) %>%
    tab_footnote(
      footnote = "standard deviations for RSAA MCMC",
      locations = cells_column_labels(
        columns = mcmc.sd
      )) %>%
    tab_footnote(
      footnote = "Free Agency Year",
      locations = cells_column_labels(
        columns = fa_yr
      )) %>%
    tab_footnote(
      footnote = "2022 Cap Hit (if Free Agency Year is 2022, then cap hit is showing 2021 cap hit)",
      locations = cells_column_labels(
        columns = prev_cap_hit
      )) %>%
    tab_header(
      title = md("Top 15 RBs in Rushing Success Above Average (RSAA)"),
      subtitle = md("Viz: @QuinnsWisdom | Data: nflfastR | Seasons: 2019-2021")
    ) 
}

a<-tab_data %>% 
  slice(1:15) %>% 
  tab_function()
a


### Bayesian Bootstrapping ###





### sample rushes ###
rush_df<-rush %>% dplyr::select(play_id,success,score_differential,ydstogo,down,rusher_player_id,stadium_id,post_team_coach,def_team_coach)
rush_df<-na.omit(rush_df)

rush_plays<-unique(rush_df$play_id)
rush_df$row<-row.names(rush_df)

samp.idx<-vector(mode="integer")
max.iter<-15
samp.max<-length(rush_plays)

### Bayesian block bagging (w/ parallelize snowe clusters) ###
set.seed(12345)

c1<-makeCluster(3)
registerDoParallel(c1)





se.results<-foreach(
  i=1:max.iter,
  .packages = c('lme4','tidyverse'),
  .inorder = FALSE,
  .verbose = TRUE) %dorng% {
    
    while (length(samp.idx) < nrow(rush_df)) {
      if (length(samp.idx) < quantile(length(rush_plays),probs = .9)){
        samp.weights <- matrix( rexp(length(rush_plays),1),ncol=length(rush_plays),byrow = TRUE)
        samp.weights <- samp.weights / rowSums(samp.weights)
        rush.samp<-sample(rush_plays,size=samp.max,prob=samp.weights,replace=TRUE)
        rush.row.samp<-dplyr::filter(rush_df,play_id %in% rush.samp)
      } else{
        samp.weights<-matrix(rexp(length(rush_plays),1),ncol=length(rush_plays),byrow=TRUE)
        samp.weights<-samp.weights / rowSums(samp.weights)
        rush.samp<-sample(rush_plays,size=samp.max,prob=samp.weights,replace=TRUE)
        rush.row.samp<-dplyr::filter(rush_df,play_id %in% rush.samp)
      }
      samp.idx<-append(samp.idx,rush.row.samp[,"row"])
    }
    rush.frame.tmp <- rush_df[samp.idx,]
    rush.frame.samp <-dplyr::slice(rush_df,1:nrow(rush_df)) #match
    rush.frame.mod<-lme4::glmer(success ~ score_differential +
                                            ydstogo + down + 
                                            (1|rusher_player_id) +
                                            (1|stadium_id) + 
                                            (1|post_team_coach) + 
                                            (1|def_team_coach),
                                          data = rush.frame.samp,
                                          family = binomial(link = "probit"),
                                          nAGQ=0,control = glmerControl(optimizer ="nloptwrap",
                                                                        calc.derivs = FALSE))
    rush.frame.preds<-rush.frame.samp
    rush.frame.preds$all_frame<-pred(rush.frame.mod,newdata=rush.frame.preds,type='response')
    rush.frame.preds$wo_rusher_frame<-predict(
      rush.frame.mod,newdata=rush.frame.preds,type='response',
      re.form = ~(1|stadium_id) + (1|post_team_coach) + (1|def_team_coach))
  
    rusher.frame.results<-rush.frame.preds %>%
      dplyr::group_by(rusher_player_id) %>%
      dplyr::summarise(
        rsaa_mean = mean(all_frame - wo_rusher_frame)
    ) %>%
      dplyr::select(rusher_player_id,rsaa_mean,everything()) %>%
      dplyr::arrange(rusher_player_id)
    
    rusher.frame.results<-as.data.frame(rusher.frame.results)
    rusher.frame.results[,"rsaa_mean"]
  }

stopCluster(c1)







