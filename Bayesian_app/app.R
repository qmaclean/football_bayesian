#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(gt)
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(shinythemes)
library(rsconnect)
library(caret)
library(lme4)
library(merTools)
library(ggpmisc)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(ggsci)
library(googledrive)
library(curl)
library(arrow)
library(ggrepel)




##### Data load ####
#pass<-load_pbp(seasons = c(1999:2010)) %>%
#    filter(pass_attempt == 1) %>%
#    mutate(post_team_coach = ifelse(
#        posteam == home_team,home_coach,away_coach),
#        def_team_coach = ifelse(
#            posteam == home_team,away_coach,home_coach)) %>%
#dplyr::select(play_id,game_id,season,pass_attempt,post_team_coach,posteam,home_team,home_coach,away_coach,
#                def_team_coach,passer_player_name,passer_id,air_yards,cpoe,air_yards,
#                yards_after_catch,receiver_id,receiver_player_name,pass_location,pass_length,
#                qb_epa)


#write_csv(pass,"pass_1999_2010.csv")

pass<-read_csv(url("https://raw.githubusercontent.com/qmaclean/football_bayesian/main/pass_1999_2010.csv"))


#separating due to memory
#pass1<-load_pbp(seasons = c(2011:2021)) %>%
#    filter(pass_attempt == 1) %>%
#    mutate(post_team_coach = ifelse(
#        posteam == home_team,home_coach,away_coach),
#        def_team_coach = ifelse(
#            posteam == home_team,away_coach,home_coach)) %>%
#  dplyr::select(play_id,game_id,season,pass_attempt,post_team_coach,posteam,home_team,home_coach,away_coach,
#                def_team_coach,passer_player_name,passer_id,air_yards,cpoe,air_yards,
#                yards_after_catch,receiver_id,receiver_player_name,pass_location,pass_length,
#                qb_epa)

#write_csv(pass1,"pass_2011_2021.csv")

pass1<-read_csv(url("https://raw.githubusercontent.com/qmaclean/football_bayesian/main/pass_2011_2021.csv"))


pass<-rbind(pass,pass1)

rm(pass1)

pass<-pass %>%
    mutate(passer_player_name = case_when(
        passer_player_name == "Aa.Rodgers" ~ "A.Rodgers",
        passer_player_name == "Jos.Allen" ~ "J.Allen",
        TRUE ~ as.character(passer_player_name)
    ))






roster<-load_rosters(seasons = c(1999:2021)) %>%
    filter(position == "QB")

wrs<-load_rosters(seasons = c(1999:2021)) %>%
    filter(position %in% c("WR","TE","RB"))

### add in PSAA data; built per year
pass.sim.final<-read_csv(url("https://raw.githubusercontent.com/qmaclean/football_bayesian/main/Bayesian_app/pass.sim.mcmc_all.csv")) %>%
    dplyr::select(-...1,-X)




### DAta COMBINE
pass_metrics<-pass %>%
    group_by(passer_id,season) %>%
    summarize(sum_air_yards = sum(air_yards,na.rm = TRUE),
              mean_cpoe = mean(cpoe,na.rm=TRUE),
              mean_air_yards = mean(air_yards,na.rm = TRUE),
              mean_yards_after_catch = mean(yards_after_catch,na.rm=TRUE),
              
    )




top_receiver <-pass %>%
    group_by(passer_id,passer_player_name,receiver_id,receiver_player_name,season) %>%
        summarize(sum_air_yards_wr = sum(air_yards,na.rm = TRUE),
                  mean_cpoe_wr = mean(cpoe,na.rm=TRUE),
                  mean_air_yards_wr = mean(air_yards,na.rm = TRUE),
                  mean_yards_after_catch_wr = mean(yards_after_catch,na.rm=TRUE),
                  receptions = n()) %>%
    arrange(desc(sum_air_yards_wr)) %>%
    ungroup() %>%
    group_by(passer_id,passer_player_name,season) %>%
    filter(complete.cases(receiver_id),
           sum_air_yards_wr > 0) %>%
        top_n(n = 5,wt=receptions) %>%
    ungroup() %>%
    left_join(wrs,by=c("receiver_id" = "gsis_id","season" = "season")) %>%
    dplyr::select(passer_id,passer_player_name,receiver_id,full_name,season,sum_air_yards_wr,mean_cpoe_wr,mean_air_yards_wr,
           mean_yards_after_catch_wr,team,position,headshot_url) %>%
    rename(rec_team = team,
           rec_position = position,
           rec_headshot_url = headshot_url,
           rec_name = full_name) %>%
    left_join(roster,by=c("passer_id" = "gsis_id","season"="season")) %>%
    dplyr::select(passer_id,passer_player_name,receiver_id,rec_name,season,sum_air_yards_wr,mean_cpoe_wr,mean_air_yards_wr,
           mean_yards_after_catch_wr,rec_team,rec_position,rec_headshot_url,headshot_url,team) %>%
    dplyr::mutate(mean_cpoe_wr = round(mean_cpoe_wr,1),
                  mean_air_yards_wr = round(mean_air_yards_wr,1),
                  mean_yards_after_catch_wr = round(mean_yards_after_catch_wr,1)) %>%
    left_join(pass.sim.final,by=c("passer_id","season","team")) %>%
    dplyr::rename(headshot_url = headshot_url.x) %>%
    dplyr::select(full_name,rec_name,team,season,headshot_url,rec_headshot_url,chances,sum_air_yards_wr,mean_cpoe_wr,
                  mean_air_yards_wr,mean_yards_after_catch_wr)







names<-pass.sim.final %>% 
    dplyr::select(full_name) %>%
    arrange(full_name) %>%
    distinct()

teams<-pass.sim.final %>%
    dplyr::select(team) %>%
    distinct() %>%
    arrange(team)


tab_data<- pass.sim.final %>%
    left_join(pass_metrics,by=c("passer_id","season")) %>%
    mutate(
           sim.mean = round(sim.mean,3),
           sim.sd = round(sim.sd,3),
           mean_air_yards = round(mean_air_yards,1),
           mean_YAC = round(mean_yards_after_catch,1),
           mean_cpoe = round(mean_cpoe,1)) %>%
    arrange(desc(sim.mean)) %>%
    #filter(chances > 500) %>%
    dplyr::select(passer_id,full_name,team,season,headshot_url,chances,sim.mean,sim.sd,sum_air_yards,mean_air_yards,mean_YAC,mean_cpoe) 

pass<-pass %>%
    left_join(tab_data,by=c("passer_id","season"))



min<-min(tab_data$chances)
max<-max(tab_data$chances)

### GT TABLE customization ###




# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("united"),
    # Application title
    titlePanel("Q**2Bs"),
    tags$div("This site is supposed to show advanced metrics for QBs"),
    br(),
    uiOutput('QBs'),
    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
    mainPanel(
        navbarPage("@QuinnsWisdom",
                   tabPanel("QBs",
                            fluidRow(
                                column(12, align = "center",
                                       
                                       tags$h3("Parameters"),
                                       
                                      shiny::selectizeInput(
                                          inputId = "season",
                                          label = "Season:",
                                          choices = 1999:2021,
                                          selected = NULL
                                      ),
            
                                       sliderInput(
                                           inputId =  "min_chances",
                                           label = "Minimum Passes:",
                                           min = min, max = max,
                                           value = 300
                                       ),
                                      
                                      shiny::selectizeInput(
                                          inputId = "QB",
                                          label = "QBs:",
                                          choices = names,
                                          selected = NULL, #c("Justin Herbert","Patrick Mahomes","Derek Carr","Russell Wilson"),
                                          multiple = TRUE#,
                                          #options = list(...)
                                      )
                                       
                                ),
                            ),
                           
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                  tabPanel("Summary",
                           splitLayout(cellWidths = c("100%","100%"),
                           column(12,gt_output("tbl")),
                           plotOutput("trendGraph1"))),
                  tabPanel("Plot",
                  fluidRow(
                      splitLayout(cellWidths = c("100%","100%"),
                      plotOutput("passerGraph1"),
                      plotOutput("passerGraph2"))
                      )),
                  tabPanel("Combo",
                           column(12,gt_output("tbl1")))
                  
                    
                      
    
                  
         #column(width = 12,
         #       hr()),
    
         

         
           #column(12,gt_output("tbl")),
           #column(8,plotOutput(outputId = "passerGraph1",width="50%",height="50%"))))
           #column(8,plotOutput(outputId = "passerGraph2",width="50%",height="50%"))))
        
     
        
        )
        )
    )
        ) 

) 
)
         
         
    
#,


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #options(shiny.maxRequestSize=10*1024^2)
    
    
    
    #observeEvent({
        
    updateSelectizeInput(
        inputId = 'season',
        choices = 1999:2021,
        selected = 2021#,
        #server = TRUE
    ) 
    
    
    updateSliderInput(
        inputId = 'min_chances',
        value = 300,
        min = min,
        max = max
        )
    
    updateSelectizeInput(
        inputId = 'QB',
        choices = c(Choose = '',names),
        server = TRUE,
        selected = c("Justin Herbert","Patrick Mahomes","Derek Carr","Russell Wilson"),
        options = list(maxOptions = 5000)
    )
    
    #})
    
    output$tbl<-render_gt({
        
        tab_data<- tab_data %>%
            dplyr::select(full_name,team,season,headshot_url,chances,sim.mean,sum_air_yards,mean_air_yards,mean_YAC,mean_cpoe) 
     
        tab_data<-tab_data  %>%
            filter(chances >= input$min_chances) %>%
            filter(season %in% input$season) %>%
            filter(full_name %in% input$QB)
        
       tab_data %>% 
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
               sim.mean = "PSAA",
               sum_air_yards = "Air Yards",
               mean_air_yards = "Air Yards Avg.",
               mean_YAC = "YAC Avg.",
               sum_air_yards = "Air Yards Total",
               mean_cpoe = "CPOE Avg."
           ) %>%
           data_color(
               columns = c(sim.mean),
               colors = scales::col_numeric(
                   palette = c("#f7f7f7","#7fbf7b"),         
                   domain = NULL
               )
           ) %>%
           tab_style(
               style = cell_text(weight = "bold"),
               locations = cells_body(
                   columns = c(full_name,team,season)
               )
           ) %>% 
           tab_options(
               column_labels.background.color = "white",
               column_labels.font.weight = "bold",
               #table.border.top.width = px(3),
               table.border.top.color = "transparent",
               table.border.bottom.color = "transparent",
               #table.border.bottom.width = px(3),
               #column_labels.border.top.width = px(3),
               column_labels.border.top.color = "transparent",
               #column_labels.border.bottom.width = px(3),
               column_labels.border.bottom.color = "black",
               data_row.padding = px(3),
               source_notes.font.size = 12,
               table.font.size = 12,
               heading.align = "left"
           )  %>%
           opt_table_font(
               font = list(
                   google_font("Chivo"),
                   default_fonts()
               )
           )  %>%
           tab_footnote(
               footnote = "Passing Success Above Average (PSAA) computed using Bayesian GLMER; Rstan",
               locations = cells_column_labels(
                   columns = sim.mean
               )) 
            
    })
    
    output$trendGraph1<-renderPlot({
      
      pass.sim.final1<-pass.sim.final %>%
        dplyr::filter(season <= input$season) %>%
        dplyr::filter(full_name %in% input$QB) %>%
        mutate(label = if_else(season == max(season), as.character(full_name), NA_character_)) 
      
      s<-as.numeric(input$season)
      name_lab<-pass.sim.final1$full_name
      
      pass.sim.final1 %>%
        ggplot() +
        aes(x=season,y=mcmc.mean,group=full_name,color=name_lab) +
        theme_bw() +
        geom_line(size = 1) +
        geom_point(size = 3) +
        geom_hline(
          yintercept = 0,
          color="grey91",
          size = 2,
          linetype="dashed"
        ) +
        geom_text_repel(
          aes(color = full_name, label = label),
          fontface = "bold",
          size = 6,
          direction = "y",
          xlim = c(s+0.8, NA),
          hjust = 1,
          segment.size = .7,
          segment.alpha = .5,
          segment.linetype = "dotted",
          box.padding = .2,
          segment.curvature = -0.1,
          segment.ncp = 3,
          segment.angle = 20
        ) +  coord_cartesian(
          clip = "off",
          ylim = c(-0.1, 0.1)
        ) +
        theme(
          legend.position = "none"
        ) 
      
    })
    
   
    
    output$passerGraph1<- renderPlot({
        
       
        new_pass<-pass %>%
            filter(chances >= input$min_chances) %>%
            filter(season %in% input$season) %>%
            filter(full_name %in% input$QB)
        
        #ggplot(new_pass,aes(x=air_yards,fill=passer_player_name)) +
        #    geom_density(alpha=0.3) +
        #    theme(legend.position = "bottom")
        
      
        
        
        new_pass %>%
        filter(air_yards > 0,
                   yards_after_catch > 0) %>% 
        ggplot() +
        aes(x=air_yards,y=yards_after_catch) +
        geom_smooth(
            se = FALSE,
            size = 1.2,
            formula = formula(y ~ x),
            method = 'lm',
            show.legend = FALSE,
            aes(color = passer_id)
        ) +
        geom_point(
            show.legend = FALSE,
            size = 2,
            alpha = 0.2,
            color = "grey"
        )   +
    stat_poly_eq(formula = formula(y ~ x),
        aes(label = ..rr.label..),
        parse = TRUE,
        size = 3,
        color = "dark grey"
    ) +
        facet_wrap(~passer_player_name) +
            theme_set(theme_minimal()) +
        theme_update(
            text = element_text(size = 9)
        ) +
            labs(title = "Air Yards vs Yards After the Catch",
                 x = "AY",
                 y = "YAC"
                 ) +
            scale_color_brewer(palette = "BuGn")
        
        
        
    },height=400,width=500)
    
    output$passerGraph2<-renderPlot({
    
        new_pass1<-pass %>%
        filter(chances >= input$min_chances) %>%
        filter(season == input$season) %>%
        filter(full_name %in% input$QB) %>%
        filter(complete.cases(pass_location),
               complete.cases(pass_length),
               pass_location != "NA",
               pass_length != "NA")
        
        new_pass1<-new_pass1 %>%
            mutate(loc = paste(pass_location,pass_length,sep="-")) %>%
            group_by(passer_player_name,loc) %>%
            summarize(qb_epa = mean(qb_epa,na.rm = T),
                      cpoe = mean(cpoe,na.rm = T),
                      chances = n())
        
        
    
    new_pass1 %>%
        ggplot() +
        aes(x=qb_epa,y=cpoe,size=chances) +
        geom_point(
            #show.legend = FALSE,
            #size = 5,
            alpha = 0.8,
            aes(color = loc) 
            
        ) +
        facet_wrap(~passer_player_name) +
        theme_set(theme_minimal()) +
        theme_update(
            text = element_text(size = 9),
            legend.position = "right"
        ) + scale_color_brewer(palette = "BuGn")
    
    })
   
    
    output$tbl1<-render_gt({
        
        top_receiver<-top_receiver %>%
            dplyr::select(full_name,rec_name,team,season,headshot_url,rec_headshot_url,chances,sum_air_yards_wr,mean_cpoe_wr,
                          mean_air_yards_wr,mean_yards_after_catch_wr)
        
        top_receiver<-top_receiver  %>%
            filter(season == input$season) %>%
        filter(chances >= input$min_chances) %>%
            filter(full_name %in% input$QB)
        
        top_receiver %>% 
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
            text_transform(
                locations = cells_body(vars(rec_headshot_url)),
                fn = function(x){
                    web_image(
                        url = x,
                        height = px(30)
                    )
                }
            ) %>%
            cols_label(
                headshot_url = "",
                rec_headshot_url = "",
                rec_name = "Receiver",
                full_name = "QB",
                team = "Team",
                sum_air_yards_wr = "Air Yards",
                mean_cpoe_wr = "CPOE",
                mean_air_yards_wr = "Air Yards Avg.",
                mean_yards_after_catch_wr = "YAC"
            ) %>%
            data_color(
                columns = c(sum_air_yards_wr),
                colors = scales::col_numeric(
                    palette = c("#f7f7f7","#7fbf7b"),         
                    domain = NULL
                )
            ) %>%
            tab_style(
                style = cell_text(weight = "bold"),
                locations = cells_body(
                    columns = c(full_name,rec_name,team,season)
                )
            ) %>% 
            tab_options(
                column_labels.background.color = "white",
                column_labels.font.weight = "bold",
                #table.border.top.width = px(3),
                table.border.top.color = "transparent",
                table.border.bottom.color = "transparent",
                #table.border.bottom.width = px(3),
                #column_labels.border.top.width = px(3),
                column_labels.border.top.color = "transparent",
                #column_labels.border.bottom.width = px(3),
                column_labels.border.bottom.color = "black",
                data_row.padding = px(3),
                source_notes.font.size = 12,
                table.font.size = 12,
                heading.align = "left"
            )  %>%
            opt_table_font(
                font = list(
                    google_font("Chivo"),
                    default_fonts()
                )
            )  
        
    })



}

# Run the application 
shinyApp(ui = ui, server = server)


