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


##### Data load ####
pbp<-load_pbp(seasons = c(2019:2021))

roster<-load_rosters(seasons = most_recent_season())


pass<-pbp %>%
    filter(pass_attempt == 1) %>%
    mutate(post_team_coach = ifelse(
        posteam == home_team,home_coach,away_coach),
        def_team_coach = ifelse(
            posteam == home_team,away_coach,home_coach)
        
    )





pass_metrics<-pass %>%
    group_by(passer_id) %>%
    summarize(sum_air_yards = sum(air_yards,na.rm = TRUE),
              mean_cpoe = mean(cpoe,na.rm=TRUE),
              mean_air_yards = sum(air_yards,na.rm = TRUE) / n_distinct(play_id),
              mean_yards_after_catch = sum(yards_after_catch,na.rm=TRUE) / n_distinct(play_id)
    )



pass.sim.mcmc<-read.csv("/Users/qmaclean/Desktop/Data_Science_Projects_Personal/bayesian_football/football_bayesian/pass.sim.mcmc.csv")

names<-pass.sim.mcmc %>% 
    dplyr::select(full_name) %>%
    arrange(full_name)

teams<-pass.sim.mcmc %>%
    dplyr::select(team) %>%
    distinct() %>%
    arrange(team)


tab_data<- pass.sim.mcmc %>%
    left_join(pass_metrics,by=c("passer_id")) %>%
    mutate(mcmc.mean = round(mcmc.mean,3),
           sim.mean = round(sim.mean,3),
           mcmc.sd = round(mcmc.sd,3),
           sim.sd = round(sim.sd,3),
           mean_air_yards = round(mean_air_yards,1),
           mean_YAC = round(mean_yards_after_catch,1)) %>%
    arrange(desc(sim.mean)) %>%
    #filter(chances > 500) %>%
    select(passer_id,full_name,team,headshot_url,chances,mcmc.mean,mcmc.sd,mean_air_yards,mean_YAC) 

pass<-pass %>%
    left_join(tab_data,by=c("passer_id"))

min<-min(tab_data$chances)
max<-max(tab_data$chances)

### GT TABLE customization ###




# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("united"),
    # Application title
    titlePanel("Passing Success Above Average"),
    tags$div("This site is supposed to show advanced metrics for QBs"),
    br(),
    uiOutput('QBs'),
    # Sidebar with a slider input for number of bins 
    #sidebarLayout(
    mainPanel(
        navbarPage("@QuinnsWisdom",
                   tabPanel("PSAA",
                            fluidRow(
                                column(4, align = "left",
                                       
                                       tags$h3("Parameters"),
                                       

                                       
                                       sliderInput(
                                           inputId =  "min_chances",
                                           label = "Minimum Passes:",
                                           min = min, max = max,
                                           value = 1000
                                       ),
                                       
                                )
                            ),
                           
        # Show a plot of the generated distribution
        mainPanel("main panel",
                  fluidRow(
                      splitLayout(cellWidths = c("100%","100%"),
                  
    
                  
         #column(width = 12,
         #       hr()),
    
         

         
           column(12,gt_output("tbl")),
           plotOutput(outputId = "passerGraph1")))
        
     
        )
        )
        )
    )
    

) 
         
         
    
#,


# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    
    
    output$tbl<-render_gt({
        
        tab_data<-pass.sim.mcmc %>%
            left_join(pass_metrics,by=c("passer_id")) %>%
            mutate(mcmc.mean = round(mcmc.mean,3),
                   sim.mean = round(sim.mean,3),
                   mcmc.sd = round(mcmc.sd,3),
                   sim.sd = round(sim.sd,3),
                   mean_air_yards = round(mean_air_yards,1),
                   mean_YAC = round(mean_yards_after_catch,1)) %>%
            arrange(desc(sim.mean)) %>%
            select(full_name,team,headshot_url,chances,mcmc.mean,mcmc.sd,mean_air_yards,mean_YAC) 
        
        tab_data<-tab_data  %>%
            filter(chances >= input$min_chances)
        
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
                   columns = mcmc.mean
               )) %>%
           tab_footnote(
               footnote = "standard deviations for RSAA",
               locations = cells_column_labels(
                   columns = mcmc.sd
               ))  
            
    })
    
    output$passerGraph1<- renderPlot({
        
       
        new_pass<-pass %>%
            filter(chances >= input$min_chances) 
        
        ggplot(new_pass,aes(x=air_yards,fill=passer_player_name)) +
            geom_density(alpha=0.3) +
            theme(legend.position = "bottom")
        
        
        
        
    },height=600,width=850)
    
    


}

# Run the application 
shinyApp(ui = ui, server = server)
