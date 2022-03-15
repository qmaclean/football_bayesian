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


##### Data load ####
pbp<-load_pbp(seasons = c(2019:2021))

roster<-load_rosters(seasons = most_recent_season())


pass<-pbp %>%
    filter(pass_attempt == 1) %>%
    mutate(post_team_coach = ifelse(
        posteam == home_team,home_coach,away_coach),
        def_team_coach = ifelse(
            posteam == home_team,away_coach,home_coach),
        
    )

pass_metrics<-pass %>%
    group_by(passer_id) %>%
    summarize(air_yards = sum(air_yards,na.rm = TRUE),
              mean_cpoe = mean(cpoe,na.rm=TRUE),
              mean_air_yards = sum(air_yards,na.rm = TRUE) / n_distinct(play_id),
              mean_yards_after_catch = sum(yards_after_catch,na.rm=TRUE) / n_distinct(play_id)
    )

pass.sim.mcmc<-read.csv("/Users/qmaclean/Desktop/Data_Science_Projects_Personal/bayesian_football/football_bayesian/pass.sim.mcmc.csv")

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
    select(full_name,team,headshot_url,chances,mcmc.mean,mcmc.sd,mean_air_yards,mean_YAC) 

### GT TABLE customization ###

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



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Passing Success Above Average"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(),

        # Show a plot of the generated distribution
        mainPanel(
           gt_output("tbl")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    output$tbl<-render_gt({
       tab_data %>% 
            filter(complete.cases(team)) %>%
            tab_function()
    })
        


}

# Run the application 
shinyApp(ui = ui, server = server)
