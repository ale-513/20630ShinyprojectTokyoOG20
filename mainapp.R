library(shiny)
library(bs4Dash)
library(dplyr)
library(DT)
library(ggplot2)
library(ovlytics)
library(datavolley)
library(shinycssloaders)

source("modules/dashwelcome_module.R")
source("modules/statplayer_module.R")
source("modules/statteam_module.R")
source("modules/playerdashboard_module.R")
source("modules/teamdashboard_module.R")
source("helpers/prep_player_dashboard_helper.R")
source("helpers/prep_team_dashboard_helper.R")

OL21_plays <- data.frame(read.csv('data/OL21_df_plays_cleaned.csv', header = TRUE))
full_player_data <- data.frame(read.csv("data/full_player_data.csv")) 
team_description <- data.frame(read.csv("data/team_description.csv")) 

ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  fullscreen = TRUE,
  
  title = "TokyoOG20",
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "TokyoOG20",
      image = "https://upload.wikimedia.org/wikipedia/en/thumb/1/1d/2020_Summer_Olympics_logo_new.svg/160px-2020_Summer_Olympics_logo_new.svg.png"
    )
  ),
  
  sidebar = dashboardSidebar(
    
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem("Dashboard - Welcome",tabName = "dashwelcome",icon = icon("volleyball"),selected = TRUE),
      menuItem("Dashboard - Player", tabName = "dashplayer", icon = icon("people-group")),
      menuItem("Dashboard - Team",tabName = "dashteam",icon = icon("chart-simple")),
      menuItem("Stat Analysis - Player",tabName = "statplayer",icon = icon("users")),
      menuItem("Stat Analysis - Team",tabName = "statteam",icon = icon("chart-line"))
    )),
  
  footer = dashboardFooter(
    left = "20630 Introduction to Sports Analytics - Shiny Project (2025)"
  ),
  
  body = dashboardBody(
    tabItems(
      
      tabItem(tabName = "dashwelcome", dashwelcomeUI("welcome")),
      
      tabItem(tabName = "dashplayer",playerdashboardUI("playerdashboard")),
      
      tabItem(tabName = "dashteam",teamdashboardUI("teamdashboard")),
      
      tabItem(tabName = "statplayer", statplayerUI("statplayer")),
      
      tabItem(tabName = "statteam", statteamUI("statteam"))
    )
  )
)

server <- function(input, output, session) {
  # Preprocess the data reactively (prep_player_dashboard_helper.R)
  processed_data <- reactive({preprocess_data(OL21_plays)})
  match_data <- reactive({preprocess_match_data(OL21_plays)})
  
  # Preprocess the data reactively (prep_team_dashboard_helper.R)
  processed_data_team <- reactive({preprocess_data_team(OL21_plays)})
  match_data_team <- reactive({preprocess_match_data_team(OL21_plays)})
  
  #dashwelcomeServer("welcome") # nothing for now
  playerdashboardServer("playerdashboard", processed_data, match_data,full_player_data)
  teamdashboardServer("teamdashboard",processed_data_team,match_data_team,team_description)
  statplayerServer("statplayer", OL21_plays)
  statteamServer("statteam",OL21_plays)
}

shinyApp(ui, server)

