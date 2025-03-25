# Load required libraries
library(shiny)
library(bs4Dash)
library(dplyr)
library(DT)
library(ggplot2)
library(ovlytics)
library(datavolley)
library(shinycssloaders)

# Source helper functions
# source("C:/Users/Monica Andrea/Downloads/prep_player_dashboard_helper.R")
# 
# # Data Processing Module
# load_data <- function() {
#   reactive({ read.csv('OL21_df_plays_cleaned.csv', header = TRUE) })
# }
# 
# processed_data_team <- function(data) {
#   reactive({ preprocess_data_team(data()) })
# }
# 
# processed_match_data_team <- function(data) {
#   reactive({ preprocess_match_data_team(data()) })
# }

# UI Module
teamdashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # SINISTRA: Statistics sopra + filtri sotto
      column(width = 5,
             box(width = 12, title = "Team Statistics", status = "primary", solidHeader = TRUE,
                 DTOutput(ns("table")) %>% withSpinner()
             ),
             
             fluidRow(
               box(width = 4, title = "Choose a Team Stat", status = "primary", solidHeader = TRUE,
                   selectInput(ns("dataset"), "Select:", choices = c(
                     "Best Scorer Team" = "best_scorer",
                     "Best Attacker Team" = "best_attacker",
                     "Best Blocker Team" = "best_blocker",
                     "Best Server Team" = "best_server",
                     "Best Digger Team" = "best_digger",
                     "Best Receiver Team" = "best_receiver"
                   ))
               ),
               box(width = 4, title = "Table Filter", status = "primary", solidHeader = TRUE,
                   selectInput(ns("tablevariable"), "Filter Variable:", choices = NULL),
                   sliderInput(ns("num"), "Range:", min = 1, max = 100, value = c(1, 100))
               ),
               box(width = 4, title = "Plot Y-axis", status = "primary", solidHeader = TRUE,
                   selectInput(ns("y_axis"), "Choose Y-axis:", choices = NULL)
               )
             )
      ),
      
      # DESTRA: Dettagli + grafico
      column(width = 7,
             box(width = 12, title = "Team Details", status = "primary", solidHeader = TRUE,
                 fluidRow(
                   column(3,
                          div(style = "text-align: center; height: 180px;",
                              uiOutput(ns("team_flag"))
                          )
                   ),
                   column(9,
                          uiOutput(ns("team_info")) %>% withSpinner()
                   )
                 ),
                 fluidRow(
                   column(12,
                          plotOutput(ns("team_plot"), height = "450px") %>% withSpinner()
                   )
                 )
             )
      )
    )
  )
}

# Server Module
teamdashboardServer <- function(id, processed_data, match_data,team_description) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Dataset selezionato
    current_dataset <- reactive({ 
      req(input$dataset)
      processed_data()[[input$dataset]] 
    })
    
    # Aggiorna dinamicamente le scelte dei selectInput con nomi leggibili
    observe({
      req(current_dataset())
      numeric_cols <- names(Filter(is.numeric, current_dataset()))
      pretty_names <- gsub("_", " ", numeric_cols)
      pretty_names <- tools::toTitleCase(pretty_names)
      named_choices <- setNames(numeric_cols, pretty_names)
      
      updateSelectInput(session, "tablevariable", choices = named_choices)
      updateSelectInput(session, "y_axis", choices = named_choices)
    })
    
    # Slider dinamico in base alla colonna selezionata
    observe({
      req(current_dataset(), input$tablevariable)
      selected_col <- current_dataset()[[input$tablevariable]]
      if (is.numeric(selected_col)) {
        updateSliderInput(session, "num", 
                          min = min(selected_col, na.rm = TRUE), 
                          max = max(selected_col, na.rm = TRUE), 
                          value = c(min(selected_col, na.rm = TRUE), max(selected_col, na.rm = TRUE)))
      }
    })
    
    # Dataset filtrato
    filtered_dataset <- reactive({
      req(current_dataset(), input$tablevariable, input$num)
      current_dataset() %>%
        filter(.data[[input$tablevariable]] >= input$num[1] & 
                 .data[[input$tablevariable]] <= input$num[2]) %>%
        arrange(desc(.data[[input$tablevariable]])) %>%
        mutate(Rank = row_number(), .before = 1)
    })
    
    # Tabella
    output$table <- renderDT({
      req(filtered_dataset())
      datatable(
        filtered_dataset(),
        options = list(scrollX = TRUE, pageLength = 5),
        selection = list(mode = 'single', selected = 1),
        rownames = FALSE
      )
    })    
    
    
    selected_row <- reactive({ req(input$table_rows_selected); filtered_dataset()[input$table_rows_selected, ] })
    
    output$team_plot <- renderPlot({
      req(selected_row(), input$y_axis)
      selected_team <- selected_row()$team
      team_data <- match_data() %>% filter(team == selected_team)
      ggplot(team_data, aes(x = file_name, y = .data[[input$y_axis]], group = 1)) +
        geom_line(color = "#333333", size = 1.2) +
        geom_point(color = "#871E2E", size = 4, alpha = 0.8) +
        geom_text(aes(label = .data[[input$y_axis]]), vjust = -0.5, color = "#333333", size = 5, fontface = "bold") +
        labs(title = paste("TokyoOG20 -", selected_team), x = "Game Played", y = input$y_axis) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    })
    outputOptions(output, "team_plot", suspendWhenHidden = FALSE)
    
    #output$team_flag <- renderUI({ req(current_dataset()); tags$img(src = "path_to_player_photo", height = "180px") })
    
    team_info_data <- reactive({
      req(selected_row())
      selected_team <- selected_row()$team  # This is the player name from the selected row
      team_description %>% filter(Team == selected_team)  # Use "Name" column for filtering
    })
    
    
    
    output$team_flag <- renderUI({
      req(selected_row())
      
      # Get the selected team's name
      selected_team <- selected_row()$team
      
      # Construct possible file paths for both .jpg and .png
      photo_path_jpg <- paste0("www/flags/", selected_team, ".jpg")
      photo_path_png <- paste0("www/flags/", selected_team, ".png")
      
      # Debug: Print the selected player and the constructed file paths
      print(paste("Selected Team:", selected_team))
      print(paste("Constructed JPG Path:", photo_path_jpg))
      print(paste("Constructed PNG Path:", photo_path_png))
      
      # Check if either file exists
      if (file.exists(photo_path_jpg)) {
        # Debug: Print confirmation that the JPG file exists
        print("JPG file exists. Displaying photo.")
        
        # Display the JPG photo (remove "www/" from the src path)
        tags$img(src = sub("www/", "", photo_path_jpg), height = "120px")
      } else if (file.exists(photo_path_png)) {
        # Debug: Print confirmation that the PNG file exists
        print("PNG file exists. Displaying photo.")
        
        # Display the PNG photo (remove "www/" from the src path)
        tags$img(src = sub("www/", "", photo_path_png), height = "120px")
      } else {
        # Debug: Print a message if neither file exists
        print("No photo file exists. Displaying 'Photo not available'.")
        
        # Display a message if the photo is not available
        tags$p("Photo not available")
      }
    })
    
    # Display the player info
    output$team_info <- renderUI({
      req(team_info_data())
      team_data <- team_info_data()
      
      tags$div(
        style = "font-size: 12px;",  # Adjust font size for the entire div
        tags$h3("Team Info"),
        tags$p(style = "margin: 2px 0;", tags$strong("Team: "), team_data$Team, "Men's National Volleyball Team"),  # Bold label
        tags$p(style = "margin: 2px 0;", tags$strong("TokyoOG20 Ranking: "), team_data$Ranking),
        tags$p(style = "margin: 2px 0;", tags$strong("Description: "), team_data$Description)
      )
    })
    
  
  })
}


# UI
ui <- fluidPage(teamdashboardUI("team_dashboard"))

# Server
server <- function(input, output, session) {
  data <- load_data()
  processed_data_team <- process_data_team(data)
  match_data_team <- process_match_data_team(data)
  teamdashboardServer("team_dashboard", processed_data_team, match_data_team)
}

# Run App
shinyApp(ui, server)
