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
# processed_data <- function(data) {
#   reactive({ preprocess_data(data()) })
# }
# 
# processed_match_data <- function(data) {
#   reactive({ preprocess_match_data(data()) })
# }

# UI Module
playerdashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # SINISTRA: Statistics sopra + controlli sotto
      column(width = 5,
             box(width = 12, title = "Player Statistics", status = "primary", solidHeader = TRUE,
                 DTOutput(ns("table")) %>% withSpinner()
             ),
             
             fluidRow(
               box(width = 4, title = "Choose a Player Stat", status = "primary", solidHeader = TRUE,
                   selectInput(ns("dataset"), "Select:", choices = c(
                     "Best Scorer" = "best_scorer",
                     "Best Attacker" = "best_attacker",
                     "Best Blocker" = "best_blocker",
                     "Best Server" = "best_server",
                     "Best Digger" = "best_digger",
                     "Best Receiver" = "best_receiver"
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
      
      # DESTRA: Dettagli + Plot
      column(width = 7,
             box(width = 12, title = "Player Details", status = "primary", solidHeader = TRUE,
                 fluidRow(
                   column(3,
                          div(style = "text-align: center; height: 180px;",
                              uiOutput(ns("player_photo"))
                          )
                   ),
                   column(9,
                          uiOutput(ns("player_info")) %>% withSpinner()
                   )
                 ),
                 fluidRow(
                   column(12,
                          plotOutput(ns("player_plot"), height = "450px") %>% withSpinner()
                   )
                 )
             )
      )
    )
  )
}

# Server Module
playerdashboardServer <- function(id, processed_data, match_data,full_player_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    current_dataset <- reactive({ 
      req(input$dataset)
      processed_data()[[input$dataset]] 
    })
    
    # Aggiorna le scelte dinamicamente quando cambia il dataset
    observe({
      req(current_dataset())
      numeric_cols <- names(Filter(is.numeric, current_dataset()))
      
      # Etichette leggibili per selectInput
      pretty_names <- gsub("_", " ", numeric_cols)
      pretty_names <- tools::toTitleCase(pretty_names)
      named_choices <- setNames(numeric_cols, pretty_names)
      
      updateSelectInput(session, "tablevariable", choices = named_choices)
      updateSelectInput(session, "y_axis", choices = named_choices)
    })
    
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
    
    filtered_dataset <- reactive({
      req(current_dataset(), input$tablevariable, input$num)
      
      current_dataset() %>%
        filter(.data[[input$tablevariable]] >= input$num[1] & 
                 .data[[input$tablevariable]] <= input$num[2]) %>%
        arrange(desc(.data[[input$tablevariable]])) %>%
        mutate(Rank = row_number(), .before = 1)
    })
    
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
    
    output$player_plot <- renderPlot({
      req(selected_row(), input$y_axis)
      selected_player <- selected_row()$player_name
      player_data <- match_data() %>% filter(player_name == selected_player)
      ggplot(player_data, aes(x = file_name, y = .data[[input$y_axis]], group = 1)) +
        geom_line(color = "#333333", size = 1.2) +
        geom_point(color = "#871E2E", size = 4, alpha = 0.8) +
        geom_text(aes(label = .data[[input$y_axis]]), vjust = -0.5, color = "#333333", size = 5, fontface = "bold") +
        labs(title = paste("TokyoOG20 -", selected_player), x = "Game Played", y = input$y_axis) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    })
    outputOptions(output, "player_plot", suspendWhenHidden = FALSE)
    
    
    player_info_data <- reactive({
      req(selected_row())
      selected_player <- selected_row()$player_name  # This is the player name from the selected row
      full_player_data %>% filter(Name == selected_player)  # Use "Name" column for filtering
    })
    
    
    
    
    
    # Update the player photo dynamically
    output$player_photo <- renderUI({
      req(selected_row())
      
      # Get the selected player's name
      selected_player <- selected_row()$player_name
      
      # Construct possible file paths for both .jpg and .png
      photo_path_jpg <- paste0("www/player_photo/", selected_player, ".jpg")
      photo_path_png <- paste0("www/player_photo/", selected_player, ".png")
      
      # Debug: Print the selected player and the constructed file paths
      print(paste("Selected Player:", selected_player))
      print(paste("Constructed JPG Path:", photo_path_jpg))
      print(paste("Constructed PNG Path:", photo_path_png))
      
      # Check if either file exists
      if (file.exists(photo_path_jpg)) {
        # Debug: Print confirmation that the JPG file exists
        print("JPG file exists. Displaying photo.")
        
        # Display the JPG photo (remove "www/" from the src path)
        tags$img(src = sub("www/", "", photo_path_jpg), height = "180px")
      } else if (file.exists(photo_path_png)) {
        # Debug: Print confirmation that the PNG file exists
        print("PNG file exists. Displaying photo.")
        
        # Display the PNG photo (remove "www/" from the src path)
        tags$img(src = sub("www/", "", photo_path_png), height = "180px")
      } else {
        # Debug: Print a message if neither file exists
        print("No photo file exists. Displaying 'Photo not available'.")
        
        # Display a message if the photo is not available
        tags$p("Photo not available")
      }
    })
    
    # Display the player info
    output$player_info <- renderUI({
      req(player_info_data())
      player_data <- player_info_data()
      
      tags$div(
        style = "line-height: 1.2;",  # Adjust line height to reduce spacing
        tags$h3("Player Info"),
        tags$p(style = "margin: 2px 0;", tags$strong("Name: "), player_data$Name),  # Bold label
        tags$p(style = "margin: 2px 0;", tags$strong("Birthdate: "), player_data$Birthdate),
        tags$p(style = "margin: 2px 0;", tags$strong("Age: "), player_data$Age),
        tags$p(style = "margin: 2px 0;", tags$strong("Height: "), player_data$Height),
        tags$p(style = "margin: 2px 0;", tags$strong("Nationality: "), player_data$Nationality),
        tags$p(style = "margin: 2px 0;", tags$strong("Position: "), player_data$Position)
      )
    })
  
  })
}


# UI
ui <- fluidPage(playerdashboardUI("player_dashboard"))

# Server
server <- function(input, output, session) {
  data <- load_data()
  processed_data <- process_data(data)
  match_data <- process_match_data(data)
  playerdashboardServer("player_dashboard", processed_data, match_data)
}

# Run App
shinyApp(ui, server)
