# statplayer_module.R


# Module UI Function
statplayerUI <- function(id) {
  ns <- NS(id)  # Namespace function
  
  tagList(
    fluidRow(
      # Column for filters
      column(
        width = 3,
        box(
          title = "Filters",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          selectInput(ns("team"), "Select Team:", choices = NULL),  # Will be updated dynamically
          selectInput(ns("player"), "Select Player:", choices = NULL),  # Will be updated dynamically
          selectInput(ns("skill"), "Select Skill:", choices = c("Attack", "Serve", "Reception", "Block", "Dig")),
          
          # Serve Panels
          conditionalPanel(
            condition = paste0("input['", ns("skill"), "'] == 'Serve'"),
            radioButtons(ns("plot_type_serve"), "Select Plot Type:", 
                         choices = c("Serve Trajectory", "Dot Heatmap", "Zone Heatmap", "Density Heatmap")),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_serve"), "'] == 'Serve Trajectory'"),
              checkboxGroupInput(ns("evaluation_trajectory_serve"), "Select Evaluation:", 
                                 choices = c("serve_points", "serve_errors", "serve_bad", "serve_positive"),
                                 selected = c("serve_points", "serve_errors", "serve_bad", "serve_positive"))
            ),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_serve"), "'] == 'Density Heatmap'"),
              selectInput(ns("evaluation_density_serve"), "Select Evaluation:", 
                          choices = c("serve_points", "serve_errors", "serve_bad", "serve_positive"))
            ),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_serve"), "'] == 'Dot Heatmap'"),
              checkboxGroupInput(ns("evaluation_dot_serve"), "Select Evaluation:", 
                                 choices = c("serve_points", "serve_errors", "serve_bad", "serve_positive"),
                                 selected = c("serve_points", "serve_errors", "serve_bad", "serve_positive"))
            ),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_serve"), "'] == 'Zone Heatmap'"),
              selectInput(ns("evaluation_zone_serve"), "Select Evaluation:", 
                          choices = c("total_serve", "serve_points", "serve_errors", "serve_bad", 
                                      "serve_positive", "serve_point_percentage", "serve_error_percentage", "serve_efficiency"))
            )
          ),
          
          # Reception Panels
          conditionalPanel(
            condition = paste0("input['", ns("skill"), "'] == 'Reception'"),
            radioButtons(ns("plot_type_reception"), "Select Plot Type:",
                         choices = c("Dot Heatmap", "Zone Heatmap", "Density Heatmap")),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_reception"), "'] == 'Density Heatmap'"),
              selectInput(ns("evaluation_density_reception"), "Select Evaluation:", 
                          choices = c("reception_error", "reception_bad", "reception_positive", "reception_perfect"))
            ),
            
            conditionalPanel(
              paste0("input['", ns("plot_type_reception"), "'] == 'Dot Heatmap'"),
              checkboxGroupInput(ns("evaluation_dot_reception"), "Select Evaluation:", 
                                 choices = c("reception_error", "reception_bad", "reception_positive", "reception_perfect"),
                                 selected = c("reception_error", "reception_bad", "reception_positive", "reception_perfect"))
            ),
            
            conditionalPanel(
              paste0("input['", ns("plot_type_reception"), "'] == 'Zone Heatmap'"),
              selectInput(ns("evaluation_zone_reception"), "Select Evaluation:", 
                          choices = c("total_reception", "reception_error", "reception_bad", "reception_positive", 
                                      "reception_perfect", "reception_positivity", "reception_efficiency"))
            )
          ),
          
          # Block Panels
          conditionalPanel(
            condition = paste0("input['", ns("skill"), "'] == 'Block'"),
            radioButtons(ns("plot_type_block"), "Select Plot Type:",
                         choices = c("Dot Heatmap", "Zone Heatmap", "Density Heatmap")),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_block"), "'] == 'Density Heatmap'"),
              selectInput(ns("evaluation_density_block"), "Select Evaluation:", 
                          choices = c("block_points", "block_errors", "block_positive", "block_poor"))
            ),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_block"), "'] == 'Dot Heatmap'"),
              checkboxGroupInput(ns("evaluation_dot_block"), "Select Evaluation:", 
                                 choices = c("block_points", "block_errors", "block_positive", "block_poor"),
                                 selected = c("block_points", "block_errors", "block_positive", "block_poor"))
            ),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_block"), "'] == 'Zone Heatmap'"),
              selectInput(ns("evaluation_zone_block"), "Select Evaluation:", 
                          choices = c("block_points", "block_errors", "block_positive", "block_poor", 
                                      "block_efficiency"))
            )
          ),
          
          # Dig Panels
          conditionalPanel(
            condition = paste0("input['", ns("skill"), "'] == 'Dig'"),
            radioButtons(ns("plot_type_dig"), "Select Plot Type:",
                         choices = c("Dot Heatmap", "Zone Heatmap", "Density Heatmap")),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_dig"), "'] == 'Density Heatmap'"),
              selectInput(ns("evaluation_density_dig"), "Select Evaluation:", 
                          choices = c("dig_error", "dig_perfect", "dig_good", "dig_freeball"))
            ),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_dig"), "'] == 'Dot Heatmap'"),
              checkboxGroupInput(ns("evaluation_dot_dig"), "Select Evaluation:", 
                                 choices = c("dig_error", "dig_perfect", "dig_good", "dig_freeball"),
                                 selected = c("dig_error", "dig_perfect", "dig_good", "dig_freeball"))
            ),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_dig"), "'] == 'Zone Heatmap'"),
              selectInput(ns("evaluation_zone_dig"), "Select Evaluation:", 
                          choices = c("dig_total", "dig_error", "dig_perfect", "dig_good", 
                                      "dig_freeball", "dig_efficiency"))
            )
          ),
          
          # Attack Panels
          conditionalPanel(
            condition = paste0("input['", ns("skill"), "'] == 'Attack'"),
            radioButtons(ns("plot_type_attack"), "Select Plot Type:", 
                         choices = c("Attack Trajectory", "Dot Heatmap", "Zone Heatmap", "Density Heatmap")),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_attack"), "'] == 'Attack Trajectory'"),
              checkboxGroupInput(ns("evaluation_trajectory_attack"), "Select Evaluation:", 
                                 choices = c("Winning attack", "Error", "Blocked"),
                                 selected = c("Winning attack", "Error", "Blocked"))
            ),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_attack"), "'] == 'Dot Heatmap'"),
              checkboxGroupInput(ns("evaluation_dot_attack"), "Select Evaluation:", 
                                 choices = c("Winning attack", "Error", "Blocked"),
                                 selected = c("Winning attack", "Error", "Blocked"))
            ),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_attack"), "'] == 'Zone Heatmap'"),
              selectInput(ns("evaluation_zone_attack"), "Select Evaluation:", 
                          choices = c("total_attack", "attack_points", "attack_errors", "attack_blocked", 
                                      "attack_point_percentage", "attack_error_percentage", "attack_efficiency"))
            ),
            
            conditionalPanel(
              condition = paste0("input['", ns("plot_type_attack"), "'] == 'Density Heatmap'"),
              selectInput(ns("evaluation_density_attack"), "Select Evaluation:", 
                          choices = c("Winning attack", "Error", "Blocked"))
            )
          )
        )
      ),
      
      # Column for plots
      column(
        width = 9,
        
        fluidRow(box(
          title = "Plots",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          conditionalPanel(
            condition = paste0("input['", ns("skill"), "'] == 'Attack'"),
            plotOutput(ns("attackPlot"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("skill"), "'] == 'Serve'"),
            plotOutput(ns("servePlot"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("skill"), "'] == 'Reception'"),
            plotOutput(ns("receptionPlot"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("skill"), "'] == 'Block'"),
            plotOutput(ns("blockPlot"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("skill"), "'] == 'Dig'"),
            plotOutput(ns("digPlot"))
          )
        )),
        
        fluidRow(
          # Add Match Selection (Dropdown)
          div(
            selectizeInput(ns("matches"), "Select Matches:", choices = NULL, multiple = TRUE),
            style = "display: flex; flex-direction: column; align-items: flex-start; gap: 5px;",
            actionButton(ns("select_all"), "Select All", style = "width: 100px; font-size: 12px; padding: 5px;"),
            actionButton(ns("deselect_all"), "Deselect All", style = "width: 100px; font-size: 12px; padding: 5px;")
          )
        )
      )
    )
  )
}



# Module Server Function
statplayerServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Update team choices dynamically
    observe({
      teams <- unique(data$team)
      updateSelectInput(session, "team", choices = teams)
    })
    
    # 2. Observe team selection -> update players
    observeEvent(input$team, {
      players <- unique(data$player_name[data$team == input$team])
      updateSelectInput(session, "player", choices = players)
    })
    
    # 3. Observe player selection -> update matches
    observeEvent(input$player, {
      req(input$player)
      
      matches <- unique(data$file_name[data$player_name == input$player])
      
      # Funzione per pulire e formattare ogni match
      format_match_label <- function(filename) {
  filename <- gsub("^_", "", filename)  # Rimuove underscore iniziale
  parts <- strsplit(filename, " ")[[1]]
  if (length(parts) >= 3) {
    date_str <- parts[1]
    teams_raw <- gsub("\\.dvw$", "", parts[3])  # Rimuove .dvw
    teams <- gsub("\\(.*\\)", "", teams_raw)   # Rimuove (VM)
    teams <- gsub("-", " vs ", teams)
    formatted_date <- format(as.Date(date_str), "%d %b")
    paste0(formatted_date, " - ", teams)
  } else {
    filename
  }
}

      
      named_matches <- setNames(matches, sapply(matches, format_match_label))
      
      updateSelectizeInput(session, "matches", choices = named_matches, selected = matches)
    })
    
    
    
    # Handle "Select All" button
    observeEvent(input$select_all, {
      matches <- unique(data$file_name[data$player_name == input$player])
      updateSelectizeInput(session, "matches", selected = matches)
    })
    
    # Handle "Deselect All" button
    observeEvent(input$deselect_all, {
      updateSelectizeInput(session, "matches", selected = character(0))
    })
    
    # Filter data based on selections
    filtered_data <- reactive({
      req(input$player, input$skill, input$matches)
      data %>%
        filter(player_name == input$player,
               skill == input$skill,
               file_name %in% input$matches) %>%
        mutate(evaluation = case_when(
          # ... (keep original case_when logic)
          skill == "Attack" & evaluation_code == "#" ~ "Winning attack",
          skill == "Attack" & evaluation_code == "=" ~ "Error",
          skill == "Attack" & evaluation_code == "/" ~ "Blocked",
          skill == "Serve" & evaluation_code == "#" ~ "serve_points",
          skill == "Serve" & evaluation_code == "=" ~ "serve_errors",
          skill == "Serve" & evaluation_code %in% c("-", "!") ~ "serve_bad",
          skill == "Serve" & evaluation_code %in% c("+", "/") ~ "serve_positive",
          skill == "Reception" & evaluation_code == "=" ~ "reception_error",
          skill == "Reception" & evaluation_code %in% c("-", "!", "/") ~ "reception_bad",
          skill == "Reception" & evaluation_code == "+" ~ "reception_positive",
          skill == "Reception" & evaluation_code == "#" ~ "reception_perfect",
          skill == "Block" & evaluation_code == "#" ~ "block_points",
          skill == "Block" & evaluation_code %in% c("=", "/") ~ "block_errors",
          skill == "Block" & evaluation_code == "+" ~ "block_positive",
          skill == "Block" & evaluation_code %in% c("!", "-") ~ "block_poor",
          skill == "Dig" & evaluation_code == "=" ~ "dig_error",
          skill == "Dig" & evaluation_code == "-" ~ "dig_freeball",
          skill == "Dig" & evaluation_code %in% c("+", "!") ~ "dig_good",
          skill == "Dig" & evaluation_code == "#" ~ "dig_perfect",
          TRUE ~ "Other"
        ))
    })
    
    # Reactive dataframes for different skills (kept the same)
    zone_attack <- reactive({
      if (input$skill == "Attack" && input$plot_type_attack == "Zone Heatmap") {
        filtered_data() %>%
          group_by(player_name, start_zone) %>%
          summarize(
            total_attack = sum(ifelse(skill == 'Attack', 1, 0), na.rm = TRUE),
            attack_points = sum(ifelse(skill == 'Attack' & evaluation_code == '#', 1, 0), na.rm = TRUE),
            attack_errors = sum(ifelse(skill == 'Attack' & evaluation_code == '=', 1, 0), na.rm = TRUE),
            attack_blocked = sum(ifelse(skill == 'Attack' & evaluation_code == '/', 1, 0), na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          mutate(
            attack_point_percentage = round((attack_points / total_attack) * 100, 2),
            attack_error_percentage = round((attack_errors / total_attack) * 100, 2),
            attack_efficiency = round(((attack_points - attack_errors - attack_blocked) / total_attack) * 100, 2)
          ) %>%
          ungroup() %>%
          cbind(dv_xy(.$start_zone, end = "upper"))
      } else {
        NULL
      }
    })
    
    # Reactive dataframe for Attack coordinate
    heatmap_attack <- reactive({
      filtered_data() %>%
        filter(skill == "Attack", player_name == input$player) %>%  
        mutate(evaluation = case_when(
          evaluation_code == "#" ~ "Winning attack",
          evaluation_code == "=" ~ "Error",
          evaluation_code == "/" ~ "Blocked",
          TRUE ~ "Other"
        ))
    })
    
    # Reactive dataframe for Serve 
    zone_serve <- reactive({
      if (input$skill == "Serve" && input$plot_type_serve == "Zone Heatmap") {
        # Add logic for Serve Zone Heatmap
        filtered_data() %>% 
          group_by(player_name,end_zone) %>% 
          summarize(
            # Serve metrics 
            total_serve = sum(ifelse(skill == "Serve", 1, 0), na.rm = TRUE),
            serve_points = sum(ifelse(skill == "Serve" & evaluation_code == "#", 1, 0), na.rm = TRUE),
            serve_errors = sum(ifelse(skill == "Serve" & evaluation_code == "=", 1, 0), na.rm = TRUE),
            serve_bad = sum(ifelse(skill == 'Serve' & evaluation_code %in% c("-", "!"), 1, 0), na.rm = TRUE),
            serve_positive = sum(ifelse(skill == 'Serve' & evaluation_code %in% c("+", "/"), 1, 0), na.rm = TRUE),
            .groups = 'drop'
          ) %>% 
          mutate(
            # Serve percentages and efficiency
            serve_point_percentage = round((serve_points/total_serve)*100,2) ,
            serve_error_percentage = round((serve_errors/total_serve)*100,2),
            serve_efficiency = round(((serve_points-serve_errors)/total_serve)*100,2),
          ) %>%  
          ungroup() %>% 
          cbind(dv_xy(.$end_zone, end = "upper")) # Add coordinates for end_zone 
        
      } else {
        NULL
      }
    })
    
    # Reactive dataframe for Serve coordinate 
    heatmap_serve <- reactive({
      if (input$skill == "Serve") {
        filtered_data() %>% 
          filter(skill == "Serve",player_name == input$player) %>% 
          mutate(evaluation = case_when(
            evaluation_code == "#" ~ "serve_points",
            evaluation_code == "=" ~ "serve_errors",
            evaluation_code %in% c("-", "!") ~ "serve_bad",
            evaluation_code %in% c("+", "/") ~ "serve_positive",
            TRUE ~ "Other"
          )) 
      }
    })
    
    # Reactive dataframe for Reception 
    zone_reception <- reactive({
      if (input$skill == "Reception" && input$plot_type_reception == "Zone Heatmap") {
        # Add logic for Reception Zone Heatmap
        zone_reception <- filtered_data() %>% 
          group_by(player_name,end_zone) %>% 
          summarize(
            # Reception metrics 
            total_reception = sum(ifelse(skill == "Reception", 1, 0), na.rm = TRUE),
            reception_error = sum(ifelse(skill == 'Reception' & evaluation_code == '=', 1, 0), na.rm = TRUE),
            reception_bad = sum(ifelse(skill == 'Reception' & evaluation_code %in% c("-", "!", "/"), 1, 0), na.rm = TRUE),
            reception_positive = sum(ifelse(skill == 'Reception' & evaluation_code == '+', 1, 0), na.rm = TRUE),
            reception_perfect = sum(ifelse(skill == 'Reception' & evaluation_code == '#', 1, 0), na.rm = TRUE),
            .groups = 'drop'
          ) %>% 
          mutate(
            # Reception percentages and efficiency
            reception_positivity = round(((reception_positive+reception_perfect) / total_reception) * 100, 2),
            reception_efficiency = round((((reception_positive+reception_perfect) - reception_error) / total_reception) * 100, 2)
          ) %>%  
          ungroup() %>% 
          # Add coordinates for end_zone
          cbind(dv_xy(.$end_zone, end = "upper"))
        
      } else {
        NULL
      }
    })
    
    # Reactive dataframe for Reception coordinate 
    heatmap_reception <- reactive({
      if (input$skill == "Reception") {
        # Add logic for Reception coordinate
        filtered_data() %>% 
          filter(skill == "Reception",player_name == input$player) %>% 
          mutate(evaluation = case_when(
            evaluation_code == "=" ~ "reception_error",
            evaluation_code %in% c("-", "!", "/") ~ "reception_bad",
            evaluation_code == "+" ~ "reception_positive",
            evaluation_code == "#" ~ "reception_perfect",
            TRUE ~ "Other"
          )) 
      }
    })
    
    # Reactive dataframe for Block 
    zone_block <- reactive({
      if (input$skill == "Block" && input$plot_type_block == "Zone Heatmap") {
        # Add logic for Block Zone Heatmap
        zone_block <- filtered_data() %>% 
          group_by(player_name,end_zone) %>% 
          summarize(
            # Block metrics
            block_points = sum(ifelse(skill == 'Block' & evaluation_code == "#", 1, 0), na.rm = TRUE),
            block_errors = sum(ifelse(skill == 'Block' & evaluation_code %in% c("=", "/"), 1, 0), na.rm = TRUE),
            block_positive = sum(ifelse(skill == 'Block' & evaluation_code == "+", 1, 0), na.rm = TRUE),
            block_poor = sum(ifelse(skill == 'Block' & evaluation_code %in% c("!", "-"), 1, 0), na.rm = TRUE),
            .groups = 'drop'
          ) %>% 
          mutate(
            # Block efficiency
            block_efficiency = round(((block_points+block_positive-block_errors)/(block_points+block_errors+block_positive+block_poor))*100,2)
          ) %>%  
          ungroup() %>% 
          # Add coordinates for end_zone
          cbind(dv_xy(.$end_zone, end = "lower"))
        
      } else {
        NULL
      }
    })
    
    # Reactive dataframe for Block coordinate
    heatmap_block <- reactive({
      if (input$skill == "Block") {
        # Add logic for Block coordinate
        filtered_data() %>% 
          filter(skill == "Block",player_name == input$player) %>% 
          mutate(evaluation = case_when(
            evaluation_code == "#" ~ "block_points",
            evaluation_code %in% c("=", "/") ~ "block_errors",
            evaluation_code == "-" ~ "block_positive",
            evaluation_code %in% c("!", "-") ~ "block_poor",
            TRUE ~ "Other"
          )) 
      }
    })
    
    # Reactive dataframe for Dig 
    zone_dig <- reactive({
      if (input$skill == "Dig" && input$plot_type_dig == "Zone Heatmap") {
        # Add logic for Dig Zone Heatmap
        zone_dig <- filtered_data() %>% 
          group_by(player_name,end_zone) %>% 
          summarize(
            # Dig metrics
            dig_total = sum(ifelse(skill == "Dig", 1, 0), na.rm = TRUE),
            dig_error = sum(ifelse(skill == 'Dig' & evaluation_code == '=', 1, 0), na.rm = TRUE),
            dig_perfect = sum(ifelse(skill == 'Dig' & evaluation_code == '#', 1, 0), na.rm = TRUE),
            dig_good = sum(ifelse(skill == 'Dig' & evaluation_code %in% c("+", "!"), 1, 0), na.rm = TRUE),
            dig_freeball = sum(ifelse(skill == 'Dig' & evaluation_code == '-', 1, 0), na.rm = TRUE),
            .groups = 'drop'
          ) %>% 
          mutate(
            # Dig efficiency
            dig_efficiency = round(((dig_perfect+dig_good-dig_error)/dig_total)*100,2)
          ) %>%  
          ungroup() %>% 
          # Add coordinates for end_zone
          cbind(dv_xy(.$end_zone, end = "upper"))
        
      } else {
        NULL
      }
    })
    
    # Reactive dataframe for Dig coordinate 
    heatmap_dig <- reactive({
      if (input$skill == "Dig") {
        # Add logic for Dig coordinate
        filtered_data() %>%
          filter(skill == "Dig", player_name == input$player) %>% 
          mutate(evaluation = case_when(
            evaluation_code == "=" ~ "dig_error",
            evaluation_code == '-' ~ "dig_freeball",
            evaluation_code%in% c("+", "!") ~ "dig_good",
            evaluation_code == "#" ~ "dig_perfect",
            TRUE ~ "Other"
          )) 
      }
    })
    
    # Plot outputs (original)
    # Attack Plot
    output$attackPlot <- renderPlot({
      
      if (input$plot_type_attack == "Zone Heatmap" && !is.null(zone_attack())) {
        ggplot(zone_attack(), aes(x, y, fill = .data[[input$evaluation_zone_attack]])) +
          geom_tile() +
          geom_text(aes(label = round(.data[[input$evaluation_zone_attack]], 2)), color = "white", size = 5) +
          ggcourt(labels = input$player, court = "upper") +
          scale_fill_gradient2(name = input$evaluation_zone_attack) +
          theme_minimal()
      } else if (input$plot_type_attack == "Dot Heatmap") {
        ggplot(heatmap_attack() %>% filter(evaluation %in% input$evaluation_dot_attack), 
               aes(x = end_coordinate_x, y = end_coordinate_y, color = evaluation)) +
          ggcourt(labels = input$player, court = "upper") +
          geom_point(size = 2) +  
          scale_color_manual(
            values = c(
              "Winning attack" = "limegreen", 
              "Error" = "firebrick", 
              "Blocked" = "orange", 
              "Other" = "dodgerblue"
            ),
            name = "Evaluation"
          )
      } else if (input$plot_type_attack == "Density Heatmap") {
        hx_attack <- ov_heatmap_kde(
          filtered_data() %>%  
            filter(evaluation == input$evaluation_density_attack) %>% 
            select(end_coordinate_x, end_coordinate_y),
          resolution = "coordinates", court = "upper"
        )
        
        ggplot(hx_attack, aes(x, y, fill = density)) +
          scale_fill_distiller(palette = "Spectral") +
          geom_raster() +
          ggcourt(labels = input$player, court = "upper") + 
          labs(
            fill = "Density",
            title = paste(input$player, input$evaluation_density_attack, "density") 
          )
      } else if (input$plot_type_attack == "Attack Trajectory") {
        ggplot(heatmap_attack() %>% filter(evaluation %in% input$evaluation_trajectory_attack), 
               aes(x = start_coordinate_x, y = start_coordinate_y,
                   xend = end_coordinate_x, yend = end_coordinate_y, colour = evaluation)) +
          geom_segment(arrow = arrow(length = unit(2, "mm"), type = "closed", angle = 20)) +
          scale_colour_manual(
            values = c(
              "Winning attack" = "limegreen", 
              "Error" = "firebrick", 
              "Blocked" = "orange", 
              "Other" = "dodgerblue"
            ),
            name = "Evaluation"
          ) +
          ggcourt(labels = paste(input$player, "Attacking")) +
          labs(title = paste("Attack Trajectories", input$player))
      }
    })
    outputOptions(output, "attackPlot", suspendWhenHidden = FALSE)
    
    # Serve Plot
    output$servePlot <- renderPlot({
      
      if (input$plot_type_serve == "Zone Heatmap" && !is.null(zone_serve())) {
        ggplot(zone_serve(), aes(x, y, fill = .data[[input$evaluation_zone_serve]])) +
          geom_tile() +
          geom_text(aes(label = round(.data[[input$evaluation_zone_serve]], 2)), color = "white", size = 5) +
          ggcourt(labels = input$player, court = "upper") + 
          scale_fill_gradient2(name = input$evaluation_zone_serve) +
          theme_minimal()
      } else if (input$plot_type_serve == "Dot Heatmap") {
        ggplot(heatmap_serve() %>% filter(evaluation %in% input$evaluation_dot_serve),
               aes(x = end_coordinate_x, y = end_coordinate_y, color = evaluation)) +
          ggcourt(labels = input$player, court = "upper") +
          geom_point(size = 2) +  
          scale_color_manual(values = c(
            "serve_points" = "limegreen", 
            "serve_errors" = "firebrick", 
            "serve_bad" = "orange", 
            "serve_positive" = "royalblue",
            "Other" = "gray50"),
            name = "Evaluation")
      } else if (input$plot_type_serve == "Density Heatmap") {
        hx_serve <- ov_heatmap_kde(
          filtered_data() %>% 
            filter(evaluation == input$evaluation_density_serve) %>% 
            select(end_coordinate_x, end_coordinate_y),
          resolution = "coordinates", court = "upper")
        
        ggplot(hx_serve, aes(x, y, fill = density)) +
          scale_fill_distiller(palette = "Spectral") +
          geom_raster() +
          ggcourt(labels = input$player, court = "upper") +
          labs(
            fill = "Density",
            title = paste(input$player, input$evaluation_density_serve, "density")
          )
      } else if (input$plot_type_serve == "Serve Trajectory") {
        ggplot(heatmap_serve() %>% filter(evaluation %in% input$evaluation_trajectory_serve),
               aes(x = start_coordinate_x, y = start_coordinate_y,
                   xend = end_coordinate_x, yend = end_coordinate_y, colour = evaluation)) +
          geom_segment(arrow = arrow(length = unit(2, "mm"), type = "closed", angle = 20)) +
          scale_colour_manual(values = c(
            "serve_points" = "limegreen", 
            "serve_errors" = "firebrick", 
            "serve_bad" = "orange", 
            "serve_positive" = "royalblue",
            "Other" = "gray50"),
            name = "Evaluation") +
          ggcourt(labels = paste(input$player, "Serving")) + 
          labs(title = paste("Serve Trajectories", input$player)) 
      }
    })
    outputOptions(output, "servePlot", suspendWhenHidden = FALSE)
    
    # Reception Plot
    output$receptionPlot <- renderPlot({
      
      if (input$plot_type_reception == "Zone Heatmap" && !is.null(zone_reception())) {
        ggplot(zone_reception(), aes(x, y, fill = .data[[input$evaluation_zone_reception]])) +
          geom_tile() +
          geom_text(aes(label = round(.data[[input$evaluation_zone_reception]], 2)), color = "white", size = 5) +
          ggcourt(labels = input$player, court = "upper") + 
          scale_fill_gradient2(name = input$evaluation_zone_reception) +
          theme_minimal()
        
      } else if (input$plot_type_reception == "Dot Heatmap") {
        ggplot(heatmap_reception() %>% filter(evaluation %in% input$evaluation_dot_reception),
               aes(x = end_coordinate_x, y = end_coordinate_y, color = evaluation)) +
          ggcourt(labels = input$player, court = "upper") +
          geom_point(size = 2) +  
          scale_color_manual(values = c(
            "reception_perfect" = "limegreen", 
            "reception_error" = "firebrick", 
            "reception_bad" = "orange", 
            "reception_positive" = "royalblue",
            "Other" = "gray50"),
            name = "Evaluation")
        
      } else if (input$plot_type_reception == "Density Heatmap") {
        hx_reception <- ov_heatmap_kde(
          filtered_data() %>% 
            filter(evaluation == input$evaluation_density_reception) %>%
            select(end_coordinate_x, end_coordinate_y),
          resolution = "coordinates", court = "upper")
        
        ggplot(hx_reception, aes(x, y, fill = density)) +
          scale_fill_distiller(palette = "Spectral") +
          geom_raster() +
          ggcourt(labels = input$player, court = "upper") +
          labs(
            fill = "Density",
            title = paste(input$player, input$evaluation_density_reception, "density")
          )
      }
    })
    outputOptions(output, "receptionPlot", suspendWhenHidden = FALSE)
    
    # Block Plot
    output$blockPlot <- renderPlot({
      
      if (input$plot_type_block == "Zone Heatmap" && !is.null(zone_block())) {
        ggplot(zone_block(), aes(x, y, fill = .data[[input$evaluation_zone_block]])) +
          geom_tile() +
          geom_text(aes(label = round(.data[[input$evaluation_zone_block]], 2)), color = "white", size = 5) +
          ggcourt(labels = input$player, court = "lower") + 
          scale_fill_gradient2(name = input$evaluation_zone_block) +
          theme_minimal()
        
      } else if (input$plot_type_block == "Dot Heatmap") {
        ggplot(heatmap_block() %>% filter(evaluation %in% input$evaluation_dot_block),
               aes(x = start_coordinate_x, y = start_coordinate_y, color = evaluation)) +
          ggcourt(labels = input$player, court = "lower") +
          geom_point(size = 2) +  
          scale_color_manual(values = c(
            "block_points" = "limegreen", 
            "block_errors" = "firebrick", 
            "block_positive" = "orange", 
            "block_poor" = "royalblue",
            "Other" = "gray50"),
            name = "Evaluation")
        
      } else if (input$plot_type_block == "Density Heatmap") {
        hx_block <- ov_heatmap_kde(
          filtered_data() %>% 
            filter(evaluation == input$evaluation_density_block) %>%
            select(start_coordinate_x, start_coordinate_y),
          resolution = "coordinates", court = "lower")
        
        ggplot(hx_block, aes(x, y, fill = density)) +
          scale_fill_distiller(palette = "Spectral") +
          geom_raster() +
          ggcourt(labels = input$player, court = "lower") +
          labs(
            fill = "Density",
            title = paste(input$player, input$evaluation_density_block, "density") 
          )
      }
    })
    
    
    # Dig Plot
    output$digPlot <- renderPlot({
      
      if (input$plot_type_dig == "Zone Heatmap" && !is.null(zone_dig())) {
        ggplot(zone_dig(), aes(x, y, fill = .data[[input$evaluation_zone_dig]])) +
          geom_tile() +
          geom_text(aes(label = round(.data[[input$evaluation_zone_dig]], 2)), color = "white", size = 5) +
          ggcourt(labels = input$player, court = "upper") + 
          scale_fill_gradient2(name = input$evaluation_zone_dig) +
          theme_minimal()
        
      } else if (input$plot_type_dig == "Dot Heatmap") {
        ggplot(heatmap_dig() %>% filter(evaluation %in% input$evaluation_dot_dig),
               aes(x = end_coordinate_x, y = end_coordinate_y, color = evaluation)) +
          ggcourt(labels = input$player, court = "upper") +
          geom_point(size = 2) +  
          scale_color_manual(values = c(
            "dig_perfect" = "limegreen", 
            "dig_error" = "firebrick", 
            "dig_freeball" = "orange", 
            "dig_good" = "royalblue",
            "Other" = "gray50"),
            name = "Evaluation")
        
      } else if (input$plot_type_dig == "Density Heatmap") {
        hx_dig <- ov_heatmap_kde(
          filtered_data() %>% 
            filter(evaluation == input$evaluation_density_dig) %>%
            select(end_coordinate_x, end_coordinate_y),
          resolution = "coordinates", court = "upper")
        
        ggplot(hx_dig, aes(x, y, fill = density)) +
          scale_fill_distiller(palette = "Spectral") +
          geom_raster() +
          ggcourt(labels = input$player, court = "upper") +
          labs(
            fill = "Density",
            title = paste(input$player, input$evaluation_density_dig, "density") 
          )
      }
    })
    
    
    
  })
  
}