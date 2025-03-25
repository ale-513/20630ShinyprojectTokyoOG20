# Preprocessing script for team dashboard


preprocess_data_team <- function(data) {
  df <- data %>% 
    group_by(team) %>% 
    summarize(
      # Total points
      total_serve = sum(ifelse(skill == "Serve", 1, 0), na.rm = TRUE),
      error_serve = sum(ifelse(skill == 'Serve' & evaluation_code == '=', 1, 0), na.rm = TRUE),
      bad_serve = sum(ifelse(skill == 'Serve' & evaluation_code %in% c("-", "!"), 1, 0), na.rm = TRUE),
      positive_serve = sum(ifelse(skill == 'Serve' & evaluation_code %in% c("+", "/"), 1, 0), na.rm = TRUE),
      ace_serve = sum(ifelse(skill == 'Serve' & evaluation_code == '#', 1, 0), na.rm = TRUE),
      
      # Reception
      total_reception = sum(ifelse(skill == "Reception", 1, 0), na.rm = TRUE),
      error_reception = sum(ifelse(skill == 'Reception' & evaluation_code == '=', 1, 0), na.rm = TRUE),
      bad_reception = sum(ifelse(skill == 'Reception' & evaluation_code %in% c("-", "!", "/"), 1, 0), na.rm = TRUE),
      positive_reception = sum(ifelse(skill == 'Reception' & evaluation_code %in% c("+"), 1, 0), na.rm = TRUE),
      perfect_reception = sum(ifelse(skill == 'Reception' & evaluation_code == '#', 1, 0), na.rm = TRUE),
      
      # Attack
      total_attack = sum(skill == "Attack", na.rm = TRUE), 
      attack_points = sum(skill == "Attack" & evaluation == "Winning attack", na.rm = TRUE), 
      attack_errors = sum(skill == "Attack" & evaluation == "Error", na.rm = TRUE),
      attack_blocked = sum(skill == "Attack" & evaluation == "Blocked", na.rm = TRUE),
      
      # Block
      block_points = sum(ifelse(skill == 'Block' & evaluation_code == "#", 1, 0), na.rm = TRUE),
      block_errors = sum(ifelse(skill == 'Block' & evaluation_code %in% c("=", "/"), 1, 0), na.rm = TRUE),
      block_positive = sum(ifelse(skill == 'Block' & evaluation_code == "+", 1, 0), na.rm = TRUE),
      block_poor = sum(ifelse(skill == 'Block' & evaluation_code %in% c("!", "-"), 1, 0), na.rm = TRUE),
      
      # Dig
      successfull_dig = sum(ifelse(skill == 'Dig' & evaluation_code != '=', 1, 0), na.rm = TRUE),
      error_dig = sum(ifelse(skill == 'Dig' & evaluation_code == '=', 1, 0), na.rm = TRUE),
      perfect_dig = sum(ifelse(skill == 'Dig' & evaluation_code == '#', 1, 0), na.rm = TRUE),
      good_dig = sum(ifelse(skill == 'Dig' & evaluation_code %in% c("+", "!"), 1, 0), na.rm = TRUE),
      freeball_dig = sum(ifelse(skill == 'Dig' & evaluation_code == '-', 1, 0), na.rm = TRUE),
      
      # New to add
      total_points = ace_serve + attack_points + block_points,
      WL = total_points - block_errors - error_serve - attack_errors,
      .groups = 'drop'
    ) %>% 
    mutate(
      # Serve percentages and efficiency
      serve_point_percentage = round((ace_serve / total_serve) * 100, 2),
      serve_error_percentage = round((error_serve / total_serve) * 100, 2),
      serve_efficiency = round(((ace_serve - error_serve) / total_serve) * 100, 2),
      
      # Reception percentages and efficiency
      reception_positivity = round(((positive_reception + perfect_reception) / total_reception) * 100, 2),
      reception_efficiency = round((((positive_reception + perfect_reception) - error_reception) / total_reception) * 100, 2),
      
      # Attack percentages and efficiency
      attack_point_percentage = round((attack_points / total_attack) * 100, 2),
      attack_error_percentage = round((attack_errors / total_attack) * 100, 2),
      attack_efficiency = round(((attack_points - attack_errors - attack_blocked) / total_attack) * 100, 2),
      
      # Block efficiency
      block_efficiency = round(((block_points + block_positive - block_errors) / (block_points + block_errors + block_positive + block_poor)) * 100, 2),
      
      # Dig efficiency
      dig_efficiency = round(((perfect_dig + good_dig - error_dig) / successfull_dig) * 100, 2),
    ) %>%
    mutate(across(-c(team), as.numeric))
  
  # Create individual dataframes as list elements
  list(
    best_scorer = df %>% select(team, total_points, WL, attack_points, ace_serve, block_points),
    best_attacker = df %>% select(team, total_attack, attack_points, attack_errors, attack_blocked, attack_point_percentage, attack_efficiency),
    best_blocker = df %>% select(team, block_points, block_errors, block_positive, block_poor, block_efficiency),
    best_server = df %>% select(team, total_serve, ace_serve, error_serve, bad_serve, positive_serve, serve_point_percentage, serve_efficiency),
    best_digger = df %>% select(team, successfull_dig, perfect_dig, good_dig, error_dig, freeball_dig, dig_efficiency),
    best_receiver = df %>% select(team, total_reception, perfect_reception, positive_reception, bad_reception, error_reception, reception_positivity, reception_efficiency)
  )
}


preprocess_match_data_team <- function(data) {
  data %>%
    group_by(team,file_name) %>%
    summarize(
      # Total matches and sets played
      match_played = n_distinct(match_id),  
      sets_played = n_distinct(set_id),
      
      # Total points
      total_serve = sum(ifelse(skill == "Serve", 1, 0), na.rm = TRUE),
      error_serve = sum(ifelse(skill == 'Serve' & evaluation_code == '=', 1, 0), na.rm = TRUE),
      bad_serve = sum(ifelse(skill == 'Serve' & evaluation_code %in% c("-", "!"), 1, 0), na.rm = TRUE),
      positive_serve = sum(ifelse(skill == 'Serve' & evaluation_code %in% c("+", "/"), 1, 0), na.rm = TRUE),
      ace_serve = sum(ifelse(skill == 'Serve' & evaluation_code == '#', 1, 0), na.rm = TRUE),
      
      # Reception
      total_reception = sum(ifelse(skill == "Reception", 1, 0), na.rm = TRUE),
      error_reception = sum(ifelse(skill == 'Reception' & evaluation_code == '=', 1, 0), na.rm = TRUE),
      bad_reception = sum(ifelse(skill == 'Reception' & evaluation_code %in% c("-", "!", "/"), 1, 0), na.rm = TRUE),
      positive_reception = sum(ifelse(skill == 'Reception' & evaluation_code %in% c("+"), 1, 0), na.rm = TRUE),
      perfect_reception = sum(ifelse(skill == 'Reception' & evaluation_code == '#', 1, 0), na.rm = TRUE),
      
      # Attack
      total_attack = sum(skill == "Attack", na.rm = TRUE), 
      attack_points = sum(skill == "Attack" & evaluation == "Winning attack", na.rm = TRUE), 
      attack_errors = sum(skill == "Attack" & evaluation == "Error", na.rm = TRUE),
      attack_blocked = sum(skill == "Attack" & evaluation == "Blocked", na.rm = TRUE),
      
      # Block
      block_points = sum(ifelse(skill == 'Block' & evaluation_code == "#", 1, 0), na.rm = TRUE),
      block_errors = sum(ifelse(skill == 'Block' & evaluation_code %in% c("=", "/"), 1, 0), na.rm = TRUE),
      block_positive = sum(ifelse(skill == 'Block' & evaluation_code == "+", 1, 0), na.rm = TRUE),
      block_poor = sum(ifelse(skill == 'Block' & evaluation_code %in% c("!", "-"), 1, 0), na.rm = TRUE),
      
      # Dig
      successfull_dig = sum(ifelse(skill == 'Dig' & evaluation_code != '=', 1, 0), na.rm = TRUE),
      error_dig = sum(ifelse(skill == 'Dig' & evaluation_code == '=', 1, 0), na.rm = TRUE),
      perfect_dig = sum(ifelse(skill == 'Dig' & evaluation_code == '#', 1, 0), na.rm = TRUE),
      good_dig = sum(ifelse(skill == 'Dig' & evaluation_code %in% c("+", "!"), 1, 0), na.rm = TRUE),
      freeball_dig = sum(ifelse(skill == 'Dig' & evaluation_code == '-', 1, 0), na.rm = TRUE),
      
      # New to add
      total_points = ace_serve + attack_points + block_points,
      WL = total_points - block_errors - error_serve - attack_errors,
      .groups = 'drop'
    ) %>% 
    mutate(
      # Serve percentages and efficiency
      serve_point_percentage = round((ace_serve / total_serve) * 100, 2),
      serve_error_percentage = round((error_serve / total_serve) * 100, 2),
      serve_efficiency = round(((ace_serve - error_serve) / total_serve) * 100, 2),
      
      # Reception percentages and efficiency
      reception_positivity = round(((positive_reception + perfect_reception) / total_reception) * 100, 2),
      reception_efficiency = round((((positive_reception + perfect_reception) - error_reception) / total_reception) * 100, 2),
      
      # Attack percentages and efficiency
      attack_point_percentage = round((attack_points / total_attack) * 100, 2),
      attack_error_percentage = round((attack_errors / total_attack) * 100, 2),
      attack_efficiency = round(((attack_points - attack_errors - attack_blocked) / total_attack) * 100, 2),
      
      # Block efficiency
      block_efficiency = round(((block_points + block_positive - block_errors) / (block_points + block_errors + block_positive + block_poor)) * 100, 2),
      
      # Dig efficiency
      dig_efficiency = round(((perfect_dig + good_dig - error_dig) / successfull_dig) * 100, 2),
    ) %>%
    mutate(across(-c(team, file_name), as.numeric))
}