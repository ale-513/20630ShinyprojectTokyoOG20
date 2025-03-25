# Load necessary libraries
library(httr)
library(jsonlite)
library(rvest)
library(dplyr)

# PROPER CREATION OF THE DATAFRAME "player_data"
rm(list = ls())
load("~/Documents/RStudio/Sport Analytics/Group_project/player_data.RData")

# Function to get Wikipedia page title from player name
get_wikipedia_title <- function(player_name) {
  search_url <- "https://en.wikipedia.org/w/api.php"
  params <- list(
    action = "query",
    format = "json",
    list = "search",
    srsearch = player_name
  )
  
  response <- GET(search_url, query = params)
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  if (!is.null(data$query$search) && length(data$query$search) > 0) {
    return(data$query$search[[1]]$title)
  } else {
    return(NA)
  }
}

# Function to get Wikipedia summary and URL
get_wikipedia_summary <- function(wiki_title) {
  if (is.na(wiki_title)) return(list(summary = NA, url = NA))
  
  summary_url <- paste0("https://en.wikipedia.org/api/rest_v1/page/summary/", gsub(" ", "_", wiki_title))
  response <- GET(summary_url)
  
  if (http_status(response)$category != "Success") return(list(summary = NA, url = NA))
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  return(list(summary = data$extract, url = data$content_urls$desktop$page))
}

# Function to get Wikipedia image URL
get_wikipedia_image <- function(wiki_title) {
  if (is.na(wiki_title)) return(NA)
  
  image_url <- paste0("https://en.wikipedia.org/w/api.php?action=query&format=json&prop=pageimages&pithumbsize=500&titles=", gsub(" ", "_", wiki_title))
  response <- GET(image_url)
  
  if (http_status(response)$category != "Success") return(NA)
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  page_id <- names(data$query$pages)[1]
  
  if (!is.null(data$query$pages[[page_id]]$thumbnail$source)) {
    return(data$query$pages[[page_id]]$thumbnail$source)
  } else {
    return(NA)
  }
}

# Function to get Wikidata ID from Wikipedia page title
get_wikidata_id <- function(wiki_title) {
  if (is.na(wiki_title)) return(NA)
  
  search_url <- paste0("https://en.wikipedia.org/w/api.php?action=query&format=json&prop=pageprops&titles=", gsub(" ", "_", wiki_title))
  response <- GET(search_url)
  
  if (http_status(response)$category != "Success") return(NA)
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  page_id <- names(data$query$pages)[1]
  return(data$query$pages[[page_id]]$pageprops$wikibase_item)
}

# Function to get human-readable labels from Wikidata
get_wikidata_label <- function(entity_id) {
  if (is.na(entity_id)) return(NA)
  
  url <- paste0("https://www.wikidata.org/wiki/Special:EntityData/", entity_id, ".json")
  response <- GET(url)
  
  if (http_status(response)$category != "Success") return(NA)
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  return(data$entities[[entity_id]]$labels$en$value)
}

# Function to scrape missing data from Wikipedia
scrape_wikipedia_info <- function(wiki_title) {
  if (is.na(wiki_title)) return(list(birthdate = NA, height = NA, position = NA, nationality = NA))
  
  wiki_url <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", wiki_title))
  page <- tryCatch(read_html(wiki_url), error = function(e) return(NA))
  
  if (is.na(page)) return(list(birthdate = NA, height = NA, position = NA, nationality = NA))
  
  # Extract birthdate
  birthdate <- page %>%
    html_nodes(xpath = "//table[contains(@class, 'infobox')]//th[contains(text(), 'Born')]/following-sibling::td") %>%
    html_text(trim = TRUE)
  
  # Extract height
  height <- page %>%
    html_nodes(xpath = "//table[contains(@class, 'infobox')]//th[contains(text(), 'Height')]/following-sibling::td") %>%
    html_text(trim = TRUE)
  
  # Extract position
  position <- page %>%
    html_nodes(xpath = "//table[contains(@class, 'infobox')]//th[contains(text(), 'Position')]/following-sibling::td") %>%
    html_text(trim = TRUE)
  
  # Extract nationality
  nationality <- page %>%
    html_nodes(xpath = "//table[contains(@class, 'infobox')]//th[contains(text(), 'Nationality')]/following-sibling::td") %>%
    html_text(trim = TRUE)
  
  return(list(birthdate = birthdate, height = height, position = position, nationality = nationality))
}

# Function to get player details from Wikidata and fill missing values with Wikipedia scraping
get_player_info <- function(wikidata_id, wiki_title) {
  player_info <- get_player_info_from_wikidata(wikidata_id)
  
  # If some values are NA, scrape Wikipedia
  if (is.na(player_info$birthdate) || is.na(player_info$height) || is.na(player_info$position) || is.na(player_info$nationality)) {
    wiki_data <- scrape_wikipedia_info(wiki_title)
    
    if (is.na(player_info$birthdate)) player_info$birthdate <- wiki_data$birthdate
    if (is.na(player_info$height)) player_info$height <- wiki_data$height
    if (is.na(player_info$position)) player_info$position <- wiki_data$position
    if (is.na(player_info$nationality)) player_info$nationality <- wiki_data$nationality
  }
  
  return(player_info)
}

# Read the csv file
library(data.table)
ol21 = fread( "OL21_df.csv" )

# Select only the columns needed, remove all na values and keep only unique player names
library(dplyr)
library(tidyr)
clean_ol21 <- ol21 %>%
  select(player_name, team) %>%
  drop_na(player_name, team) %>%
  distinct(player_name, .keep_all = TRUE) %>%
  filter(!player_name %in% c("Ismail Moalla", "Mauricio Borges Almeida Silva", 
                             "Agustin Loser", "Matias Sanchez", "Federico Pereyra",
                             "Jose Verdi", "Willner Enrique Rivas Quijada",
                             "Velazquez Armando", "Emerson Alexander Rodriguez Gonzalez",
                             "Ronald Fayola", "Edson Alberto valencia Gonzalez",
                             "Hector Mata", "Jose Carrasco", "Luis Arias",
                             "Fernando Gonzalez", "Robert Oramas", "Aleksander Sliwka",
                             "MEISAM SALEHI", "Jakub Kochanowski", "Pawel Zatorski",
                             "Grzegorz Lomacz", "Lukasz Kaczmarek", "Kevin Tillie",
                             "Barthelemy Chinenyeze", "Stephen Boyer", "Trevor Clevenot",
                             "Denis Bogdan", "Michal Kubiak", "Mauricio Luiz De Souza",
                             "Salim Mbareki", "Martin Ramos", "Eliecer Canelo",
                             "Ronald Daniel Fayola HURTADO", "Jose Manuel Carrasco ANGULO",
                             "Luis Antonio Arias GUZMAN", "Robert  Manuel Oramas BRIZUELA",
                             "Eliecer Alfonso Canelo SANCHEZ", "Velazquez ESCALANTE Armando Fernan",
                             "Yassin Kassis", "Chema Chema", "Maita Maita")) %>%
  mutate(player_name = case_when(
    player_name == "Tyler Sanders" ~ "TJ Sanders",
    player_name == "Ivan Zaytsev" ~ "Ivan Zaytsev (volleyball)",
    player_name == "Steven Marshall" ~ "Steven Marshall (volleyball)",
    player_name == "Med Ali Ben Othmen Miledi" ~ "Mohamed Ali Ben Othmen Miladi",
    player_name == "Dmitry Volkov" ~ "Dmitry Volkov (volleyball)",
    player_name == "Ivan YAKOVLEV" ~ "Ivan Iakovlev",
    player_name == "Milad Ebadipour Ghara H." ~ "Milad Ebadipour",
    player_name == "Matt Anderson" ~ "Matt Anderson (volleyball)",
    player_name == "David Smith" ~ "David Smith (volleyball)",
    TRUE ~ player_name  # Keep other values unchanged
  ))

# List of player names
# players <- c("Ivan Zaytsev (volleyball)", "Matteo Piano", "Bruno Rezende")
players <- clean_ol21$player_name

# Create an empty dataframe
player_data <- data.frame()

# Extract data for each player
for (player in players) {
  cat("Processing:", player, "\n")
  
  wiki_title <- get_wikipedia_title(player)
  
  # Skip if Wikipedia title is not found
  if (is.na(wiki_title)) {
    cat("Skipping player (no Wikipedia page):", player, "\n")
    next
  }
  
  summary_info <- get_wikipedia_summary(wiki_title)
  image_url <- get_wikipedia_image(wiki_title)
  wikidata_id <- get_wikidata_id(wiki_title)
  player_info <- get_player_info(wikidata_id, wiki_title)
  
  # Only append valid data
  if (!is.na(summary_info$summary) && !is.na(summary_info$url)) {
    row_data <- data.frame(
      Name = player,
      Wikipedia_Title = wiki_title,
      Wikipedia_URL = summary_info$url,
      Image_URL = image_url,
      Birthdate = player_info$birthdate,
      Height = player_info$height,
      Position = player_info$position,
      Nationality = player_info$nationality,
      Age = player_info$age,
      stringsAsFactors = FALSE
    )
    
    # Add data only if row is valid (i.e., not all values are NA)
    if (nrow(row_data) > 0) {
      player_data <- rbind(player_data, row_data)
    }
  } else {
    cat("Skipping player (no valid summary):", player, "\n")
  }
}

# Fix some values
player_data$Birthdate[player_data$Name == "Jiri Kovar"] <- "10/04/1989"
player_data$Birthdate[player_data$Name == "Sebastian Sole"] <- "12/06/1991"
player_data$Birthdate[player_data$Name == "Yuki Ishikawa"] <- "11/12/1995"
player_data$Birthdate[player_data$Name == "Leon Wilfredo"] <- "31/07/1993"
player_data$Nationality[player_data$Name == "Osmany JUANTORENA"] <- "Italy"
player_data$Nationality[player_data$Name == "TJ Sanders"] <- "Canada"
player_data$Nationality[player_data$Name == "Jiri Kovar"] <- "Italy"
player_data$Nationality[player_data$Name == "Yoandy Leal Hidalgo"] <- "Brazil"
player_data$Nationality[player_data$Name == "Sebastian Sole"] <- "Argentina"
player_data$Nationality[player_data$Name == "Yuki Ishikawa"] <- "Japan"
player_data$Nationality[player_data$Name == "Aliasghar Mojarad"] <- "Iran"
player_data$Nationality[player_data$Name == "Leon Wilfredo"] <- "Poland"
player_data$Position[player_data$Name == "Mahdi Marandi"] <- "Libero"
player_data$Height[player_data$Name == "Jean Patry"] <- "207 cm"

# FIXING SOME COLUMNS OF THE player_data DATAFRAME
# Capitalize the first letter of each word in the column "Position"
library(stringr)
player_data$Position <- str_to_title(player_data$Position)

# Updating the proper values for the column "Age"
library(lubridate)
player_data$Age <- floor(interval(dmy(player_data$Birthdate), Sys.Date()) / years(1))

# Fix the column "Height" with all the values in the format "xxx cm"
library(dplyr)
library(stringr)

player_data <- player_data %>%
  mutate(
    # Remove anything in parentheses or square brackets
    Height_clean = str_remove_all(Height, "\\(.*?\\)|\\[.*?\\]"),
    
    # Trim whitespace
    Height_clean = str_trim(Height_clean),
    
    # Process height: if in meters (m), convert to cm; if in cm, keep as is
    Height_cm = case_when(
      str_detect(Height_clean, "m") & !str_detect(Height_clean, "cm") ~ as.numeric(str_extract(Height_clean, "[0-9]+\\.?[0-9]*")) * 100,
      str_detect(Height_clean, "cm") ~ as.numeric(str_extract(Height_clean, "[0-9]+")),
      TRUE ~ NA_real_
    ),
    
    # Round and format the height as "xxx cm"
    Height = paste0(round(Height_cm), " cm")
  ) %>%
  select(-Height_clean, -Height_cm)
#-------------------------------------------------------------------------------

# PROPER CREATION OF THE DATAFRAME "rem_player_data"
# Load necessary libraries
library(httr)
library(jsonlite)
library(rvest)
library(dplyr)
library(tidyr)

# Function to get Wikipedia page title from player name
get_wikipedia_title <- function(player_name) {
  search_url <- "https://en.wikipedia.org/w/api.php"
  params <- list(
    action = "query",
    format = "json",
    list = "search",
    srsearch = player_name
  )
  
  response <- tryCatch(GET(search_url, query = params), error = function(e) return(NULL))
  if (is.null(response)) return(NA)
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  if (!is.null(data$query$search) && length(data$query$search) > 0) {
    return(data$query$search[[1]]$title)
  } else {
    return(NA)
  }
}

# Function to get Wikipedia summary and URL
get_wikipedia_summary <- function(wiki_title) {
  if (is.null(wiki_title) || length(wiki_title) == 0 || is.na(wiki_title)) return(list(summary = NA, url = NA))
  
  summary_url <- paste0("https://en.wikipedia.org/api/rest_v1/page/summary/", URLencode(gsub(" ", "_", wiki_title), reserved = TRUE))
  response <- tryCatch(GET(summary_url), error = function(e) return(NULL))
  
  if (is.null(response) || http_status(response)$category != "Success") return(list(summary = NA, url = NA))
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  return(list(summary = data$extract, url = data$content_urls$desktop$page))
}

# Function to get Wikipedia image URL
get_wikipedia_image <- function(wiki_title) {
  if (is.null(wiki_title) || length(wiki_title) == 0 || is.na(wiki_title)) return(NA)
  
  image_url <- paste0("https://en.wikipedia.org/w/api.php?action=query&format=json&prop=pageimages&pithumbsize=500&titles=", URLencode(gsub(" ", "_", wiki_title), reserved = TRUE))
  response <- tryCatch(GET(image_url), error = function(e) return(NULL))
  
  if (is.null(response) || http_status(response)$category != "Success") return(NA)
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  page_id <- names(data$query$pages)[1]
  
  if (!is.null(data$query$pages[[page_id]]$thumbnail$source)) {
    return(data$query$pages[[page_id]]$thumbnail$source)
  } else {
    return(NA)
  }
}

# Function to get Wikidata ID from Wikipedia page title
get_wikidata_id <- function(wiki_title) {
  if (is.null(wiki_title) || length(wiki_title) == 0 || is.na(wiki_title)) return(NA)
  
  search_url <- paste0("https://en.wikipedia.org/w/api.php?action=query&format=json&prop=pageprops&titles=", URLencode(gsub(" ", "_", wiki_title), reserved = TRUE))
  response <- tryCatch(GET(search_url), error = function(e) return(NULL))
  
  if (is.null(response) || http_status(response)$category != "Success") return(NA)
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  page_id <- names(data$query$pages)[1]
  return(data$query$pages[[page_id]]$pageprops$wikibase_item)
}

# Function to get human-readable labels from Wikidata
get_wikidata_label <- function(entity_id) {
  if (is.null(entity_id) || length(entity_id) == 0 || is.na(entity_id)) return(NA)
  
  url <- paste0("https://www.wikidata.org/wiki/Special:EntityData/", entity_id, ".json")
  response <- tryCatch(GET(url), error = function(e) return(NULL))
  
  if (is.null(response) || http_status(response)$category != "Success") return(NA)
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  return(data$entities[[entity_id]]$labels$en$value)
}

# Function to scrape missing data from Wikipedia
scrape_wikipedia_info <- function(wiki_title) {
  if (is.null(wiki_title) || length(wiki_title) == 0 || is.na(wiki_title)) return(list(birthdate = NA, height = NA, position = NA, nationality = NA))
  
  wiki_url <- paste0("https://en.wikipedia.org/wiki/", URLencode(gsub(" ", "_", wiki_title), reserved = TRUE))
  page <- tryCatch(read_html(wiki_url), error = function(e) return(NA))
  
  if (is.na(page)) return(list(birthdate = NA, height = NA, position = NA, nationality = NA))
  
  birthdate <- page %>% html_nodes(xpath = "//table[contains(@class, 'infobox')]//th[contains(text(), 'Born') or contains(text(), 'Date of birth')]/following-sibling::td") %>% html_text(trim = TRUE)
  height <- page %>% html_nodes(xpath = "//table[contains(@class, 'infobox')]//th[contains(text(), 'Height') or contains(text(), 'height')]/following-sibling::td") %>% html_text(trim = TRUE)
  position <- page %>% html_nodes(xpath = "//table[contains(@class, 'infobox')]//th[contains(text(), 'Position') or contains(text(), 'Playing position')]/following-sibling::td") %>% html_text(trim = TRUE)
  nationality <- page %>% html_nodes(xpath = "//table[contains(@class, 'infobox')]//th[contains(text(), 'Nationality') or contains(text(), 'Country') or contains(text(), 'Citizenship')]/following-sibling::td") %>% html_text(trim = TRUE)
  
  return(list(birthdate = birthdate, height = height, position = position, nationality = nationality))
}

# Function to get player info from Wikidata (if you have this, ensure it returns list with birthdate, height, position, nationality, age; if not defined yet, define it)
get_player_info_from_wikidata <- function(wikidata_id) {
  if (is.null(wikidata_id) || length(wikidata_id) == 0 || is.na(wikidata_id)) {
    return(list(birthdate = NA, height = NA, position = NA, nationality = NA, age = NA))
  }
  
  url <- paste0("https://www.wikidata.org/wiki/Special:EntityData/", wikidata_id, ".json")
  response <- GET(url)
  
  if (http_status(response)$category != "Success") {
    return(list(birthdate = NA, height = NA, position = NA, nationality = NA, age = NA))
  }
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  entity <- data$entities[[wikidata_id]]
  
  birthdate <- tryCatch(entity$claims$P569[[1]]$mainsnak$datavalue$value$time, error = function(e) NA)
  height <- tryCatch(entity$claims$P2048[[1]]$mainsnak$datavalue$value$amount, error = function(e) NA)
  position <- tryCatch(entity$claims$P413[[1]]$mainsnak$datavalue$value$id, error = function(e) NA)
  nationality <- tryCatch(entity$claims$P27[[1]]$mainsnak$datavalue$value$id, error = function(e) NA)
  
  age <- if (!is.na(birthdate)) {
    birth_year <- as.numeric(substring(birthdate, 2, 5))
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    current_year - birth_year
  } else {
    NA
  }
  
  return(list(birthdate = birthdate, height = height, position = position, nationality = nationality, age = age))
}

# Function to get player details, filling missing fields
get_player_info <- function(wikidata_id, wiki_title) {
  player_info <- get_player_info_from_wikidata(wikidata_id)
  
  # Helper function to safely check if a field is missing or NA
  is_missing_or_na <- function(field) {
    return(is.null(field) || length(field) == 0 || all(is.na(field)) || field == "")
  }
  
  # Ensure player_info is not an empty structure
  if (is_missing_or_na(player_info$birthdate)) {
    wiki_data <- scrape_wikipedia_info(wiki_title)
    if (is_missing_or_na(player_info$birthdate)) {
      player_info$birthdate <- wiki_data$birthdate
    }
  }
  
  if (is_missing_or_na(player_info$height)) {
    wiki_data <- scrape_wikipedia_info(wiki_title)
    if (is_missing_or_na(player_info$height)) {
      player_info$height <- wiki_data$height
    }
  }
  
  if (is_missing_or_na(player_info$position)) {
    wiki_data <- scrape_wikipedia_info(wiki_title)
    if (is_missing_or_na(player_info$position)) {
      player_info$position <- wiki_data$position
    }
  }
  
  if (is_missing_or_na(player_info$nationality)) {
    wiki_data <- scrape_wikipedia_info(wiki_title)
    if (is_missing_or_na(player_info$nationality)) {
      player_info$nationality <- wiki_data$nationality
    }
  }
  
  return(player_info)
}

# Select only the columns needed, remove all na values and keep only unique player names
library(dplyr)
library(tidyr)
leftover_ol21 <- ol21 %>%
  select(player_name, team) %>%
  drop_na(player_name, team) %>%
  distinct(player_name, .keep_all = TRUE) %>%
  filter(player_name %in% c("Ismail Moalla", "Mauricio Borges Almeida Silva", 
                            "Agustin Loser", "Matias Sanchez", "Federico Pereyra",
                            "Jose Verdi", "Willner Enrique Rivas Quijada",
                            "Velazquez Armando", "Emerson Alexander Rodriguez Gonzalez",
                            "Ronald Fayola", "Edson Alberto valencia Gonzalez",
                            "Hector Mata", "Jose Carrasco", "Luis Arias",
                            "Fernando Gonzalez", "Robert Oramas", "Aleksander Sliwka",
                            "MEISAM SALEHI", "Jakub Kochanowski", "Pawel Zatorski",
                            "Grzegorz Lomacz", "Lukasz Kaczmarek", "Kevin Tillie",
                            "Barthelemy Chinenyeze", "Stephen Boyer", "Trevor Clevenot",
                            "Denis Bogdan", "Michal Kubiak", "Mauricio Luiz De Souza",
                            "Salim Mbareki", "Martin Ramos", "Eliecer Canelo",
                            "Ronald Daniel Fayola HURTADO", "Jose Manuel Carrasco ANGULO",
                            "Luis Antonio Arias GUZMAN", "Robert  Manuel Oramas BRIZUELA",
                            "Eliecer Alfonso Canelo SANCHEZ", "Velazquez ESCALANTE Armando Fernan",
                            "Yassin Kassis", "Chema Chema", "Maita Maita")) %>%
  filter(!player_name %in% c("Federico Pereyra", "Jose Carrasco", "Fernando Gonzalez",
                             "Martin Ramos", "Yassin Kassis", "Chema Chema",
                             "Maita Maita", "Denis Bogdan")) %>%
  mutate(player_name = case_when(
    player_name == "Matias Sanchez" ~ "Matías Sánchez (volleyball)",
    player_name == "Federico Pereyra" ~ "Federico Pereyra (volleyball)",
    player_name == "Velazquez Armando" ~ "Armando Velásquez",
    player_name == "Jose Carrasco" ~ "José Carrasco (volleyball)",
    player_name == "Luis Arias" ~ "Luis Arias (volleyball)",
    player_name == "Fernando Gonzalez" ~ "Fernando González (volleyball)",
    player_name == "Salim Mbareki" ~ "Selim Mbareki",
    player_name == "Martin Ramos" ~ "Martín Ramos",
    TRUE ~ player_name  # Keep other values unchanged
  ))

# List of player names
rem_players <- leftover_ol21$player_name

# Create an empty dataframe to store results
rem_player_data <- data.frame(
  Name = character(),
  Wikipedia_Title = character(),
  Wikipedia_Summary = character(),
  Wikipedia_URL = character(),
  Wikidata_ID = character(),
  Birthdate = character(),
  Height = character(),
  Age = numeric(),
  Position = character(),
  Nationality = character(),
  stringsAsFactors = FALSE
)

# Loop through each player and extract data
for (player in rem_players) {
  cat("Processing:", player, "\n")
  
  wiki_title <- get_wikipedia_title(player)
  summary_info <- get_wikipedia_summary(wiki_title)
  wikidata_id <- get_wikidata_id(wiki_title)
  player_info <- get_player_info(wikidata_id, wiki_title)
  
  # Convert Position and Nationality IDs to human-readable names
  position_name <- get_wikidata_label(player_info$position)
  nationality_name <- get_wikidata_label(player_info$nationality)
  
  # Add data to dataframe
  rem_player_data <- rbind(rem_player_data, data.frame(
    Name = player,
    Wikipedia_Title = wiki_title,
    Wikipedia_Summary = summary_info$summary,
    Wikipedia_URL = summary_info$url,
    Wikidata_ID = wikidata_id,
    Birthdate = player_info$birthdate,
    Height = player_info$height,
    Age = player_info$age,
    Position = position_name,
    Nationality = nationality_name,
    stringsAsFactors = FALSE
  ))
}

# Some adjustments for the dataframe "rem_player_data"
library(dplyr)

rem_player_data <- rem_player_data %>%
  select(-Wikipedia_Summary, -Wikidata_ID, -Age) %>%
  mutate(
    Wikipedia_Title = if_else(Name == "Velazquez ESCALANTE Armando Fernan",
                              "Armando Velásquez", Wikipedia_Title),
    Wikipedia_URL = if_else(Name == "Velazquez ESCALANTE Armando Fernan",
                            "https://en.wikipedia.org/wiki/Armando_Vel%C3%A1squez", Wikipedia_URL),
    Birthdate = if_else(Name == "Velazquez ESCALANTE Armando Fernan",
                        "+1988-01-01T00:00:00Z", Birthdate),
    Height = case_when(
      Name == "Stephen Boyer" ~ "+196",
      Name == "Velazquez ESCALANTE Armando Fernan" ~ "1.85 m (6 ft 1 in)",
      TRUE ~ Height
    ),
    Position = case_when(
      Name == "Matías Sánchez (volleyball)" ~ "Setter",
      Name == "Jose Verdi" ~ "Middle Blocker",
      Name == "Willner Enrique Rivas Quijada" ~ "Outside Hitter",
      Name == "Armando Velásquez" ~ "Setter",
      Name == "Emerson Alexander Rodriguez Gonzalez" ~ "Opposite Spiker",
      Name == "Ronald Fayola" ~ "Outside Hitter",
      Name == "Edson Alberto valencia Gonzalez" ~ "Opposite",
      Name == "Hector Mata" ~ "Libero",
      Name == "Luis Arias (volleyball)" ~ "Opposite",
      Name == "Robert Oramas" ~ "Middle Blocker",
      Name == "MEISAM SALEHI" ~ "Outside Hitter",
      Name == "Selim Mbareki" ~ "Middle Blocker",
      Name == "Eliecer Canelo" ~ "Outside Hitter",
      Name == "Ronald Daniel Fayola HURTADO" ~ "Outside Hitter",
      Name == "Jose Manuel Carrasco ANGULO" ~ "Setter",
      Name == "Luis Antonio Arias GUZMAN" ~ "Opposite",
      Name == "Robert  Manuel Oramas BRIZUELA" ~ "Middle Blocker",
      Name == "Eliecer Alfonso Canelo SANCHEZ" ~ "Oustide Hitter",
      Name == "Velazquez ESCALANTE Armando Fernan" ~ "Setter",
      TRUE ~ Position
    ),
    Nationality = case_when(
      Name == "Jose Verdi" ~ "Venezuela",
      Name == "Armando Velásquez" ~ "Venezuela",
      Name == "Ronald Fayola" ~ "Venezuela",
      Name == "Edson Alberto valencia Gonzalez" ~ "Venezuela",
      Name == "Robert Oramas" ~ "Venezuela",
      Name == "MEISAM SALEHI" ~ "Iran",
      Name == "Jakub Kochanowski" ~ "Poland",
      Name == "Eliecer Canelo" ~ "Venezuela",
      Name == "Ronald Daniel Fayola HURTADO" ~ "Venezuela",
      Name == "Robert  Manuel Oramas BRIZUELA" ~ "Venezuela",
      Name == "Eliecer Alfonso Canelo SANCHEZ" ~ "Venezuela",
      Name == "Velazquez ESCALANTE Armando Fernan" ~ "Venezuela",
      TRUE ~ Nationality
    )
  )

# Other adjustments for the dataframe rem_player_data
library(dplyr)
library(stringr)
library(lubridate)

rem_player_data <- rem_player_data %>%
  mutate(
    Birthdate = Birthdate %>%
      str_remove("^\\+") %>%
      str_remove("T00:00:00Z$") %>%
      ymd() %>%
      format("%d/%m/%Y"),
    Position = str_to_title(Position)
  )

# Other adjustments
library(dplyr)
library(stringr)

rem_player_data <- rem_player_data %>%
  mutate(
    Height = case_when(
      str_detect(Height, "m") ~ paste0(
        as.integer(as.numeric(str_extract(Height, "\\d+\\.\\d+")) * 100),
        " cm"
      ),
      str_detect(Height, "^\\+") ~ paste0(
        str_remove(Height, "^\\+"),
        " cm"
      ),
      TRUE ~ Height
    )
  )

# Adding the new columns "Age" and "Image_URL" in the dataset "rem_player_data"
library(dplyr)
library(lubridate)

rem_player_data <- rem_player_data %>%
  # First, calculate the Age column
  mutate(
    Age = as.integer(interval(dmy(Birthdate), Sys.Date()) / years(1))
  ) %>%
  # Then create the Image_URL column
  mutate(
    Image_URL = case_when(
      Name == "Ismail Moalla" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Mauricio Borges Almeida Silva" ~ "https://upload.wikimedia.org/wikipedia/commons/1/12/Mauricio_Borges_Silva_Rio_2016.jpg",
      Name == "Agustin Loser" ~ "https://upload.wikimedia.org/wikipedia/commons/a/a3/Selecci%C3%B3n_de_Voley_Argentina_previo_a_Paris_2024_-_BugWarp_%288%29.jpg",
      Name == "Matías Sánchez (volleyball)" ~ "https://upload.wikimedia.org/wikipedia/commons/4/49/Mat%C3%ADas_S%C3%A1nchez_2024.03.24_17_%28cropped%29.jpg",
      Name == "Jose Verdi" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Willner Enrique Rivas Quijada" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Armando Velásquez" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Emerson Alexander Rodriguez Gonzalez" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Ronald Fayola" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Edson Alberto valencia Gonzalez" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Hector Mata" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Luis Arias (volleyball)" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Robert Oramas" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Aleksander Sliwka" ~ "https://upload.wikimedia.org/wikipedia/commons/b/bb/Aleksander_Sliwka_2018.jpg",
      Name == "MEISAM SALEHI" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Jakub Kochanowski" ~ "https://upload.wikimedia.org/wikipedia/commons/2/28/Volleyball_Nations_League_Poland_-_France_%2842450042692%29_%28cropped%29.jpg",
      Name == "Pawel Zatorski" ~ "https://upload.wikimedia.org/wikipedia/commons/8/8a/Pawe%C5%82_Zatorski_2013_02.jpg",
      Name == "Grzegorz Lomacz" ~ "https://upload.wikimedia.org/wikipedia/commons/1/14/Grzegorz_%C5%81omacz_2010-05-30.jpg",
      Name == "Lukasz Kaczmarek" ~ "https://upload.wikimedia.org/wikipedia/commons/a/a6/%C5%81ukasz_Kaczmarek_%28volleyball_player%29_.jpg",
      Name == "Kevin Tillie" ~ "https://upload.wikimedia.org/wikipedia/commons/6/65/Kevin_Tillie_AVCA_All_American_Banquet_2013.jpg",
      Name == "Barthelemy Chinenyeze" ~ "https://upload.wikimedia.org/wikipedia/commons/0/0d/Barth%C3%A9l%C3%A9my_Chinenyeze_2018.jpg",
      Name == "Stephen Boyer" ~ "https://upload.wikimedia.org/wikipedia/commons/7/79/St%C3%A9phen_Boyer_in_a_match_against_Portugal.jpg",
      Name == "Trevor Clevenot" ~ "https://upload.wikimedia.org/wikipedia/commons/1/1f/Tr%C3%A9vor_Clevenot_%C3%A0_Piacenza_-_2017.png",
      Name == "Michal Kubiak" ~ "https://upload.wikimedia.org/wikipedia/commons/7/7b/Volleyball_Nations_League_Poland_-_France_%2828628996908%29.jpg",
      Name == "Mauricio Luiz De Souza" ~ "https://upload.wikimedia.org/wikipedia/commons/4/47/Maur%C3%ADcio_Souza_Rio_2016.jpg",
      Name == "Selim Mbareki" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Eliecer Canelo" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Ronald Daniel Fayola HURTADO" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Jose Manuel Carrasco ANGULO" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Luis Antonio Arias GUZMAN" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Robert  Manuel Oramas BRIZUELA" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Eliecer Alfonso Canelo SANCHEZ" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      Name == "Velazquez ESCALANTE Armando Fernan" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
      TRUE ~ NA_character_
    )
  ) %>%
  # Reorder columns: place Image_URL between Wikipedia_URL and Birthdate
  relocate(Image_URL, .after = Wikipedia_URL)

# Adjustments in the dataframe player_data
library(dplyr)

player_data <- player_data %>%
  mutate(Image_URL = case_when(
    Name == "TJ Sanders" ~ "https://upload.wikimedia.org/wikipedia/commons/7/79/Tyler_Sanders.jpg",
    Name == "Graham Vigrass" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Steven Marshall (volleyball)" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Jiri Kovar" ~ "https://upload.wikimedia.org/wikipedia/commons/f/f4/Ji%C5%99%C3%AD_Kov%C3%A1%C5%993.JPG",
    Name == "Thales Hoss" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Omar Agrebi" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Mohamed Ali Ben Othmen Miladi" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Ahmed Kadhi" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Elyes Karamosli" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Alan Souza" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Fernando Gil Kreling" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Aymen Bouguerra" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Sebastian Sole" ~ "https://upload.wikimedia.org/wikipedia/commons/8/87/Sebastian_Sol%C3%A9_%28Legavolley_2019%29.jpg",
    Name == "Valentin Golubev" ~ "https://upload.wikimedia.org/wikipedia/commons/c/c0/Valentin_Golubev.jpg",
    Name == "Pavel Pankov" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Masahiro Sekita" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Yuki Ishikawa" ~ "https://upload.wikimedia.org/wikipedia/commons/f/f0/Sirperugiaofficialphotoofyukiishikawa.jpg",
    Name == "Tomohiro Yamamoto" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Taishi Onodera" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Naonobu Fujii" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Kunihiro Shimizu" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Kenta Takanashi" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "HAKU RI" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Leon Wilfredo" ~ "https://upload.wikimedia.org/wikipedia/commons/f/f0/20240701_Wilfredo_Leon.jpg",
    Name == "Masoud Gholami" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Victor Poletaev" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Mitchell Stahl" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "LEE HAKU" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Tatsunori Otsuka" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Arman Salehi" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Ilyas Kurkaev" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    Name == "Maique Reis Nascimento" ~ "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
    TRUE ~ Image_URL  # keep the existing value if no condition is met
  ))

# Merge the two dataframes in a new one
library(dplyr)

full_player_data <- bind_rows(player_data, rem_player_data)

# Adding new rows in the dataframe full_player_data
# Create new rows as data frames
new_rows <- data.frame(
  Name = c("Federico Pereyra", "Jose Carrasco", "Fernando Gonzalez",
           "Martin Ramos", "Yassin Kassis", "Chema Chema",
           "Maita Maita", "Denis Bogdan"),
  Wikipedia_Title = c("Federico Pereyra (volleyball)", "José Carrasco (volleyball)",
                      "Fernando González (volleyball)", "Martín Ramos", "Absent",
                      "José Carrasco (volleyball)", "Luis Arias (volleyball)",
                      "Denis Bogdan"),
  Wikipedia_URL = c("https://en.wikipedia.org/wiki/Federico_Pereyra_(volleyball)",
                    "https://en.wikipedia.org/wiki/Jos%C3%A9_Carrasco_(volleyball)",
                    "https://en.wikipedia.org/wiki/Fernando_Gonz%C3%A1lez_(volleyball)",
                    "https://en.wikipedia.org/wiki/Mart%C3%ADn_Ramos", "Absent",
                    "https://en.wikipedia.org/wiki/Jos%C3%A9_Carrasco_(volleyball)", 
                    "https://en.wikipedia.org/wiki/Luis_Arias_(volleyball)",
                    "https://en.wikipedia.org/wiki/Denis_Bogdan"),
  Image_URL = c("https://upload.wikimedia.org/wikipedia/commons/6/69/Federico_Pereyra_-_Volleyball_at_the_2020_Summer_Olympics_-_Argentina_%28men%29_%28cropped%29.jpg",
                "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
                "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
                "https://upload.wikimedia.org/wikipedia/commons/e/ed/Selecci%C3%B3n_de_Voley_Argentina_previo_a_Paris_2024_-_BugWarp_%2810%29_%28Mart%C3%ADn_Ramos%29.jpg",
                "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg", 
                "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg", 
                "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg",
                "https://upload.wikimedia.org/wikipedia/commons/a/ac/Default_pfp.jpg"),
  Birthdate = c("19/06/1988", "20/05/1989", "30/06/1989", "26/08/1991", "20/10/1996", "20/05/1989",
                "17/01/1989", "13/10/1996"),
  Height = c("200 cm", "195 cm", "194 cm", "198 cm", "183 cm", "195 cm", "196 cm", "200 cm"),
  Position = c("Opposite", "Setter", "Outside Hitter", "Middle Blocker", "Libero", "Setter",
               "Opposite", "Outside Hitter"),
  Nationality = c("Argentina", "Venezuela", "Venezuela", "Argentina", "Tunisia", "Venezuela",
                  "Venezuela", "Russia"),
  Age = c("36", "35", "35", "33", "28", "35", "36", "28"),
  stringsAsFactors = FALSE
)

# Add the new rows to full_player_data
full_player_data <- rbind(full_player_data, new_rows)

# Fixing the column "Age"
full_player_data <- full_player_data %>%
  mutate(
    Age = as.integer(interval(dmy(Birthdate), Sys.Date()) / years(1))
  )

# Saving the data frame "full_player_data" in my pc as a file Excel (.xlsx file)
install.packages("writexl")
library(writexl)

write_xlsx(full_player_data, "/Users/andreamarchesi/Documents/RStudio/Sport Analytics/Group_project/full_player_data.xlsx")

# Saving it as a .csv file
write.csv(full_player_data, "/Users/andreamarchesi/Documents/RStudio/Sport Analytics/Group_project/full_player_data.csv", row.names = FALSE)

#-------------------------------------------------------------------------------
# Saving all the players' images in a folder in my pc (each image is saved with the corresponding player name) 
library(httr)
library(dplyr)
library(stringr)

# Set the directory where you want to save the images (change to your desired folder path)
output_directory <- "/Users/andreamarchesi/Documents/RStudio/Sport Analytics/Group_project/images_full_player_data/"

# Loop through each row of the dataset and download the images
for(i in 1:nrow(full_player_data)) {
  player_name <- full_player_data$Name[i]
  image_url <- full_player_data$Image_URL[i]
  
  # Check the extension from the URL or determine from content type
  if (str_detect(image_url, "\\.jpg$") | str_detect(image_url, "\\.JPG$")) {
    image_extension <- ".jpg"  # Use .jpg for .jpg and .JPG files
  } else if (str_detect(image_url, "\\.png$")) {
    image_extension <- ".png"  # Use .png for .png files
  } else {
    image_extension <- ".jpg"  # Default to .jpg if no known extension is found
  }
  
  # Create the path to save the image using the player's name (with the correct extension)
  image_path <- paste0(output_directory, player_name, image_extension)
  
  # Download the image using httr and save it to the specified path
  try({
    # Perform the download
    GET(image_url, write_disk(image_path, overwrite = TRUE))
    message(paste("Downloaded:", player_name))
  }, silent = TRUE)
}

#-------------------------------------------------------------------------------
# OPTIONAL:
# Reading the csv file "full_player_data"
imported_full_pl_data <- read.csv("/Users/andreamarchesi/Documents/RStudio/Sport Analytics/Group_project/full_player_data.csv")


