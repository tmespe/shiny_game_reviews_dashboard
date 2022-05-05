library("jsonlite")
library("httr")
library("dotenv")
library("dplyr")
library("lubridate")
library("mongolite")

# Get API keys from .env file
readRenviron(".Renviron")
client_id <- Sys.getenv("TWITCH_CLIENT_ID")
client_secret <- Sys.getenv("TWITCH_CLIENT_SECRET")
mongo_user <- Sys.getenv("MONGO_DB_USER")
mongo_pass <- Sys.getenv("MONGO_DB_PASS")
mongo_host <- Sys.getenv("MONGO_DB_HOST")

# MongoDB
options(mongodb = list(
  "host" = mongo_host,
  "username" = mongo_user,
  "password" = mongo_pass
))

db_name <- "igdb"
collection_name <- "igdb_games"

save_data <- function(data) {
  # Connect to the database
  db <- mongo(collection = collection_name,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                db_name
              ),
              options = ssl_options(weak_cert_validation = TRUE))
  # Insert the data into the mongo collection as a data.frame
  #data <- as.data.frame(t(data))
  db$insert(data)
}

# Function for getting Twitch token to authenticate IGDB API
get_twitch_token <- function() {
    #' Get a twitch authentication token from the Twitch API
    #'
    #' @description
    #' The get_twitch_token function get's a Twitch bearer token from the
    #'   Twitch API. It will look for TWITCH_CLIENT_ID, and TWITCH_CLIENT_SECRET
    #'    in a .env in the same folder as the script. It if credentials are valid
    #'   a Twitch authentication token is returned
  url <- "https://id.twitch.tv/oauth2/token"

  query_string <- list(
    client_id = client_id,
    #token = paste("Bearer", bearer_token)
    client_secret = client_secret,
    grant_type = "client_credentials"

  )

  response <-
    VERB(
      "POST",
      url,
      query = query_string,
      content_type("application/json"),
      accept("application/json")
    )
  token <- fromJSON(rawToChar(response$content))
  return(token$access_token)
}

# Get data from any IGDB endpoint
get_from_igdb <-
  function(token, endpoint, query_string = "fields *; limit 500;") {
        #' Get data for a given endpoint from IGDB.com
        #'
        #' @description
        #' The get_from_igdb function will return the json content for any given
        #' valid IGDB API endpoint
        #'
        #' @param token A valid twitch bearer token for accessing the API
        #' @param endpoint A valid IGDB endpoint. See https://api-docs.igdb.com/#about
        #'  for valid endpoints
        #' @param query_string A valid query string determining what data to get.
        #' By default returns all fields with the API
        #'   limit of 500 result per page
    base_url <- "https://api.igdb.com/v4/"

    response <-
      VERB(
        "POST",
        paste0(base_url, endpoint),
        add_headers(
          "Content-Type" = "application/json",
          Accept = "application/+json",
          "Client-ID" = client_id,
          "Authorization" = paste("Bearer", token)
        ),
        body = query_string
      )
    if (response$status_code == 200 & length(response$content) > 1) {
      return(list(response, fromJSON(rawToChar(
        response$content
      ))))

    } else if (response$status_code == 400) {
      return()
    } else {
      Sys.sleep(4)
      get_from_igdb(token, endpoint = endpoint, query_string = query_string)
    }
  }

# Download all games from IGDB
get_all_games <- function(n = 190000,
                          incremental_save = FALSE, fields = NULL) {
    #' Save and return a list of all games on IGDB
    #'
    #' @description get_all_games returns a and saves a dataframe to disk with
    #'   all games currently on IGDB.
    #'   The file saved on disk will be games.RData prefixed by YYYYMMDD_HHMMSS.
    #'
    #' @param n The number of games to return. Defaults to 1800000 which
    #'   currently fetches all games.
    #' @param incermental_save Whether to save data incrementaly. Defaults to false.
  # Set query string to get all fields and 500 games per query, which is max.
  fields <- c("id", "name", "external_games.id", "external_games.url", "external_games.countries", "first_release_date",
              "game_modes", "genres.name", "genres", "platforms.name", "platforms.platform_family.name", "platforms.id", "release_dates.date",
              "release_dates.platform", "similar_games.id", "summary", "themes.name", "websites.url",
              "age_ratings.rating", "age_ratings.category", "age_ratings.synopsis", "involved_companies",
              "parent_game", "rating", "rating_count", "total_rating", "total_rating_count", "game_engines.name",
              "game_engines.companies", "keywords.name", "player_perspectives.name", "collection.name", "storyline",
              "hypes", "follows", "aggregated_rating", "aggregated_rating_count", "multiplayer_modes", "cover.url", "url")

  fields <- toString(fields)
  query_string <- paste0("fields ", fields, " ;", "limit 500;", collapse = "")

  # Get twitch token for authentication
  token <- get_twitch_token()

  # Get initial 500 games using get_from_igdb setting endpoint to games
  all_games <-
    get_from_igdb(token, "games", query_string = query_string)[[2]]

  # Create a sequence from 1-n by 500 in order to set offset to skip previous
  # 500 games and get all games.
  for (num in seq(1, n, by = 500)) {
    # Set offset to be previous offset + 500
    query_string <- paste(query_string, "offset", num, ";")
    games <- get_from_igdb(token, "games", query_string)[[2]]
    # Update dataframe with next 500 batch of games
    all_games <- bind_rows(all_games, games)

    # Save every 500 batch to disk if incremental enabled
    if (incremental_save == TRUE) {
      export_json <- toJSON(all_games)
      write(export_json, paste("Saved games", num, sep = "_"))
    }

    # Print batch fetched since process can take a long time
    print(paste("Saved games", num - 500, num))
  }
  #save(all_games, file = paste0(format(Sys.time(), "%Y%m%d_%H%M%S_"), "games.RData"))
  return(all_games)
}

# Get all games from IGDB
games_df <- get_all_games(incremental_save = F, n = 200000)

# Clean data
games_with_reviews <- games_df %>%
  filter(!is.na(aggregated_rating),!is.na(first_release_date)) %>%
  select(
    id,
    first_release_date,
    game_modes,
    genres,
    name,
    platforms,
    themes,
    game_engines,
    keywords,
    similar_games,
    release_dates,
    summary,
    age_ratings,
    player_perspectives,
    total_rating,
    total_rating_count,
    aggregated_rating,
    aggregated_rating_count,
    follows,
    hypes,
    storyline,
    release_dates,
    collection,
    cover,
    url
  ) %>%
  mutate(
    first_release_date = as.Date(as.POSIXct(first_release_date, origin = "1970-01-01")),
    aggregated_rating = round(aggregated_rating, 2),
    total_rating = round(total_rating, 2),
    first_release_year = year(first_release_date),
    cover = str_replace(cover$url, "//", "https://") %>% str_replace("t_thumb", "t_cover_big"),
    collection = collection$name
  ) %>%
  rename(
    game_id = id
  )

#save(games_with_reviews, file = paste0("../data/tidy_games_updated_", Sys.time() ,".RData"))
save_data(games_with_reviews)