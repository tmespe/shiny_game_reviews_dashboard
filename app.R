if(!require(shiny)) {
  install.packages(c("shiny", "shinydashboard"))
}
# Base
library(tidyverse)
library(lubridate)
library(plotly)
library(purrr)
library(janitor)
library(ggthemes)
library(scales)
library(tm)


# Shiny
library(shiny)
library(shinydashboard)
library(DT)
library(fresh)
library(spsComps)
library(reactable)
#library(bs4Dash)


# Wordcloud
library(wordcloud)
library(tidytext)
library(textdata)


load(url("https://github.com/tmespe/shiny_game_reviews_dashboard/blob/master/data/tidy_games_2022-03-20.RData?raw=true"))
#load("tidy_games_updated.RData.RData")
load(url("https://github.com/tmespe/shiny_game_reviews_dashboard/blob/master/data/igdb_platform_family.RData?raw=true"))

# color scheme : https://colorhunt.co/palette/1b1a17f0a500e45826e6d5b8
mytheme <- create_theme(
  theme = "flatly",
  adminlte_color(
    light_blue = "#1B1A17"
  ),
  adminlte_sidebar(
    width = "320",
    dark_bg = "#1B1A17",
    dark_hover_bg = "#1B1A17",
    dark_color = "#E6D5B8"
  ),
  adminlte_global(
    content_bg = "#E6D5B8", # background of body
    box_bg = "#FEF1B3",     # background for box & header
    info_box_bg = "#E45826"
  )
)

min_year = min(games_with_reviews$first_release_year)
max_year = max(games_with_reviews$first_release_year)
min_rating = min(games_with_reviews$aggregated_rating)
max_rating = max(games_with_reviews$aggregated_rating)


ui <- tagList(
dashboardPage(
  dashboardHeader(title = "Games Dashboard"),

  # Header zone
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem(
      "About",
      tabName = "about",
      icon = icon("file", lib = "font-awesome")
    ),
    menuItem(
      "Platform, genres, themes",
      tabName = "plot",
      icon = icon("images", lib = "font-awesome")
    ),
    menuItem(
      "Stories and titles",
      tabName = "story_title",
      icon = icon("images", lib = "font-awesome")
    ),
    menuItem(
      "Table overview",
      tabName = "table",
      icon = icon("table", lib = "font-awesome")
    ),
    menuItem(
      "Overview",
      tabName = "overview",
      icon = icon("table", lib = "font-awesome"),
      selected = T
    ),
    menuItem(
      "Inputs",
      tabName = "inputvalues",
      startExpanded = TRUE,
      icon = icon("keyboard", lib = "font-awesome"),
      sliderInput(
        "year",
        h4("Year"),
        min = min_year,
        max = max_year,
        value = c(max_year -2, max_year),
        step = 1,
        sep = "",
      ),
      sliderInput(
        "rating",
        h4("Rating"),
        min = min_rating,
        max = max_rating,
        value = c(min_rating, max_rating),
        step = 5,
        sep = "",
      ),
      selectInput("genres", h4("Genre"),
                  choices = c("All genres", games_with_reviews %>%
                                unnest(genres, names_repair = "unique", names_sep = "_") %>%
                                pull(genres_name) %>%
                                unique()),
                  selected = "All genres", multiple = T
      )
    )
    
  )),
  
  dashboardBody(
    tags$head(includeHTML("www/google_analytics.html")),
    use_theme(mytheme),
    tags$style(".small-box.bg-teal { background-color: #F0A500 !important; color: #000000 !important; height: 140px}"),
    tags$style(".small-box.bg-yellow { background-color: #F0A500 !important; color: #000000 !important; height:140px}"),
    tags$style(".small-box.bg-red { background-color: #F0A500 !important; color: #000000 !important; height:140px}"),
    tabItems(
    tabItem(tabName = "overview", 
            fluidRow(valueBoxOutput("box1", width = 4),
                     valueBoxOutput("box2", width = 4),
                    valueBoxOutput("box3", width = 4), class = "text-info"),
            fluidRow(column(3, uiOutput("cover1")),
                     column(3, uiOutput("cover2")),
                     column(3, uiOutput("cover3")),
                     column(3, uiOutput("cover4"))),
            fluidRow(column(3, uiOutput("cover5")),
                     column(3, uiOutput("cover6")),
                     column(3, uiOutput("cover7")),
                     column(3, uiOutput("cover8"))),
            ),
    tabItem(tabName = "story_title",
            fluidRow(
              valueBoxOutput("most_common_title"),
              valueBoxOutput("biggest_collection"),
              valueBoxOutput("most_used_keyword")),
            h2("Wordcloud of most used storyline words"),
            plotOutput("wordcloud"),
            ),
    
    tabItem(tabName = "table",
            titlePanel("Table overview"),
            reactableOutput("table")),
    tabItem(tabName = "plot",
            textOutput(h3("Platform plots")),
            fluidRow(valueBoxOutput("plat_box"), valueBoxOutput("most_recent_plat"),
                     valueBoxOutput("most_pop_plat_fam")),
            plotlyOutput("plot"),
            h2("Genres and themes"),
            plotlyOutput("genre_plot"),
            hr(),
            plotlyOutput("themes_plot"),
            width = "100%"),
    tabItem(tabName = "about",
            includeHTML("www/about.html"),
            style = "font-size:1.5em"
            )
  ))),
    tags$footer(HTML("Designed and developed by <a href=mailto:terje.m.espedal@gmail.com>Terje Espedal</a>"), 
                tags$style("footer {
                              background-color:#1B1A17;
                              color: #E6D5B8;
                              text-align: center;
                              } 
                             a:link {
                              color:#E45826;
                             }"))
)

server <- function(session, input, output) {
  data <-
    reactive(
      if ("All genres" %in% input$genres) {
        games_with_reviews %>% unnest(genres, names_repair = "unique", names_sep = "_") %>% 
          filter(first_release_year >= input$year[1],
                 first_release_year <= input$year[2],
                 aggregated_rating_count > 3,
                 aggregated_rating >= input$rating[1],
                 aggregated_rating <= input$rating[2]
          ) %>% nest(genres = c(genres_name, genres_id))
      } else if (length(input$genres) != F) {
        games_with_reviews %>% unnest(genres, names_repair = "unique", names_sep = "_") %>% 
          filter(first_release_year >= input$year[1],
                 first_release_year <= input$year[2],
                 aggregated_rating_count > 3,
                 aggregated_rating >= input$rating[1],
                 aggregated_rating <= input$rating[2],
                 genres_name %in% input$genres
          ) %>% nest(genres = c(genres_name, genres_id))
      } else {
        games_with_reviews %>% unnest(genres, names_repair = "unique", names_sep = "_") %>% 
          filter(first_release_year >= input$year[1],
                 first_release_year <= input$year[2],
                 aggregated_rating_count > 3,
                 aggregated_rating >= input$rating[1],
                 aggregated_rating <= input$rating[2]
          ) %>% nest(genres = c(genres_name, genres_id))
      }
      
    )
  
  platforms <- reactive(data() %>% unnest(release_dates, names_repair = "unique", names_sep = "_") %>%
                          unnest(platforms, names_repair = "unique", names_sep = "_") %>%
                          unnest(platforms_platform_family, names_repair = "unique", names_sep = "_") %>%
                          filter(release_dates_platform == platforms_id) %>%
                          clean_names() %>% 
                          left_join(igdb_platform_family, by = c("platforms_platform_family" = "id")) %>%
                          rename(name = name.x, platforms_family_name = name.y) %>% 
                          mutate(year = year(as.POSIXct(release_dates_date, origin = "1970-01-01"))) %>%
                          mutate(platform_family_name = ifelse(platforms_name == "PC (Microsoft Windows)",
                                                               "PC", platforms_family_name)) %>%
                          mutate(platform_family_name = factor(replace_na(platform_family_name, "Other"))) %>% 
                          mutate(platforms_name = ifelse(platforms_name == "PC (Microsoft Windows)",
                                                        "PC", platforms_name)))
  
  observeEvent(input$tabs, {
    if (input$tabs == "overview") {
      updateSliderInput(session, "year", value = c(max_year -2, max_year))
    } else {
      updateSliderInput(session, "year", value = c(min_year, max_year))
    }
  })
  
  output$table <- renderReactable({
    reactable(data() %>% 
                select(name, first_release_year, total_rating, 
                       aggregated_rating, hypes, follows,
                       aggregated_rating_count),
              filterable = TRUE, searchable = TRUE, minRows = 10,
              compact = TRUE,
              #bordered = TRUE,
              outlined = TRUE,
              defaultSorted = list("aggregated_rating" = "desc", "first_release_year" = "desc"))
  })

  
  # Plots
  output$plot <-

    renderPlotly({
      platform_colors <-  c("Linux" = "#807F83", "Nintendo" = "#dd2020", "PlayStation" = "#003087", "Sega" = "#17569b", "Xbox" = "#107C10",
                            "PC" = "#FFB900", "Other" = "black")
      platforms() %>% 
        select(release_dates_date, platform_family_name, platforms_name, year) %>%
        group_by(platform_family_name, year) %>%
        summarise(games_pr_platform = n()) %>%
        ggplot(aes(x = year, y = games_pr_platform, color = platform_family_name)) +
        geom_line(show.legend = FALSE, size = 1.2) +
        facet_grid(platform_family_name ~ ., scales = "free_y", switch = "y") +
        scale_x_continuous(n.breaks = 21) +
        scale_color_manual(values = platform_colors) +
        theme_fivethirtyeight() +
        #theme_minimal() +
        labs(title = "Number of games released per platform over time",
             subtitle = "The death of Sega", y = "Count of games", x = "") +
        theme(strip.text.x = element_text(size = 8, face = "bold"),
              legend.position = "none",
              strip.text.y.left = element_text(angle = 0))
              #legend.position = "left")
              #axis.title.y=element_text(size=14, face="bold"))
    })
  
  output$genre_plot <- 
    renderPlotly({
      data() %>% 
        unnest(genres) %>% 
        group_by(genres_name) %>% 
        summarise(genre_count = n()) %>% 
        ggplot(aes(y = reorder(factor(genres_name), genre_count), x = genre_count, fill = genres_name)) +
        geom_col(show.legend = FALSE) +
        theme_fivethirtyeight() +
        #theme_minimal() +
        scale_x_log10() +
        #theme_minimal() +
        ggtitle("Number of games released by genre") +
        ylab("") +
        xlab("Number of games released") +
        scale_fill_viridis_d(alpha = 0.8, direction = -1) +
        theme(legend.position = "none")
    })
  
  output$themes_plot <- 
    renderPlotly({
      reverselog_trans <- function(base = exp(1)) {
        trans <- function(x) -log(x, base)
        inv <- function(x) base^(-x)
        trans_new(paste0("reverselog-", format(base)), trans, inv, 
                  log_breaks(base = base), 
                  domain = c(1e-100, Inf))
      }
      
      themes <- data() %>% 
        unnest(themes, names_repair = "unique", names_sep = "_") %>% 
        group_by(themes_name) %>% 
        mutate(themes_name = replace(themes_name, themes_name == "4X (explore, expand, exploit, and exterminate)", "4X")) %>% 
        summarise(theme_count = n()) %>% 
        ggplot(aes(y = reorder(themes_name, theme_count), x = theme_count, fill = themes_name)) +
        geom_col(show.legend = FALSE) +
        #coord_flip() +
        theme_fivethirtyeight() +
        scale_x_continuous(trans=reverselog_trans(10)) +
        #theme_minimal() +
        ggtitle("Number of games released by theme") +
        ylab("") +
        xlab("Number of games released") +
        scale_fill_viridis_d(alpha = 0.8, direction = -1) +
        theme(legend.position = "none")
      #ggplotly(themes, tooltip = c("text", "size"))
    })

  output$wordcloud <- renderPlot({
    cloud_data <- data() %>%
      unnest_tokens(word, storyline) %>%
      anti_join(stop_words) %>%
      #select(storyline) %>%
      count(word)
    wordcloud(words = cloud_data$word,
              freq = cloud_data$n, min.freq = 2,
              max.words = 40, random.order = FALSE, rot.per = 0.15,
              colors=brewer.pal(8, "Dark2"),
              scale = c(0.5, 2))
  })
  
  # Plot boxes
  output$plat_box <- renderValueBox({
    platform_count <- platforms() %>% count(platforms_name) %>% 
        count()
    valueBox(
      value = platform_count,
      subtitle = "Total number of platforms",
      #subtitle = games %>% pull(name),
      icon = icon("desktop", lib = "font-awesome"),
      color = "teal",
      width = 12
    )
  })
  
  output$most_recent_plat <- renderValueBox({
    latest_platform <- platforms() %>% group_by(platforms_name) %>% 
      group_by(platforms_name) %>% 
      summarise(min_year = min(first_release_year)) %>% 
      arrange(desc(min_year)) %>% 
      head(1)
    valueBox(
      value = HTML(paste("Year:", latest_platform %>% pull(min_year), "<br>Platform: ", latest_platform %>% pull(platforms_name))),
      subtitle = "Newest platform",
      #subtitle = games %>% pull(name),
      icon = icon("desktop", lib = "font-awesome"),
      color = "teal",
      width = 12
    )
  })
  
  output$most_pop_plat_fam <- renderValueBox({
    platform_count <- platforms() %>% group_by(platforms_name) %>% count(platforms_name) %>% 
      arrange(desc(n)) %>% head(1)
    valueBox(
      value = HTML(paste0("Count: ", platform_count %>% pull(n), "<br>Platform: ", platform_count %>% pull(platforms_name))),
      subtitle = "Platform with most releases",
      #subtitle = games %>% pull(name),
      icon = icon("desktop", lib = "font-awesome"),
      color = "teal",
      width = 12
    )
  })
  
  # Text boxes 
  output$most_common_title <- renderValueBox({
    word_counts <- data() %>% unnest_tokens(word, name) %>%
      anti_join(stop_words) %>%
      mutate(word = str_replace_all(word, "[0-9]", NA_character_)) %>%
      drop_na(word) %>% 
      count(word) %>% 
      arrange(desc(n)) %>% 
      head(1)
    valueBox(
      value = word_counts %>% pull(word),
      subtitle = "Most popular word in title",
      #subtitle = games %>% pull(name),
      icon = icon("compact-disc", lib = "font-awesome"),
      color = "teal",
      width = 12
    )
  })
  
  output$biggest_collection <- renderValueBox({
    collection_count <- data() %>% group_by(collection) %>% 
      drop_na(collection) %>% 
      summarise(col_count = n()) %>%
      arrange(desc(col_count)) %>% 
      head(1)
    valueBox(
      value = HTML(paste(collection_count %>% pull(collection), "<br>Count:", collection_count 
                         %>% pull(col_count))),
      subtitle = "Game with largest amount of games in Series/Collection",
      #subtitle = games %>% pull(name),
      icon = icon("compact-disc", lib = "font-awesome"),
      color = "teal",
      width = 12
    )
  })
  
  output$most_used_keyword <- renderValueBox({
    keywords <- data() %>% unnest(keywords, names_repair = "unique", names_sep = "_") %>% 
      group_by(keywords_name) %>% 
      summarise(keywords_count = n()) %>%
      arrange(desc(keywords_count)) %>% 
      head(1)
    valueBox(
      value = HTML(paste(keywords %>% pull(keywords_name), "<br>Count:", keywords 
                         %>% pull(keywords_count))),
      subtitle = "Most used keyword",
      #subtitle = games %>% pull(name),
      icon = icon("compact-disc", lib = "font-awesome"),
      color = "teal",
      width = 12
    )
  })
  
  # Overview boxes
  output$box1 <- renderValueBox({
    games <- games_with_reviews  %>% filter(first_release_year == max_year) %>% 
      arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), 
              desc(hypes), desc(total_rating_count)) %>% head(1) 
    valueBox(
      value = paste(games %>% pull(aggregated_rating)),
      subtitle = HTML("<strong><h4>", paste(games %>% pull(name), "</strong></h4>", 
                                            " Best rated game this year")),
      #subtitle = games %>% pull(name),
      icon = icon("compact-disc", lib = "font-awesome"),
      color = "teal",
      width = 12
    )
  })
  
  output$box2 <- renderValueBox({
    n <-
      data() %>% nrow()
    valueBox(
      value = n,
      subtitle = HTML("Games with reviews during period/in genre"),
      icon = icon("gamepad", lib = "font-awesome"),
      color = "yellow",
      width = 12,
    )
  })
  
  output$box3 <- renderValueBox({
    n <-
      data() %>% summarise(mean_rating = mean(aggregated_rating)) %>% round(1)
    valueBox(
      value = n,
      subtitle = HTML("Average rating"),
      icon = icon("thumbs-up", lib = "font-awesome"),
      color = "red",
      width = 12  
    )
  })
  
  # Covers
  
  # Row 1
  output$cover1 <- renderUI(
    {
      game_data <- data() %>%
        arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), hypes, total_rating_count) %>%
        slice(1) %>% select(name, cover, aggregated_rating, url)
      html <- paste0("<p><strong><h4>", game_data %>% pull(name), "</h4></strong><h4>Rating: ", 
                     game_data %>% pull(aggregated_rating), "</h4></p>")
      tagList(tags$div(tags$a(href=game_data %>% pull(url),
                              target="_blank",
                              tags$img(src = game_data %>% pull(cover), height = 352, 
                                       width = 264, style = "border: 0.5em solid #E45826")), 
                       tags$div(HTML(html)),
                       class = "text-center",
      ))
    }
  )
  
   output$cover2 <- renderUI(
     {
       game_data <- data() %>%
         arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), hypes, total_rating_count) %>%
         slice(2) %>% select(name, cover, aggregated_rating, url)
       html <- paste0("<p><strong><h4>", game_data %>% pull(name), "</h4></strong><h4>Rating: ", 
                      game_data %>% pull(aggregated_rating), "</h4></p>")
       tagList(tags$div(tags$a(href=game_data %>% pull(url),
                               target="_blank",
                               tags$img(src = game_data %>% pull(cover), height = 352, width = 264, style = "border: 0.5em solid #E45826")), 
                        tags$div(HTML(html)),
                        class = "text-center"
       ))
     }
   )

  output$cover3 <- renderUI(
    {
      game_data <- data() %>%
        arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), hypes, total_rating_count) %>%
        slice(3) %>% select(name, cover, aggregated_rating, url)
      html <- paste0("<p><strong><h4>", game_data %>% pull(name), "</h4></strong><h4>Rating: ", 
                     game_data %>% pull(aggregated_rating), "</h4></p>")
      tagList(tags$div(tags$a(href=game_data %>% pull(url),
                            target="_blank",
                            tags$img(src = game_data %>% pull(cover), height = 352, width = 264, style = "border: 0.5em solid #E45826")), 
                      tags$div(HTML(html)),
                      class = "text-center",
      ))
    }
  )
  
  output$cover4 <- renderUI(
    {
      game_data <- data() %>%
        arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), hypes, total_rating_count) %>%
        slice(4) %>% select(name, cover, aggregated_rating, url)
      html <- paste0("<p><strong><h4>", game_data %>% pull(name), "</h4></strong><h4>Rating: ", 
                     game_data %>% pull(aggregated_rating), "</h4></p>")
      tagList(tags$div(tags$a(href=game_data %>% pull(url),
                              target="_blank",
                              tags$img(src = game_data %>% pull(cover), height = 352, width = 264, style = "border: 0.5em solid #E45826")), 
                       tags$div(HTML(html)),
                       class = "text-center",
                      
      ))
    }
  )
  
  # Row 2
  
  output$cover5 <- renderUI(
    {
      game_data <- data() %>%
        arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), hypes, total_rating_count) %>%
        slice(5) %>% select(name, cover, aggregated_rating, url)
      html <- paste0("<p><strong><h4>", game_data %>% pull(name), "</h4></strong><h4>Rating: ", 
                     game_data %>% pull(aggregated_rating), "</h4></p>")
      tagList(tags$div(tags$a(href=game_data %>% pull(url),
                              target="_blank",
                              tags$img(src = game_data %>% pull(cover), height = 352, width = 264, style = "border: 0.5em solid #E45826")), 
                       tags$div(HTML(html)),
                       class = "text-center",
      ))
    }
  )
  
  output$cover6 <- renderUI(
    {
      game_data <- data() %>%
        arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), hypes, total_rating_count) %>%
        slice(6) %>% select(name, cover, aggregated_rating, url)
      html <- paste0("<p><strong><h4>", game_data %>% pull(name), "</h4></strong><h4>Rating: ", 
                     game_data %>% pull(aggregated_rating), "</h4></p>")
      tagList(tags$div(tags$a(href=game_data %>% pull(url),
                              target="_blank",
                              tags$img(src = game_data %>% pull(cover), height = 352, width = 264, style = "border: 0.5em solid #E45826")), 
                       tags$div(HTML(html)),
                       class = "text-center",
      ))
    }
  )
  
  output$cover7 <- renderUI(
    {
      game_data <- data() %>%
        arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), hypes, total_rating_count) %>%
        slice(7) %>% select(name, cover, aggregated_rating, url)
      html <- paste0("<p><strong><h4>", game_data %>% pull(name), "</h4></strong><h4>Rating: ", 
                     game_data %>% pull(aggregated_rating), "</h4></p>")
      tagList(tags$div(tags$a(href=game_data %>% pull(url),
                              target="_blank",
                              tags$img(src = game_data %>% pull(cover), height = 352, width = 264, style = "border: 0.5em solid #E45826")), 
                       tags$div(HTML(html)),
                       class = "text-center",
      ))
    }
  )
  
  output$cover8 <- renderUI(
    {
      game_data <- data() %>%
        arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), hypes, total_rating_count) %>%
        slice(8) %>% select(name, cover, aggregated_rating, url)
      html <- paste0("<p><strong><h4>", game_data %>% pull(name), "</h4></strong><h4>Rating: ", 
                     game_data %>% pull(aggregated_rating), "</h4></p>")
      tagList(tags$div(tags$a(href=game_data %>% pull(url),
                              target="_blank",
                              tags$img(src = game_data %>% pull(cover), height = 352, width = 264, style = "border: 0.5em solid #E45826")), 
                       tags$div(HTML(html)),
                       class = "text-center",
      ))
    }
  )
  
  output$cover8 <- renderUI(
    {
      game_data <- data() %>%
        arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), hypes, total_rating_count) %>%
        slice(8) %>% select(name, cover, aggregated_rating, url)
      html <- paste0("<p><strong><h4>", game_data %>% pull(name), "</h4></strong><h4>Rating: ", 
                     game_data %>% pull(aggregated_rating), "</h4></p>")
      tagList(tags$div(tags$a(href=game_data %>% pull(url),
                              target="_blank",
                              tags$img(src = game_data %>% pull(cover), height = 352, width = 264, style = "border: 0.5em solid #E45826")), 
                       tags$div(HTML(html)),
                       class = "text-center",
      ))
    }
  )
  
  covers <- reactive( data() %>%
    arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), hypes, total_rating_count) %>%
    head(8) %>% pull(cover))

  text <-  reactive(data() %>%
    arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), hypes, total_rating_count) %>%
    head(8) %>% pull(name))

  links <- reactive(data() %>%
    arrange(desc(aggregated_rating), desc(aggregated_rating_count), desc(follows), hypes, total_rating_count) %>%
    head(8) %>% pull(name))
  
}

# Run the app ----
shinyApp(ui = ui, server = server)  # Aggregates the app.