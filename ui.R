# Load Helper Functions
source("functions/helper_functions.R")

## Images URL
small_image_url = "https://liangfgithub.github.io/MovieImages/"

# Load Ratings Data
ratings = load_clean_ratings()

# Load Movies Data
movies = load_clean_movies(ratings)

# Get list of rengres
genres = get_genre_list(movies)

# Define UI Page
ui = dashboardPage(
  skin = "blue",
  # Header
  dashboardHeader(title = "Movie Recommender"),
  
  # Sidebar
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Recommend by Genre",
      tabName = "system1",
      icon = icon("film")
    ),
    menuItem(
      "Recommend by Preference",
      tabName = "system2",
      icon = icon("film")
    )
  )),
  
  # Body
  dashboardBody(#includeCSS("css/movies.css"),
    includeCSS("css/custom.css"),
    # Tabs
    tabItems(
      # First tab content
      tabItem(
        tabName = "system1",
        fluidRow(useShinyjs(),
                 align = "center",
                 box(
                   width = 12,
                   h2("Please Select a Genre"),
                   selectInput('genre', '', genres)
                 )),
        fluidRow(
          useShinyjs(),
          align = "center",
          box(
            width = 12,
            h2("Recommended Movies"),
            br(),
            tableOutput("results_s1") %>% withSpinner(size = 1, type = 6)
          )
        ),
      ),
      
      # Second tab content
      tabItem(tabName = "system2",
              fluidRow(
                box(
                  width = 12,
                  title = "Step 1: Rate as Many Movies as Possible",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  div(
                    class = "rateitems",
                    style = "height: 62vh;",
                    uiOutput('ratings') %>% withSpinner(size = 1, type = 6)
                  )
                )
              ),
              fluidRow(
                useShinyjs(),
                box(
                  style = "text-align: center;",
                  width = 12,
                  title = "Step 2: Discover Movie Recommendations",
                  status = "primary",
                  solidHeader = TRUE,
                  br(),
                  actionButton(
                    "btn",
                    "Click here to get your recommendations",
                    class = "btn-primary",
                    style = "margin-bottom: 3rem;"
                  ),
                  br(),
                  tableOutput("results")
                )
              ))
    ))
)