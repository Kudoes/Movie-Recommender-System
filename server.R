# Load Helper Functions
source("functions/helper_functions.R")

# Images URL
small_image_url = "https://liangfgithub.github.io/MovieImages/"

# Load Ratings Data
ratings = load_clean_ratings()

# Load Movies Data
movies = load_clean_movies(ratings)

# Convert to Sparse Matrix
ratingmat <- get_sparse_matrix(ratings) # user x movie matrix

# Get Genre List of All Movies and their Weighted Ratings
film_genre_mat = get_film_genre_mat(movies, ratings)

# Load Model
model <- readRDS("data/model.rds")

# Function to gather user ratings from UI output
get_user_ratings <- function(value_list) {
  
  # Compile ratings into a table
  dat <- data.table(movie_id = sapply(strsplit(names(value_list), "_"), 
                                      function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(rating) & !is.na(movie_id)]
  dat[rating == " ", rating := 0]
  dat[, ':=' (movie_id = as.numeric(movie_id), rating = as.numeric(rating))]
  dat <- dat[rating > 0]
  
  # Gather indexes of movies rated
  idx = rep(0, length(dat$rating))
  if (length(dat$rating) == 0) {
    idx = nrow(dat$rating)
  } else {
    for (i in 1:length(dat$rating)) {
      tmp_idx = which(as.numeric(colnames(ratingmat)) == dat$movie_id[i])
      idx[i] = tmp_idx
    }
  }
  
  # Convert ratings to sparse matrix format
  user_ratings <- sparseMatrix(i = rep(1, nrow(dat)), # row (user)
                               j = idx, # columns (for movie)
                               x = dat$rating,
                               dims = c(1, ncol(ratingmat)))
  return(user_ratings)
}

# Define server function
server <- function(input, output) {
  
  # System 1: Filter the movies to only include the selected genre
  selectedData <- reactive({
    genre_recommendations(input$genre, 18, movies, film_genre_mat)
  })
  
  # Display recommendations on UI
  output$results_s1 <- renderUI({
    
    # 20 recommendations in total
    num_rows <- 3
    num_cols <- 6
    recom_result1 <- selectedData() # Get recommendations
    
    # Create rows of recommendations in UI
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_cols, function(j) {
        id = recom_result1$MovieID[(i - 1) * num_cols + j]
        box(
          style = "height: 100%; overflow: auto; background-color: #E5E3E1",
          width = 2,
          status = "primary",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_cols + j),
          div(
            class = "box-body-class",
            div(
              style = "text-align:center",
              img(
                src = paste0(
                  small_image_url,
                  movies$MovieID[movies$MovieID == id],
                  '.jpg?raw=true"></img>'
                ),
                height = 200,
                style = "padding: 0.5rem 0 0.5rem 0"
              )
            ),
            div(style = "text-align:center; color: black; font-size: 110%",
                movies$Title[movies$MovieID == id])
          )
        )
      }))) # columns
    }) # rows
  }) # renderUI function
  
  # System 2: Show Movies to Rate Initially
  output$ratings <- renderUI({
    num_rows <- 20
    num_cols <- 6 
    
    # Generate a new random sample of (semi-popular) movies to rate each time
    sample_movies = sample_n((
      ratings %>%
        group_by(MovieID) %>%
        summarize(ratings = n()) %>%
        filter(ratings > 500) %>%
        inner_join(movies, by = "MovieID") %>%
        select(c("MovieID", "Title"))
    ),
    120)
    
    # Generate rows of recommendations on UI
    lapply(1:num_rows, function(i) {
      list(fluidRow(style = "margin-left: 0; margin-right: 0;",
                    lapply(1:num_cols, function(j) {
                      id = sample_movies$MovieID[(i - 1) * num_cols + j]
                      img_src = paste0(small_image_url,
                                       sample_movies$MovieID[sample_movies$MovieID == id],
                                       '.jpg?raw=true"></img>')
                      list(
                        box(
                          width = 2,
                          style = "height: 50%; background-color: #E5E3E1; color: black",
                          div(
                            style = "text-align:center",
                            img(
                              src = img_src,
                              height = 200,
                              style = "padding: 0.5rem 0 0.5rem 0"
                            )
                          ),
                          div(style = "text-align:center; color: black; font-size: 110%", 
                              sample_movies$Title[sample_movies$MovieID == id]),
                          div(style = "text-align:center; font-size: 120%; color: #f0ad4e;",
                              ratingInput(
                                paste0("select_", sample_movies$MovieID[sample_movies$MovieID == id]),
                                label = "",
                                dataStop = 5
                              ))
                        )
                      )
                    })))
    })
  })
  
  # Compute recommendations based on ratings above once button is clicked
  df <- eventReactive(input$btn, {
      # hide the rating container
      useShinyjs()
      jsCode <-
        "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # Get users rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      # Add users ratings as first row of rating matrix
      rmat <- rbind(user_ratings, ratingmat)
      
      # Convert to realRatingMatrix format for recommender
      user_rmat = as(rmat, "realRatingMatrix")
      
      # Get weighted ratings of all the movies
      mv_weighted_ratings <- film_genre_mat %>% select(c(MovieID, W_Rating))
      
      # Run the recommender algorithm with the new user's ratings as input
      res <- generate_recommendations(model, user_rmat[1,], 18, film_genre_mat, ratings)

      # Compile results into a dataframe
      movie_ids = res[[1]]
      titles = rep(0, 18)
      for (i in 1:18) {
        titles[i] = movies[movies$MovieID == movie_ids[i],]$Title
      }
      
      movie_results <- data.frame(
        Rank = 1:18,
        MovieID = movie_ids,
        Title = titles
      )
      return(movie_results)
  }) # clicked on button
  
  
  # Display results on UI
  output$results <- renderUI({
    
    # 18 Total Recommendations
    num_rows <- 3
    num_cols <- 6
    recom_result2 <- df() # Get recommendations
    
    # Create rows of movies
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_cols, function(j) {
        id = recom_result2$MovieID[(i - 1) * num_cols + j]
        box(
          style = "height: 50%; background-color: #E5E3E1; color: black",
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_cols + j),
          div(
            class = "box-body-class",
            div(
              style = "text-align:center",
              img(
                src = paste0(
                  small_image_url,
                  movies$MovieID[movies$MovieID == id],
                  '.jpg?raw=true"></img>'
                ),
                height = 200,
                style = "padding: 0.5rem 0 0.5rem 0"
              )
            ),
            div(style = "text-align:center; color: black; font-size: 110%",
                movies$Title[movies$MovieID == id])
          ))
      }))) # columns
    }) # rows
  }) # renderUI function
}