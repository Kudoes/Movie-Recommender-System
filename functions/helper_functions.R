# Load libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(shinycssloaders)

# Load and clean ratings data
load_clean_ratings = function() {
  ratings = read.csv(
    "data/ratings.dat",
    sep = ':',
    colClasses = c('integer', 'NULL'),
    header = FALSE
  )
  colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
  
  # Get users with more than 30 ratings
  valid_users <- ratings %>%
    group_by(UserID) %>%
    summarize("ratings" = n()) %>%
    filter(ratings > 30) %>%
    select(UserID)
  
  # Get movies with more than 30 ratings
  valid_movies <- ratings %>%
    group_by(MovieID) %>%
    summarize("ratings" = n()) %>%
    filter(ratings > 30) %>%
    select(MovieID)
  
  # Filter out the users and movies who don't fit above criteria
  ratings <- ratings %>% filter(UserID %in% valid_users$UserID)
  ratings <- ratings %>% filter(MovieID %in% valid_movies$MovieID)
  
  return(ratings)
}

# Load and clean movies data
load_clean_movies = function(ratings) {
  movies = readLines("data/movies.dat")
  movies = strsplit(movies,
                    split = "::",
                    fixed = TRUE,
                    useBytes = TRUE)
  movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
  movies = data.frame(movies, stringsAsFactors = FALSE)
  colnames(movies) = c('MovieID', 'Title', 'Genres')
  movies$MovieID = as.integer(movies$MovieID)
  
  # convert accented characters
  movies$Title = iconv(movies$Title, "latin1", "UTF-8")
  
  # extract year
  movies$Year = as.numeric(unlist(lapply(movies$Title, function(x)
    substr(x, nchar(x) - 4, nchar(x) - 1))))
  
  # Filter out movies that are not in the ratings df
  valid_movie_ids = unique(ratings$MovieID)
  movies = movies %>% filter(MovieID %in% valid_movie_ids)
  
  return(movies)
}

# Load and clean users data
load_clean_users = function() {
  users = read.csv('data/users.dat',
                   sep = ':', header = FALSE)
  users = users[,-c(2, 4, 6, 8)] # skip columns
  colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')
  return(users)
}

# Get a sparse matrix representation of the ratings dataframe
get_sparse_matrix = function(ratings) {
  i = paste0(ratings$UserID)
  j = paste0(ratings$MovieID)
  x = ratings$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  return(Rmat)
}

# Generate a list of all unique genres in the movies matrix
get_genre_list = function(movies) {
  genre_list = c()
  genres = as.data.frame(movies$Genres, stringsAsFactors = FALSE)
  
  # For each film's genre, split the categories and append to list if not already there
  for (i in 1:nrow(genres)) {
    genres_listed = as.vector(strsplit(genres[i, ], '[|]'))
    for (genre in genres_listed[[1]]) {
      if (!genre %in% genre_list) {
        genre_list = append(genre, genre_list)
      }
    }
  }
  genre_list = genre_list %>% sort()
  return(genre_list)
}

# Get the film-genre matrix and then compute weighted ratings for each movie
get_film_genre_mat = function(movies, ratings) {
  ## First compute the basic film genre matrix with flags for genres
  genres = as.data.frame(movies$Genres, stringsAsFactors = FALSE)
  genre_list = get_genre_list(movies)
  tmp = as.data.frame(tstrsplit(genres[, 1], '[|]',
                                type.convert = TRUE),
                      stringsAsFactors = FALSE)
  
  m = length(genre_list)
  film_genre_mat = matrix(0, nrow(movies), length(genre_list))
  for (i in 1:nrow(tmp)) {
    film_genre_mat[i, genre_list %in% tmp[i, ]] = 1
  }
  colnames(film_genre_mat) = genre_list
  remove("tmp", "genres")
  
  film_genre_mat = as.data.frame(film_genre_mat)
  film_genre_mat$MovieID = movies$MovieID
  
  ## Now compute weighted ratings for each film
  # Get ratings per user matrix
  rpu = ratings %>%
    group_by(UserID) %>%
    summarize(ratings_per_user = n())
  
  # Normalize ratings
  std_tmp = ratings %>% group_by(UserID) %>% mutate(norm = scale(Rating))
  
  # Basic arithmetic mean with scaling
  std_avg_ratings = std_tmp %>%
    group_by(MovieID) %>%
    dplyr::summarize(Std_Avg_Rating = mean(norm))
  
  # Ratings per movie matrix
  rpm = ratings %>%
    group_by(MovieID) %>%
    summarize(Total_Ratings = n(),
              Avg_Rating = mean(Rating)) %>%
    inner_join(movies, by = 'MovieID') %>%
    inner_join(std_avg_ratings, by = "MovieID")
  
  # Compute weighted score for each movie
  rankings = rpm %>%
    mutate(W_Rating = weighted_rating(MovieID, rpu, rpm)) %>%
    arrange(desc(W_Rating))
  
  # Add weighted score per movie to film_genre_mat
  film_genre_mat = film_genre_mat %>%
    inner_join(rankings, by = "MovieID") %>%
    select(names(film_genre_mat), "W_Rating") %>%
    arrange(desc(W_Rating))
  
  return(film_genre_mat)
}

# IMDB weighted rating function
# v = number of votes for the movie
# m = minimum votes required to be listed in the chart (40)
# R = average rating of the movie
# C = mean vote across the whole report
weighted_rating = function(id, rpu, rpm) {
  movie = rpm[rpm$MovieID == id,]
  v = movie$Total_Ratings
  # before this, the rating approaches global movie std avg
  # after this, the rating approaches personal movie std avg
  m = quantile(rpu$ratings_per_user, 0.9) # 90% quantile;
  R = movie$Std_Avg_Rating
  C = mean(rpm$Std_Avg_Rating)
  res = (((v) / (v + m)) * R) + (((m) / (v + m)) * C)
  return(res)
}

# Compute a list of recommended movies based on the selected genre
genre_recommendations = function(genre, N, movies, film_genre_mat) {
  res = (film_genre_mat %>% filter(get(genre) == 1)) %>%
    arrange(desc(W_Rating)) %>%
    inner_join(movies, by = "MovieID") %>%
    select("MovieID", "Title", "W_Rating")
  return(res[1:N, ])
}

# Return N recommended movies for a new user
generate_recommendations = function(recommender_model,
                                    user_mat,
                                    n,
                                    mv_weighted_ratings,
                                    ratings) {
  # This returns INDICES of columns of top N movies to recommend, not IDs!
  preds <- predict(recommender_model,
                   user_mat,
                   type = "topNList",
                   n)
  
  # Convert to list
  preds <- preds@items
  
  # Make sure we have at least n recommendations for each user
  for (i in 1:length(preds)) {
    # Get the corresponding MovieID for the index returned
    for (j in 1:length(preds[[i]])) {
      preds[[i]][j] = as.numeric(colnames(user_mat))[preds[[i]][j]]
    }
    
    # If < n recommendations, then recommend the most popular movies not yet seen by them
    if (length(preds[[i]]) < n) {
      userID = as.numeric(names(preds)[i]) # UserID of user
      
      # If there are no recommendations returned, set list to empty
      if (is.na(preds[[i]])) {
        preds[[i]] <- rep(0, 0)
        num_missing = n - length(preds[[i]]) # # of recommendations missing
      } else {
        num_missing = n - length(preds[[i]]) # # of recommendations missing
      }
      
      # Get list of all the movies they've rated
      seen_movies = (ratings %>% filter(UserID == userID) %>% select(MovieID))$MovieID
      
      # Identify top n movies they haven't seen (sorted by weighted rating)
      recs <-
        (mv_weighted_ratings %>% filter(!(MovieID %in% seen_movies)))$MovieID[1:num_missing]
      
      # Append those movies to the recommendations list
      preds[[i]] <- c(preds[[i]], recs)
    }
  }
  return(preds)
}

# Function to trail the model from scratch
train_model = function(data) {
  # Training data is 90% of overall data
  train.id = sample(nrow(data), floor(nrow(data)) * 0.9)
  train = data[train.id,]
  
  # Train the model
  model = Recommender(
    train,
    method = 'IBCF',
    parameter = list(
      normalize = 'center',
      method = 'pearson',
      k = 100
    )
  )
  return(model)
}