library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)
library(Matrix)

# Load Helper Functions
source("functions/helper_functions.R")

set.seed(100)

# Load data
data <- load_clean_ratings()

# Convert the dataframe to a realRatingMatrix
data = as(data, "realRatingMatrix")

# Specify a list of algorithms to evaluate
# This compares scheme 1 vs scheme 2
set.seed(100)
algorithms <- list(
  "IBCF - Pearson (100)" = list(name = "IBCF",
                                param = list(method = "pearson",
                                             k = 100)),
  "UBCF - Cosine (5)" = list(
    name = "UBCF",
    param = list(
      method = "cosine",
      normalize = 'Z-score',
      nn = 5
    )
  ),
  "Random" = list(name = "RANDOM", param = NULL)
)

# Define a 10-fold cross-validation scheme
eval_sets <- evaluationScheme(
  data = data,
  method = "cross",
  #train = 0.8,
  k = 10,
  given = 12,
  goodRating = 3
)

# Generate the results
n_recommendations <- c(1, 3, 5, 10, 15, 20, 25, 30)
list_results <- evaluate(x = eval_sets,
                         method = algorithms,
                         n = n_recommendations)

# Save the best performing model
best_model = Recommender(
  getData(eval_sets, "train"),
  method = 'IBCF',
  parameter = list(
    normalize = 'center',
    method = 'pearson',
    k = 100
  )
)
#saveRDS(best_model, "model.rds")