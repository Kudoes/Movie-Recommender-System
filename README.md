# Movie Recommender System

Source Code for STAT 542: PSL - Project 4. UIUC.

This repository contains the source code for the R Shiny application hosted at: https://kashifmkhan.shinyapps.io/movie-recommender/.

---

# Summary

This application utilizes a portion of the MovieLens data to recommend movies to a user based on two different systems. System 1 allows a user to specify a genre, and the movies returned are recommended by a content-based recommender system based on weighted average rating. System 2 asks a user to rate a set of movies, and then those ratings are used as historical data and recommendations are generated for the user based on item-based collaborative filtering. The user is able to select the system they wish to use via the sidebar.

This project was done in R, and deployed on RShiny.

# Folders and Files

1. **css/**: This folder contains the css file used in the Shiny application.

2. **data/**: This folder contains the data files used by the recommender systems in the Shiny application, as well as the pre-trained recommender model.

3. **functions/**: This folder contains additional code used for the recommender systems.

    - **functions/helper_functions.R**: The helper functions R file. 
  
    - **functions/evaluate_CF_models.R**: This file contains the code which is run to compare the two primary CF-recommender algorithms and determine the best recommender.

4. **server.R**: The server code for the Shiny application.

5. **ui.R**: The UI code for the Shiny application.

# Sources

This application was made with the help of information and some code from the following locations:

1. https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf

2. https://github.com/pspachtholz/BookRecommender

3. http://www.cs.carleton.edu/cs_comps/0607/recommend/recommender/itembased.html

4. http://www.bgu.ac.il/~shanigu/Publications/EvaluationMetrics.17.pdf

5. https://help.imdb.com/article/imdb/track-movies-tv/ratings-faq/G67Y87TFYYP6TWAV

6. Professor Liang’s Starter Code on Piazza
