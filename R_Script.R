library(caret)
library(tidyverse)
library(ggplot2)


head(final_holdout_test)
head(edx)
#=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*
                        #Functions
#=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*
# ------------------------------------------
#function to calculate RMSE
# ------------------------------------------
rmse <- function(predictions,actuals){
  sqrt(mean((predictions-actuals)^2))
}
# ------------------------------------------
# Preprocessing Function (Training Data)
# ------------------------------------------
Preprocessing <- function(df) {
  # This function preprocesses the training dataset (edx).
  # - Splits genres into multiple rows and one-hot encodes them.
  # - Converts genre columns into factors (0 or 1).
  # - Computes average movie rating.
  # - Computes average user rating.
  # - Removes duplicate rows.
  # Returns a cleaned dataframe ready for model training.
  
  # Separate genres into multiple rows and one-hot encode
  df_clean <- df %>%
    separate_rows(genres, sep = "\\|") %>%
    mutate(value = 1) %>%
    spread(key = genres, value = value, fill = 0) %>%
    mutate(across(
      .cols = setdiff(names(.), c("userId", "movieId", "rating", "timestamp", "title")),
      .fns = ~ factor(.x, levels = c(0, 1))
    ))
  
  # Add average movie rating (average rating per movie)
  movie_avg_rating <- df_clean %>%
    group_by(movieId) %>%
    summarise(avg_movie_rating = mean(rating, na.rm = TRUE))
  
  df_clean <- left_join(df_clean, movie_avg_rating, by = "movieId")
  
  # Add average user rating (average rating per user)
  user_avg_rating <- df_clean %>%
    group_by(userId) %>%
    summarise(avg_user_rating = mean(rating, na.rm = TRUE))
  
  df_clean <- left_join(df_clean, user_avg_rating, by = "userId")
  
  # Remove duplicates
  df_clean <- df_clean %>% distinct()
  
  return(df_clean)
}
# ------------------------------------------
# Preprocessing Function (Testing Data)
# ------------------------------------------
Preprocessing_Test <- function(df, edx_clean) {
  # This function preprocesses the test dataset (final_holdout_test).
  # - Applies the same genre encoding and factor conversion as training.
  # - Imports average movie rating and average user rating from edx_clean.
  # - Ensures consistency without introducing data leakage.
  # - Replaces missing rating values with NA for unseen users/movies.
  # Returns a cleaned dataframe ready for evaluation.
  
  # Separate genres into multiple rows and one-hot encode
  df_clean <- df %>%
    separate_rows(genres, sep = "\\|") %>%
    mutate(value = 1) %>%
    spread(key = genres, value = value, fill = 0) %>%
    mutate(across(where(is.numeric) & !c(userId, movieId, rating, timestamp, title), ~ factor(.x, levels = c(0, 1))))
  
  # Import average movie rating from edx_clean
  movie_avg_rating <- edx_clean %>%
    select(movieId, avg_movie_rating) %>%
    distinct()
  
  df_clean <- left_join(df_clean, movie_avg_rating, by = "movieId")
  
  # Import average user rating from edx_clean
  user_avg_rating <- edx_clean %>%
    select(userId, avg_user_rating) %>%
    distinct()
  
  df_clean <- left_join(df_clean, user_avg_rating, by = "userId")
  
  # Replace NA values with NA (if a movie/user was never seen in training)
  df_clean <- df_clean %>%
    mutate(avg_movie_rating = replace_na(avg_movie_rating, NA),
           avg_user_rating = replace_na(avg_user_rating, NA))
  
  return(df_clean)
}
#==========================================================
#                   Exploratory Data Analysis (EDA)
#==========================================================
#EDA <- function(df) {
  # This function performs an exploratory data analysis (EDA) on the given dataset.
  # It provides:
  # 1. Missing values check.
  # 2. Timestamp distribution.
  # 3. Correlation between genres and ratings.
  # 4. User and movie rating interaction.
  # 5. Ratings distribution by movie genre.
  
  # ===== 1. Missing Values =====
  cat("===== Checking for Missing Values =====\n")
  print(colSums(is.na(df)))  # Check missing values
  
  # ===== 2. Timestamp Distribution =====
  cat("\n===== Timestamp Distribution =====\n")
  plot1 <- ggplot(df, aes(x = timestamp)) +
    geom_histogram(fill = "blue", bins = 30, alpha = 0.7) +
    labs(title = "Timestamp Distribution", x = "Timestamp", y = "Count") +
    theme_minimal()
  print(plot1)
  
  # ===== 3. Correlation between Genres and Ratings =====
  cat("\n===== Correlation between Genres and Ratings =====\n")
  genre_ratings <- df %>%
    select(starts_with("Action"), starts_with("Adventure"), starts_with("Animation"),
           starts_with("Children"), starts_with("Comedy"), starts_with("Crime"),
           starts_with("Documentary"), starts_with("Drama"), starts_with("Fantasy"),
           starts_with("Film-Noir"), starts_with("Horror"), starts_with("IMAX"),
           starts_with("Musical"), starts_with("Mystery"), starts_with("Romance"),
           starts_with("Sci-Fi"), starts_with("Thriller"), starts_with("War"),
           starts_with("Western"), rating) %>%
    gather(key = "Genre", value = "Value", -rating)
  
  plot2 <- ggplot(genre_ratings, aes(x = Genre, y = rating, color = Genre)) +
    geom_boxplot() +
    labs(title = "Correlation between Genres and Ratings", x = "Genre", y = "Rating") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(plot2)
  
  # ===== 4. User and Movie Rating Interaction =====
  cat("\n===== User and Movie Rating Interaction =====\n")
  user_movie_ratings <- df %>%
    group_by(userId, movieId) %>%
    summarise(avg_rating = mean(rating))
  
  plot3 <- ggplot(user_movie_ratings, aes(x = userId, y = movieId, color = avg_rating)) +
    geom_point(alpha = 0.7) +
    labs(title = "User and Movie Rating Interaction", x = "User ID", y = "Movie ID") +
    theme_minimal()
  print(plot3)
  
  # ===== 5. Ratings Distribution by Movie Genre =====
  cat("\n===== Ratings Distribution by Movie Genre =====\n")
  genre_ratings_summary <- df %>%
    select(starts_with("Action"), starts_with("Adventure"), starts_with("Animation"),
           starts_with("Children"), starts_with("Comedy"), starts_with("Crime"),
           starts_with("Documentary"), starts_with("Drama"), starts_with("Fantasy"),
           starts_with("Film-Noir"), starts_with("Horror"), starts_with("IMAX"),
           starts_with("Musical"), starts_with("Mystery"), starts_with("Romance"),
           starts_with("Sci-Fi"), starts_with("Thriller"), starts_with("War"),
           starts_with("Western"), rating) %>%
    gather(key = "Genre", value = "Value", -rating) %>%
    group_by(Genre) %>%
    summarise(mean_rating = mean(rating), min_rating = min(rating), max_rating = max(rating),
              n_ratings = n())
  
  print(genre_ratings_summary)
  
  # Return all plots and tables
  return(list(Missing_Values = colSums(is.na(df)), Timestamp_Plot = plot1,
              Genre_Ratings_Plot = plot2, User_Movie_Plot = plot3, Genre_Summary = genre_ratings_summary))
}

#=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*
                      #Preprocessing
#=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*

# Apply preprocessing to the training dataset (edx)
edx_clean <- Preprocessing(edx)
# Apply preprocessing to the final holdout test set using edx_clean as reference
Preprocessed_holdout_set <- Preprocessing_Test(final_holdout_test,edx_clean)

#split to train and test sets
set.seed(123)  # Set seed for reproducibility
# Randomly sample 80% of the data for training
train_indices <- sample(1:nrow(edx_clean), 0.8 * nrow(edx_clean))
# Create training and validation sets
train_set <- edx_clean[train_indices, ]  
validation_set <- edx_clean[-train_indices, ]

#=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*
                      #Exploratory Data Analysis (EDA)
#=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*
#EDA(train_set)

#=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*
                        #Modeling
#=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*





#=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*
                        #Testing
#=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=**=*=*=*=*=*=*
# actual_ratings <- final_holdout_test$rating
# predicted_ratings <- predict(model, newdata = final_holdout_test)
# 
# rmse_value <- rmse(predicted_ratings, actual_ratings)
# print(paste("RMSE:", rmse_value))