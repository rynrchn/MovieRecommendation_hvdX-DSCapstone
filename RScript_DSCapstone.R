library(tidyverse)

# Function to calculate RMSE
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


# Calculate RMSE if we predict all ratings in the validation set to be the mean rating in the edx set.
mu <- mean(edx$rating) # Mean rating
y_hat <- rep(mu, nrow(validation))
model_1_rmse <- RMSE(validation$rating, y_hat)

rmse_results <- tibble(Method = "Just the mean",
                       RMSE = model_1_rmse) # Data frame to store RMSE results for comparison
rmse_results


# Find Movie Effect b_i
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating-mu))
head(movie_avgs)


# Find User Effect b_u
user_avg <- edx %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))
head(user_avg)


# Regularisation

# Find tuning parameter lambda via cross-validation

library(caret)

lambdas <- seq(0, 10, 0.25)
reg_rmses <- sapply(lambdas, function(l) {
  
  test_index <- createDataPartition(edx$rating, times=1, p=0.1, list=F)
  train_set <- edx[-test_index,]
  temp <- edx[test_index,]
  cv_set <- temp %>%
    semi_join(train_set, by="movieId") %>%
    semi_join(train_set, by="userId")
  removed <- anti_join(temp, cv_set)
  train_set <- rbind(train_set, removed)
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating-mu)/(l+n()))
  
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating-mu-b_i)/(l+n()))
  
  predicted_ratings <- cv_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, cv_set$rating))
})

lambdas[which.min(reg_rmses)] # Most optimal lambda


# Regularise Movie Effect
l <- 1.5
b_i <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating-mu)/(l+n()))

# Regularise User Effect
b_u <- edx %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating-mu-b_i)/(l+n()))


# Review RMSE of current model vs first model
y_hat <- validation %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(validation$rating, y_hat)
rmse_results <- rbind(rmse_results, data.frame(Method = "Regularised Movie + User Effects",
                                               RMSE = model_2_rmse))
rmse_results

































