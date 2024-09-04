# Load required libraries
library(rpart)
library(dplyr)
# Loading car dataset 
data <- read.csv("zomato_dataset_clean.csv")
columns <- c("dining_rating", "delivery_rating", "dining_votes", "delivery_votes", "votes", "best_seller")

data <- data %>%
  select(all_of(columns)) %>%
  head(500)

# Convert best_seller to factor and set factor levels
data$best_seller <- factor(data$best_seller)

# Defining the target variable and features
target_var <- "best_seller"
features <-  c("dining_rating", "delivery_rating", "dining_votes", "delivery_votes", "votes")

# Defining k for k-fold cross-validation
k <- 5

# Splitting the dataset into k folds
set.seed(123)  # For reproducibility
fold_indices <- split(1:nrow(data), cut(1:nrow(data), breaks = k, labels = FALSE))

# Initializing variables to store Accuracy and confusion matrix
accuracy_info_gain <- vector("numeric", length = k)
accuracy_gini <- vector("numeric", length = k)
accuracy_gain_ratio <- vector("numeric", length = k)

confusion_matrices_info_gain <- list()
confusion_matrices_gini <- list()
confusion_matrices_gain_ratio <- list()

# Performing k-fold cross-validation for each criterion
for (i in 1:k) {
  # Extract the current fold's indices
  test_indices <- fold_indices[[i]]
  train_indices <- unlist(fold_indices[-i])
  
  # Create training and testing datasets
  train_data <- data[train_indices, ]
  test_data <- data[test_indices, ]
  
  # Fit the decision tree model with Information Gain
  decision_tree_info_gain <- rpart(formula(paste(target_var, "~", paste(features, collapse = "+"))),
                                   data = train_data,
                                   method = "class",
                                   parms = list(split = "information"))
  
  # Fit the decision tree model with Gini Index
  decision_tree_gini <- rpart(formula(paste(target_var, "~", paste(features, collapse = "+"))),
                              data = train_data,
                              method = "class",
                              parms = list(split = "gini"))
  
  # Fit the decision tree model with Gain Ratio
  decision_tree_gain_ratio <- rpart(formula(paste(target_var, "~", paste(features, collapse = "+"))),
                                    data = train_data,
                                    method = "class",
                                    parms = list(split = "gainratio"))
  
  # Make predictions on the test data for each criterion
  predictions_info_gain <- predict(decision_tree_info_gain, test_data, type = "class")
  predictions_gini <- predict(decision_tree_gini, test_data, type = "class")
  predictions_gain_ratio <- predict(decision_tree_gain_ratio, test_data, type = "class")
  
  # Set factor levels to match actual test data levels
  predicted_levels <- levels(data$best_seller)
  
  predictions_info_gain <- factor(predictions_info_gain, levels = predicted_levels)
  predictions_gini <- factor(predictions_gini, levels = predicted_levels)
  predictions_gain_ratio <- factor(predictions_gain_ratio, levels = predicted_levels)
  
  # Calculate accuracy for each criterion
  accuracy_info_gain[i] <- mean(predictions_info_gain == test_data$best_seller)
  accuracy_gini[i] <- mean(predictions_gini == test_data$best_seller)
  accuracy_gain_ratio[i] <- mean(predictions_gain_ratio == test_data$best_seller)
  
  # Create confusion matrix for each criterion
  confusion_matrices_info_gain[[i]] <- confusionMatrix(predictions_info_gain, test_data$best_seller)
  confusion_matrices_gini[[i]] <- confusionMatrix(predictions_gini, test_data$best_seller)
  confusion_matrices_gain_ratio[[i]] <- confusionMatrix(predictions_gain_ratio, test_data$best_seller)
}

# Printing confusion matrices for each criterion
for (i in 1:k) {
  cat("Confusion Matrix for Information Gain (Fold", i, "):\n")
  print(confusion_matrices_info_gain[[i]])
  
  cat("Confusion Matrix for Gini Index (Fold", i, "):\n")
  print(confusion_matrices_gini[[i]])
  
  cat("Confusion Matrix for Gain Ratio (Fold", i, "):\n")
  print(confusion_matrices_gain_ratio[[i]])
}
