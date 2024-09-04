library(caret)
library(rpart)
library(rpart.plot)
library(dplyr)
library(rattle)



# Read the CSV file
df <- read.csv("zomato_dataset_clean.csv")

columns <- c("dining_rating", "delivery_rating", "dining_votes", "delivery_votes", "votes", 'best_seller')

df <- df %>%
  select(all_of(columns)) %>%
  head(5000)

X <- df %>%
  select(-best_seller)

set.seed(42)

y <- as.factor(df$best_seller)

X[is.na(X)] <- 0

clf <- rpart(y ~ ., data = df, method = "class", control = rpart.control(maxdepth = 5))

folds <- createFolds(df$best_seller, k = 5, list = TRUE, returnTrain = FALSE)
scores <- sapply(folds, function(fold) {
  test_indices <- fold
  train_indices <- setdiff(seq_along(df$best_seller), test_indices)
  train_df <- df[train_indices, ]
  test_df <- df[test_indices, ]
  
  pred_y <- predict(clf, newdata = test_df, type = "class")
  accuracy <- sum(pred_y == test_df$best_seller) / nrow(test_df)
  return(accuracy)
})

fancyRpartPlot(clf)


rpart.plot(clf, extra = 101, fallen.leaves = TRUE, under = TRUE, faclen = 0, cex = 0.8, branch.lty = 3, shadow.col = "gray", nn = TRUE)

print("Cross-Validation Scores:")
print(scores)
print("Mean Accuracy:")
print(mean(scores))

# Fit the model on the full dataset
final_clf <- rpart(y ~ ., data = df, method = "class", control = rpart.control(maxdepth = 5))

# Predict on the full dataset
predicted_y <- predict(final_clf, newdata = df, type = "class")

# Create the confusion matrix
conf_matrix <- confusionMatrix(predicted_y, df$best_seller)

# Print the confusion matrix
print(conf_matrix)

# Visualize the confusion matrix
conf_matrix_table <- as.table(conf_matrix$table)
rownames(conf_matrix_table) <- levels(conf_matrix$table$Reference)
colnames(conf_matrix_table) <- levels(conf_matrix$table$Prediction)

library(ggplot2)
library(reshape2)

conf_matrix_melt <- melt(conf_matrix_table)

ggplot(data = conf_matrix_melt, aes(x = Reference, y = Prediction, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = 0.5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix",
       x = "Reference",
       y = "Prediction") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

