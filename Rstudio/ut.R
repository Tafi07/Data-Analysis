library(caret)
library(rpart)
library(dplyr)
library(ggplot2)
library(reshape2)

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

# Predict on the full dataset
predicted_y <- predict(clf, newdata = df, type = "class")

# Ensure predicted_y has the same levels as possible outcomes
predicted_y <- factor(predicted_y, levels = levels(y))

# Create the confusion matrix
conf_matrix <- confusionMatrix(predicted_y, y)

# Print the confusion matrix
print(conf_matrix)

# Visualize the confusion matrix
conf_matrix_table <- as.table(conf_matrix$table)
rownames(conf_matrix_table) <- levels(y)
colnames(conf_matrix_table) <- levels(y)

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
