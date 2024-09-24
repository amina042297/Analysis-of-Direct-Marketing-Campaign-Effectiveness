
library(dplyr)
library(psych)
library(ggplot2)
library(tidyr)
library(reshape2)  
library(caret)
library(glmnet)
library(pROC)


bank_data <- read.csv("/Users/aminabauyrzan/Downloads/bank-marketing.csv")

str(bank_data)
head(bank_data)
sum(is.na(bank_data))

names(bank_data) <- c(
  "age", "job_type", "marital_status", "education", "default_history", 
  "balance", "housing_loan_status", "personal_loan_status", 
  "contact_communication_type", "last_contact_day", "last_contact_month", 
  "last_contact_duration", "campaign_contacts", "days_since_last_contact", 
  "previous_contacts", "previous_campaign_outcome", "class"
)

head(bank_data)

bank_data <- bank_data %>%
  mutate(class = ifelse(class == 1, 0, ifelse(class == 2, 1, class)))
bank_data$class <- as.factor(bank_data$class)
# percentage of responses for each Class 
response_percentages <- bank_data %>%
  group_by(class) %>%
  summarise(Count = n(),
            Total = nrow(bank_data),
            Percentage = (Count / Total) * 100, .groups = 'drop') # Calculate percentage
print(response_percentages)

categorical_columns <- c("job_type", "marital_status", "education", "default_history", 
                         "housing_loan_status", "personal_loan_status", "contact_communication_type", 
                         "last_contact_month", "previous_campaign_outcome")

bank_data[categorical_columns] <- lapply(bank_data[categorical_columns], factor)

bank_data %>%
  dplyr::select(all_of(categorical_columns)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  ggplot(aes(x = value, y = Frequency, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Category", y = "Frequency", title = "Frequency of Categorical Levels") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~variable, scales = "free_x")

data_long <- bank_data %>%
  dplyr::select(all_of(categorical_columns), class) %>%  
  pivot_longer(cols = -class, names_to = "variable", values_to = "value")
frequency_by_class <- data_long %>%
  group_by(class, variable, value) %>%
  summarise(Frequency = n(), .groups = 'drop')
ggplot(frequency_by_class, aes(x = value, y = Frequency, fill = as.factor(class))) +
  geom_bar(stat = "identity", position = position_dodge()) +  
  facet_wrap(~variable, scales = "free_x", nrow = 2) +  
  theme_minimal() +
  labs(x = "Category", y = "Frequency", title = "Frequency of Categorical Levels by Class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_fill_brewer(palette = "Set1", name = "Class")  

proportions_by_class <- data_long %>%
  group_by(variable, value, class) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  ungroup() %>%
  group_by(variable, value) %>%
  mutate(Total = sum(Frequency),  # Total per level across classes
         Proportion = Frequency / Total)  # Proportion per class
ggplot(proportions_by_class, aes(x = value, y = Proportion, fill = as.factor(class))) +
  geom_bar(stat = "identity", position = position_dodge()) +  
  facet_wrap(~variable, scales = "free_x", nrow = 2) + 
  theme_minimal() +
  labs(x = "Category", y = "Proportion", title = "Proportion of Categorical Levels by Class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_fill_brewer(palette = "Set1", name = "Class")  


numeric_data <- bank_data %>% select_if(is.numeric)
descriptive_stats <- psych::describe(numeric_data)
selected_stats <- descriptive_stats %>% select(min, mean, median, max, sd)
print(selected_stats)

long_numeric <- numeric_data %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
ggplot(long_numeric, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(x = "Value", y = "Frequency", title = "Histogram of Numeric Variables")

numeric_vars <- bank_data %>% 
  select_if(is.numeric) %>% 
  names()
long_data <- bank_data %>%
  pivot_longer(cols = all_of(numeric_vars), names_to = "numeric_variable", values_to = "value") %>%
  filter(class %in% c(0, 1)) 
ggplot(long_data, aes(x = as.factor(class), y = value, fill = as.factor(class))) +
  geom_boxplot() +
  facet_wrap(~ numeric_variable, scales = "free_y") +
  theme_minimal() +
  labs(fill = "Class", x = "Class", y = "Value") +
  theme(legend.position = "none")


# Calculate the mean and standard deviation for each variable
stats <- long_data %>% 
  group_by(numeric_variable) %>% 
  summarise(
    mean = mean(value, na.rm = TRUE), 
    sd = sd(value, na.rm = TRUE)
  ) %>% 
  ungroup()

#  z-scores for each observation
long_data <- long_data %>% 
  left_join(stats, by = "numeric_variable") %>% 
  mutate(z_score = (value - mean) / sd) %>%
  mutate(outlier = abs(z_score) > 3)

# Count the number of outliers for each variable
outlier_counts <- long_data %>% 
  group_by(numeric_variable) %>% 
  summarise(outliers = sum(outlier, na.rm = TRUE))
print(outlier_counts)


replace_outliers_with_NA <- function(x) {
  x[abs(x - mean(x, na.rm = TRUE)) > 3 * sd(x, na.rm = TRUE)] <- NA
  return(x)
}
cleaned_data <- bank_data %>%
  mutate(across(where(is.numeric), replace_outliers_with_NA))

bank_data <- na.omit(cleaned_data)

numeric_data <- bank_data %>% select_if(is.numeric)
cor_matrix <- cor(numeric_data, use = "complete.obs")  
melted_cor_matrix <- melt(cor_matrix)
ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) +
  labs(x = "", y = "", title = "Heatmap of Correlation Matrix") +
  coord_fixed()

print(cor_matrix)

str(bank_data)


# Downsampling

table(bank_data$class)
set.seed(123)
majority_class_rows <- bank_data$class == "0"
minority_class_count <- sum(bank_data$class == "1")
downsampled_majority <- bank_data[majority_class_rows, ] %>%
  sample_n(minority_class_count)

# Combine downsampled majority class with minority class
bank_data <- rbind(downsampled_majority, bank_data[!majority_class_rows, ])
table(bank_data$class)


numeric_data <- bank_data %>% select_if(is.numeric) %>% scale(center = TRUE, scale = TRUE)
# Convert categorical variables to dummy variables
categorical_vars <- setdiff(names(bank_data[sapply(bank_data, is.factor) | sapply(bank_data, is.character)]), "class")  # Exclude the class variable
categorical_data <- bank_data %>%
  select(all_of(categorical_vars))
categorical_data <- model.matrix(~ . - 1, data = categorical_data)
# Combine the numeric and categorical data
combined_data <- data.frame(categorical_data, numeric_data)
response <- bank_data$class


# Split into training and test 
set.seed(555)
trainIndex <- createDataPartition(response, p = 0.8, list = FALSE)
x_train <- as.matrix(combined_data[trainIndex, ])
x_test <- as.matrix(combined_data[-trainIndex, ])
y_train <- as.factor(response[trainIndex])
y_test <- as.factor(response[-trainIndex])
y_train <- relevel(y_train, ref = "0")
y_test <- relevel(y_test, ref = "0")


set.seed(555)
ridge_model <- glmnet(x_train, y_train, alpha = 0, family = "binomial")
plot(ridge_model, xvar="lambda", label=TRUE)
# Fit ridge regression model with cross-validation on the training set
set.seed(555)
cv_fit <- cv.glmnet(x_train, y_train, alpha = 0, family = "binomial")
plot(cv_fit, xvar="lambda", label=TRUE)
# Determine the optimal lambda
best_lambda <- cv_fit$lambda.min
print(paste("Best Lambda for Ridge Regression:", best_lambda))

# Refit the model on the training set with the best lambda
final_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda, family = "binomial")
coef(final_model)

# Predict on test set
predictions_prob <- predict(final_model, newx = x_test, type = "response", s = best_lambda)

predictions <- ifelse(predictions_prob > 0.45, 1, 0)

# Confusion Matrix
conf_matrix <- confusionMatrix(factor(predictions), y_test)

# ROC Curve and AUC
roc_result <- roc(response = y_test, predictor = as.numeric(predictions_prob))
auc_value <- auc(roc_result)
plot(roc_result, main = "ROC Curve")
abline(a = 0, b = 1, col = "red", lty = 2)  # Random classifier line

# confusion matrix and AUC
print(conf_matrix)
cat("Area Under the ROC Curve (AUC):", auc_value, "\n")

# performance metrics
accuracy <- conf_matrix$overall['Accuracy']
sensitivity <- conf_matrix$byClass['Sensitivity']
specificity <- conf_matrix$byClass['Specificity']
precision <- conf_matrix$byClass['Precision']
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

cat("Accuracy:", accuracy, "\n")
cat("Sensitivity (Recall):", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1_score, "\n")


write.csv(bank_data, file = "/Users/aminabauyrzan/Downloads/bank_data.csv", row.names = FALSE)



