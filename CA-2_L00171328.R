install.packages("dplyr")
install.packages("psych")
install.packages("car")
install.packages("MASS")
install.packages("ggplot2")

# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(car)
library(corrplot)
library(psych)

# Load the dataset
#data <- read_excel("Dataset_2024.csv")

data <- read.csv("Dataset_2024.csv", na = "")
str(data)


# Data Preprocessing Steps
# 1. Handling missing values
data <- na.omit(data)

# 2. Outlier detection and removal using IQR method
remove_outliers <- function(df) {
  num_cols <- sapply(df, is.numeric)
  df <- df[, num_cols]
  for (col in colnames(df)) {
    Q1 <- quantile(df[[col]], 0.25)
    Q3 <- quantile(df[[col]], 0.75)
    IQR <- Q3 - Q1
    df <- df[df[[col]] >= (Q1 - 1.5 * IQR) & df[[col]] <= (Q3 + 1.5 * IQR), ]
  }
  return(df)
}
data <- remove_outliers(data)

# 3. Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data_norm <- as.data.frame(lapply(data[, sapply(data, is.numeric)], normalize))

# 4. Splitting the dataset into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(data_norm), 0.7 * nrow(data_norm))
train_data <- data_norm[train_index, ]
test_data <- data_norm[-train_index, ]

# Descriptive Statistics
desc_stats <- data %>%
  summarize(
    sample_size = n(),
    mean = mean(`Body fat (%)`),
    sd = sd(`Body fat (%)`),
    median = median(`Body fat (%)`),
    Q1 = quantile(`Body fat (%)`, 0.25),
    Q3 = quantile(`Body fat (%)`, 0.75),
    min = min(`Body fat (%)`),
    max = max(`Body fat (%)`)
  )
print(desc_stats)

# Checking Linearity
for (col in names(data)[-2]) {
  p <- ggplot(data, aes_string(x = col, y = "Body fat (%)")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(paste("Scatterplot of Body fat (%) vs", col)) +
    theme_minimal()
  print(p)
}

# Correlation Coefficients
cor_matrix <- cor(data[, sapply(data, is.numeric)])
corrplot(cor_matrix, method = "number")

# Boxplots
for (col in names(data)[-2]) {
  p <- ggplot(data, aes_string(x = "1", y = col)) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", col)) +
    theme_minimal()
  print(p)
}

# Test for Normality
for (col in names(data)) {
  p <- ggplot(data, aes_string(sample = col)) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("Q-Q Plot of", col)) +
    theme_minimal()
  print(p)
  print(shapiro.test(data[[col]]))
}

# Data Transformations if needed
data$log_body_fat <- log(data$`Body fat (%)`)

# Model Selection
full_model <- lm(`Body fat (%)` ~ ., data = train_data)
summary(full_model)

# Variance Inflation Factor (VIF)
vif_values <- vif(full_model)
print(vif_values)

# Final Model
final_model <- step(full_model, direction = "backward")
summary(final_model)

# Model Diagnostics
par(mfrow = c(2, 2))
plot(final_model)

# Conclusion
cat("Adjusted R2 of the final model:", summary(final_model)$adj.r.squared, "\n")
cat("Regression Coefficients:\n")
print(coef(final_model))
cat("Variance Inflation Factors:\n")
print(vif(final_model))

# Check for conditions placed on residuals
par(mfrow = c(1, 1))
plot(final_model$residuals, main = "Residuals of Final Model", ylab = "Residuals")

