#LOADING THE DATASET
setwd("C:/Users/ankit/Desktop/")
df <- read.csv("C:/Users/ankit/Desktop/India_demo.csv")

#EDA
head(df)


str(df)


summary(df)

missing_values <- colSums(is.na(df))
print(missing_values)


library(ggplot2)
library(corrplot)
library(reshape2)


if('Region' %in% colnames(df)){
  region_counts <- table(df$Region)
  pie(region_counts, main="Distribution of Regions", col=rainbow(length(region_counts)))
}

# Correlation heatmap
numeric_columns <- sapply(df, is.numeric)
cor_matrix <- cor(df[, numeric_columns], use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# Pair plot (scatterplot matrix)
pairs(df[, numeric_columns], main = "Scatterplot Matrix")

# Distribution of Life Expectancy
ggplot(df, aes(x = `Life.Expectancy`)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Life Expectancy", x = "Life Expectancy", y = "Frequency")

# Relationship between Life Expectancy and Income
ggplot(df, aes(x = Income, y = `Life.Expectancy`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "red") +
  labs(title = "Life Expectancy vs Income", x = "Income", y = "Life Expectancy")

# Boxplot of Life Expectancy by Region (if Region is a categorical variable)
if('Region' %in% colnames(df)){
  ggplot(df, aes(x = Region, y = `Life.Expectancy`, fill = Region)) +
    geom_boxplot() +
    labs(title = "Life Expectancy by Region", x = "Region", y = "Life Expectancy")
}









#MLR
india_demo <- read.csv("India_demo.csv")


model <- lm(`Life.Expectancy` ~ Population + Income + Illiteracy + Murder + `HS.Grad` + Frost + Area, data = india_demo)

r_squared <- summary(model)$r.squared
print(paste("R-squared:", round(r_squared, 4)))


#LASSO REG
library(glmnet)




X <- as.matrix(df[, c("Population", "Income", "Illiteracy", "Murder", "HS.Grad", "Frost", "Area")])
y <- india_demo$`Life.Expectancy`

lasso_model <- cv.glmnet(X, y, alpha = 1)  # alpha = 1 specifies Lasso regression


best_lambda <- lasso_model$lambda.min

print(paste("Optimal Lambda (min):", best_lambda))




coefficients <- coef(lasso_model, s = "lambda.min")
print(coefficients)

r_squared <- cor(y, predict(lasso_model, s = "lambda.min", newx = X))^2
print(paste("R-squared (Lasso):", round(r_squared, 4)))



#RIDGE REGRESSION
library(glmnet)





X <- as.matrix(df[, c("Population", "Income", "Illiteracy", "Murder", "HS.Grad", "Frost", "Area")])
y <- india_demo$`Life.Expectancy`


ridge_model <- cv.glmnet(X, y, alpha = 0)  # alpha = 0 specifies Ridge regression


best_lambda <- ridge_model$lambda.min

print(paste("Optimal Lambda (min):", best_lambda))


coefficients <- coef(ridge_model, s = "lambda.min")
print(coefficients)


r_squared <- cor(y, predict(ridge_model, s = "lambda.min", newx = X))^2
print(paste("R-squared (Ridge):", round(r_squared, 4)))






