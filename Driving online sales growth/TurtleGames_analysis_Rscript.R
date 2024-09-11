# Turtle Games case study
# Prepared by Lilliana Golob

# Determine the working directory and set accordingly if necessary
getwd()


# Import libraries
library(tidyverse)
library(skimr)
library(moments)
library(corrplot)
library(plotly)
library(GGally)

# Import colour package
library(ggokabeito)

# Import data (turtle_reviews_clean_2.csv where education column updated)
turtle <- read.csv('turtle_reviews_clean_2.csv', header = TRUE)

# Sense check data
as_tibble(turtle)

# Remove unnecessary columns (review, summary, and bins)
turtle <- select(turtle, -review, -summary, 
                 -age_bin, -income_bin, -spend_score_bin, -loyalty_points_bin)
as_tibble(turtle)

# Rename columns
turtle <- turtle %>%
  rename(income = income_.k)

# Check renamed
names(turtle)


##### EXPLORATORY ANALYSIS #####


# Summary statistics
skim(turtle)

# Interquartile range
IQR(turtle$loyalty_points)

# Shapiro Wilk test of normal distribution
shapiro.test(turtle$loyalty_points)

# Skewness
skewness(turtle$loyalty_points)

# Kurtosis
kurtosis(turtle$loyalty_points)

# QQ plot
qqnorm(turtle$loyalty_points, col = 'orange', main = 'QQ plot to check for normal distribution')
qqline(turtle$loyalty_points, col = 'blue', lwd = 1.5)

# DEMOGRAPHICS

# Number of products
n_distinct(turtle$product)

# Number of customers by gender
table(turtle$gender)

# Number of customers education
table(turtle$education)

# Number of customers by gender and education
table(turtle$gender, turtle$education)


##### VISUALISATIONS #####


# BOXPLOT

# Loyalty points by gender
ggplot(turtle, aes(x = as.factor(gender), y = loyalty_points)) +
  geom_boxplot(colour = 'blue', alpha = 0.5) + 
  labs(title = 'Loyalty points by gender', x = '', y = 'Loyalty points') +
  scale_y_continuous(n.breaks = 12) +
  theme_classic()


# HISTOGRAMS

# Distribution of loyalty points (bin width unspecified)
ggplot(turtle, aes(x = loyalty_points)) + 
  geom_histogram(fill = 'blue', alpha = 0.7) + 
  labs(title = 'Distribution of loyalty points', x = 'Loyalty points', y = 'Frequency') +
  scale_x_continuous(n.breaks = 8) +
  theme_classic()

# Turn off scientific notation
options(scipen = 999)

# Distribution of loyalty points
# Smoothed density plot
ggplot(turtle, aes(x = loyalty_points)) + 
  geom_density(colour = 'blue', size = 1) + 
  labs(title = 'Distribution of loyalty points (alternative view)',  x = 'Loyalty points', y = 'Density') +
  scale_x_continuous(n.breaks = 12) +
  theme_classic()

# Distribution of loyalty points by gender
# Smoothed density plot
ggplot(turtle, aes(loyalty_points, colour = gender)) + 
  geom_density(linewidth = 1) + 
  labs(title = 'Distribution of loyalty points by gender',  x = 'Loyalty points', y = 'Density') +
  scale_x_continuous(n.breaks = 12) +
  theme_classic()

# Distribution of loyalty points by education
# Smoothed density plot
ggplot(turtle, aes(loyalty_points, colour = education)) + 
  geom_density(linewidth = 1) + 
  labs(title = 'Distribution of loyalty points by education',  x = 'Loyalty points', y = 'Density') +
  scale_x_continuous(n.breaks = 12) +
  scale_color_okabe_ito() +
  theme_classic()


# SCATTERPLOTS: AGE

# Relationship between loyalty points and age (by gender)
# With linear line of best fit
ggplot(turtle, aes(x = age, y = loyalty_points, col = gender)) +
  geom_point(alpha = 0.5) + 
  labs(title = 'Relationship between loyalty points and age (by gender)',  x = 'Age', y = 'Loyalty points') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 6) +
  theme_classic() + 
  geom_smooth(method = 'lm', se = FALSE, size = 1.5)

# Relationship between loyalty points and age (by gender)
# With line of best fit not specified
ggplot(turtle, aes(x = age, y = loyalty_points, col = gender)) +
  geom_point(alpha = 0.5) + 
  labs(title = 'Relationship between loyalty points and age (by gender)',  x = 'Age', y = 'Loyalty points') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 6) +
  theme_classic() + 
  geom_smooth(se = FALSE, size = 1.5)


# SCATTERPLOTS: INCOME

# Relationship between loyalty points and income (by gender)
# With linear line of best fit
ggplot(turtle, aes(x = income, y = loyalty_points, col = gender)) +
  geom_point(alpha = 0.5) + 
  labs(title = 'Relationship between loyalty points and income (by gender)',  x = 'Income £k', y = 'Loyalty points') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 6) +
  theme_classic() + 
  geom_smooth(method = 'lm', se = FALSE, size = 1.5)

# Relationship between loyalty points and income (by gender)
# With line of best fit not specified
ggplot(turtle, aes(x = income, y = loyalty_points, col = gender)) +
  geom_point(alpha = 0.5) + 
  labs(title = 'Relationship between loyalty points and income (by gender)',  x = 'Income £k', y = 'Loyalty points') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 6) +
  theme_classic() + 
  geom_smooth(se = FALSE, size = 1.5)

# Relationship between loyalty points and income (by gender)
# Smooth lines
ggplot(turtle, aes(x = income, y = loyalty_points, col = gender)) +
  labs(title = 'Relationship between loyalty points and income (by gender)',
       subtitle = 'Alternative view') +
  scale_x_continuous(n.breaks = 10, 'Income £k') +
  scale_y_continuous(n.breaks = 10, 'Loyalty points') +
  theme_classic() + 
  geom_smooth(se = FALSE, size = 1.5)


# SCATTERPLOTS: SPEND SCORE

# Relationship between loyalty points and spend score (by gender)
# With linear line of best fit
ggplot(turtle, aes(x = spend_score, y = loyalty_points, col = gender)) +
  geom_point(alpha = 0.5) + 
  labs(title = 'Relationship between loyalty points and spend score (by gender)',  x = 'Spend score', y = 'Loyalty points') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 6) +
  theme_classic() + 
  geom_smooth(method = 'lm', se = FALSE, size = 1.5)

# Relationship between loyalty points and spend score (by gender)
# With line of best fit not specified
ggplot(turtle, aes(x = spend_score, y = loyalty_points, col = gender)) +
  geom_point(alpha = 0.5) + 
  labs(title = 'Relationship between loyalty points and spend score (by gender)',  x = 'Spend score', y = 'Loyalty points') +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 6) +
  theme_classic() + 
  geom_smooth(se = FALSE, size = 1.5)

# Relationship between loyalty points and spend score (by gender)
# Smooth lines
ggplot(turtle, aes(x = spend_score, y = loyalty_points, col = gender)) +
  labs(title = 'Relationship between loyalty points and spend score (by gender)',
       subtitle = 'Alternative view') +
  scale_x_continuous(n.breaks = 10, 'Spend score') +
  scale_y_continuous(n.breaks = 10, 'Loyalty points') +
  theme_classic() + 
  geom_smooth(se = FALSE, size = 1.5)

# Relationship between loyalty points and spend score (by gender)
# Facet by education
# With linear line of best fit method
ggplot(turtle, aes(x = spend_score, y = loyalty_points, col = gender)) +
  geom_point(alpha = 0.5) + 
  labs(title = 'Relationship between loyalty points and spend score',
       subtitle = 'By gender and education') +
  scale_x_continuous(n.breaks = 10, 'Spend score') +
  scale_y_continuous(n.breaks = 6, 'Loyalty points') +
  theme_classic() + 
  facet_wrap(~education) + 
  geom_smooth(method = 'lm', se = FALSE, size = 1)


# SCATTERPLOTS: PRODUCT CODE

# Relationship between loyalty points and product
# With unspecified line of best fit method
ggplot(turtle, aes(x = product, y = loyalty_points)) +
  geom_point(colour = 'orange', alpha = 0.5) + 
  labs(title = 'Relationship between loyalty points and product code') +
  scale_x_continuous(n.breaks = 10, 'Product code') +
  scale_y_continuous(n.breaks = 6, 'Loyalty points') +
  theme_classic() + 
  geom_smooth(se = FALSE, size = 1.5)


# CORRELATION PLOTS

# Select numeric columns only
numeric_data <- turtle %>%
  select_if(is.numeric)

# Correlation
round(cor(numeric_data), digits = 2)

# Correlation matrix on numeric columns
correlation_matrix <- cor(numeric_data)

# Plot matrix
corrplot(correlation_matrix, method = 'circle')
corrplot(correlation_matrix, method = 'number')

# Pairs plots
ggpairs(turtle[, c('loyalty_points', 'income', 'spend_score', 'age', 'product')],
        aes(color = turtle$gender, alpha = 0.7)) + ggtitle("Relationship between loyalty points and other variables")




##### REMOVE OUTLIERS #####

# Remove loyalty points greater than 3000, create new df
turtle_new <- filter(turtle, loyalty_points < 3000)
dim(turtle_new)
dim(turtle)

# QQ plot
# loyalty points less than 3000
qqnorm(turtle_new$loyalty_points, col = 'orange', main = 'Updated QQ plot: removed outliers (loyalty points less than 3000)')
qqline(turtle_new$loyalty_points, col = 'blue', lwd = 1.5)

# Pairs plots
# loyalty points less than 3000
ggpairs(turtle_new[, c('loyalty_points', 'income', 'spend_score', 'age', 'product')],
        aes(color = turtle_new$gender, alpha = 0.7)) + ggtitle("Updated: removed outliers (loyalty points less than 3000)")



##### MULTIPLE LINEAR REGRESSION #####

# Independent variables: income and spend score
# Fit linear regression model
model <- lm(loyalty_points ~ income + spend_score, data = turtle)

# View full regression table
summary(model)

# Plot prediction (smooth method = lm)
ggplot(turtle, aes(x = loyalty_points, y = predict(model, turtle))) + 
  geom_point(col = 'orange', alpha = 0.7, size = 1.5) + 
  stat_smooth(method = 'lm') + 
  labs(x = 'Actual loyalty points', y = 'Predicted loyalty points') + 
  ggtitle('Actual vs predicted loyalty points (linear smoothing line)') +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8) +
  theme_classic()

# Plot prediction (smooth method = gam)
ggplot(turtle, aes(x = loyalty_points, y = predict(model, turtle))) + 
  geom_point(col = 'orange', alpha = 0.7, size = 1.5) + 
  stat_smooth(method = 'gam') + 
  labs(x = 'Actual loyalty points', y = 'Predicted loyalty points') + 
  ggtitle('Actual vs predicted loyalty points (with gam smoothing line)') +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8) +
  theme_classic()


# LOG TRANSFORMATION

# Create new variable logloyalty
turtle <- mutate(turtle, logloyalty = log(loyalty_points))
head(turtle)

# Create new model using logloyalty
model2 <- lm(logloyalty ~ income + spend_score, data = turtle)

# View full regression table
# Model2
summary(model2)

# QQ plot
qqnorm(turtle$logloyalty, col = 'orange', main = 'QQ plot for log transformation')
qqline(turtle$logloyalty, col = 'blue', lwd = 1.5)

# Plot prediction (smooth method = lm)
# Model2
ggplot(turtle, aes(x = loyalty_points, y = predict(model2, turtle))) + 
  geom_point(col = 'orange', alpha = 0.7, size = 1.5) + 
  stat_smooth(method = 'lm') + 
  labs(x = 'Actual loyalty points', y = 'Predicted loyalty points') + 
  ggtitle('Actual vs predicted loyalty points model 2 (log transformation)') +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8) +
  theme_classic()

# Plot prediction (smooth method = gam)
# Model2
ggplot(turtle, aes(x = loyalty_points, y = predict(model2, turtle))) + 
  geom_point(col = 'orange', alpha = 0.7, size = 1.5) + 
  stat_smooth(method = 'gam') + 
  labs(x = 'Actual loyalty points', y = 'Predicted loyalty points') + 
  ggtitle('Actual vs predicted loyalty points model 2 (log transformation)') +
  scale_x_continuous(n.breaks = 8) +
  scale_y_continuous(n.breaks = 8) +
  theme_classic()



# PREDICTIONS

# Predict loyalty points and compare impact of spend score on income
# Based on most customers earn between £20k - £80k, with a mean of £48k
# Use original model

# Customer 1: Income = 30, Spend score = 25
# Customer 2: Income = 30, Spend score = 50
# Customer 3: Income = 30, Spend score = 75
# Customer 4: Income = 30, Spend score = 100
# Customer 5: Income = 50, Spend score = 25
# Customer 6: Income = 50, Spend score = 50
# Customer 7: Income = 50, Spend score = 75
# Customer 8: Income = 50, Spend score = 100
# Customer 9: Income = 70, Spend score = 25
# Customer 10: Income = 70, Spend score = 50
# Customer 11: Income = 70, Spend score = 75
# Customer 12: Income = 70, Spend score = 100

data = data.frame(income = c(30, 30, 30, 30, 50, 50, 50, 50, 70, 70, 70, 70), 
                  spend_score = c(25, 50, 75, 100, 25, 50, 75, 100,25, 50, 75, 100))
predictions <-predict(model, data)
round(predictions, digits = 0)
