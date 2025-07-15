
library(Sleuth3)
data("ex0525")

# ANOVA to test if mean incomes differ among education levels
anova_result <- aov(Income2005 ~ factor(Educ), data = ex0525)
summary(anova_result)

# Multiple comparison using Tukey's HSD test
TukeyHSD(anova_result)


# Visually analyze the data with a boxplot
boxplot(Income2005 ~ factor(Educ), data = ex0525,
        main = "Income vs. Education (Transformed)",
        ylab = "2005 Income", xlab = "Education Years",
        col = 2:8)

#Based on skewness and spread, we perform a log transformation if appropriate
ex0525$Income2005_log <- log(ex0525$Income2005)

# Boxplot to visualize the transformed data
boxplot(Income2005_log ~ factor(Educ), data = ex0525,
        main = "Log-transformed Income vs. Education",
        ylab = "Log of 2005 Income", xlab = "Education Years",
        col = 2:8)

# Calculate median incomes for each education level
median_incomes <- tapply(ex0525$Income2005, ex0525$Educ, median)
median_incomes

# Calculate differences between consecutive education levels
median_diffs <- diff(median_incomes)
median_diffs

# If you need to calculate percentage differences
median_perc_diffs <- median_diffs / median_incomes[1:length(median_incomes)-1] * 100
median_perc_diffs


# Assign average years of education to each group
ex0525$AvgYears <- factor(ex0525$Educ, labels = c(10, 12, 14, 16, 20))

# Linear model to test for linear trend in income as a function of the number of years studied
linear_trend_model <- lm(Income2005 ~ as.numeric(AvgYears), data = ex0525)
summary(linear_trend_model)
