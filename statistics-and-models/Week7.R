data("mtcars")

library(ggplot2)
library(dplyr)
library(car)
library(reshape2)

mtcars$cyl <- factor(mtcars$cyl)
alpha <- 0.05
# Anova test
# Concerned with variance between groups and within groups
# To assess how much variability is there between groups and within

# Variance
# H0: all groups are equal to number
# H1: at least one group is different from the rest
# ANOVA test is a parametric test and assumes that the data is normally
# distributed and the variances are equal across groups
# If the p value is less than the alpha level we reject the null
# hypothesis and conclude that there is a significant difference
# If the p value is greater than alpha we fail to reject null
# hypothesis and conclude there is no significant difference
# ANOVA test is used to compare the means of three or more groups
# If we have only two groups we can use a t test to compare means
# If we have more than two groups we can use anova test to compare
# means of the groups.
anova_model <- aov(mpg ~ cyl, data = mtcars)
summary(anova_model)

# ANOVA table - extraction from model summary to plot F distribution
# and find the critical value for the F test
anova_table <- summary(anova_model)[[1]]
f_stat <- anova_table["cyl", "F value"]
df1 <- anova_table["cyl", "Df"]
df2 <- anova_table["Residuals", "Df"]

# Find critical value for upper tail of F distribution
f_crit <- qf(1 - alpha, df1, df2)
# Plotting the F distribution and critical value for the F test
x_max <- max(f_stat, f_crit) + 1
# Setting x axis limit to be slightly larger than max value
x_vals <- seq(0, x_max, length.out = 1000)
# Creating a sequence of x values from 0 to the maximum x value

y_vals <- df(x_vals, df1, df2)
plot(x_vals, y_vals, type = "l", main = "F Distribution",
     xlab = "F statistic", ylab = "Density")

# Shade the rejection region for the F test
x_shade <- seq(f_crit, x_max, length.out = 1000)
y_shade <- df(x_shade, df1, df2)
polygon(c(f_crit, x_shade, x_max),
        c(0, y_shade, 0), col = "green", border = NA)
# Critical value line
abline(v = f_crit, col = "blue", lwd = 2)
# F statistic line
abline(v = f_stat, col = "red", lwd = 2)

# F-statistic is the ratio of variance between groups to variance
# within groups. The larger the F statistic the more likely it is
# that there is a significant difference between groups

# Save the plot as a png file
png("F_distribution.png", width = 800, height = 600)
plot(x_vals, y_vals, type = "l", main = "F Distribution",
     xlab = "F statistic", ylab = "Density")
polygon(c(f_crit, x_shade, x_max),
        c(0, y_shade, 0), col = "green", border = NA)
abline(v = f_crit, col = "blue", lwd = 2)
abline(v = f_stat, col = "red", lwd = 2)
dev.off()

shapiro.test(residuals(anova_model))
# Test for normality of the data which is an assumption of ANOVA

leveneTest(mpg ~ cyl, data = mtcars)
# Test for homogeneity of variances which is an assumption of ANOVA
# Assumptions of ANOVA are met if p value > alpha level
# We fail to reject the null hypothesis and conclude no significant
# difference between the groups
# ANOVA assumes normal distribution of the data and homogeneity of
# variances across groups. If these assumptions are violated, the
# results of ANOVA test may not be valid.
# Non-parametric test for ANOVA is the Kruskal-Wallis test which does
# not assume normal distribution of data and homogeneity of variances
# across groups. It is used to compare the medians of three or more
# groups.
kruskal.test(mpg ~ cyl, data = mtcars)





# Steps: how to know what test to use

# 1. Identify the research question and type of data
# (categorical or continuous)
# Tests for categorical data include chi square test for independence
# and chi square test for goodness of fit. Tests for continuous data
# include t test for comparing the means of two groups and ANOVA test
# for comparing the means of three or more groups. If the assumptions
# of the t test or ANOVA test are not met, we can use non-parametric
# tests such as the Wilcoxon rank sum test for comparing the medians
# of two groups and the Kruskal-Wallis test for comparing the medians
# of three or more groups.
# For categorical data, use chi square test to compare the observed
# frequencies of the categories to the expected frequencies under null
# hypothesis. It is used to test for independence between two
# categorical variables or to test for goodness of fit of a categorical
# variable to a theoretical distribution. The chi square test is
# non-parametric and does not assume normal distribution of the data or
# homogeneity of variances across groups. It is used when the data is
# categorical and the sample size is large enough to meet assumptions.
# For continuous data, use t test to compare the means of two groups.
# It is used when the data is continuous and the sample size is small.
# The t test assumes that the data is normally distributed and that the
# variances are equal across groups. If the assumptions of the t test
# are not met, we can use a non-parametric test such as the Wilcoxon
# rank sum test which does not assume normal distribution of the data
# or homogeneity of variances across groups. It is used to compare the
# medians of two groups when the data is not normally distributed or
# when the variances are not equal across groups.

# 2. Check the assumptions of the test you want to use
# (normality, homogeneity of variances, independence)
# Normality using these tests:
# Shapiro test for normality of the data
shapiro.test(residuals(anova_model))
# Test for normality of the data which is an assumption of ANOVA
# QQ plot for normality of the data
qqnorm(residuals(anova_model))
qqline(residuals(anova_model), col = "red")
# Homogeneity of variances using these tests:
# Levene test for homogeneity of variances across groups.
leveneTest(mpg ~ cyl, data = mtcars)
# Test for homogeneity of variances which is an assumption of ANOVA

# Independence using these tests:
# Durbin Watson test for independence of residuals in a regression
# model. It is used to test for autocorrelation in the residuals of a
# regression model. The Durbin Watson test statistic ranges from 0 to 4,
# with a value of 2 indicating no autocorrelation, values less than 2
# indicating positive autocorrelation, and values greater than 2
# indicating negative autocorrelation. If the p value is less than the
# alpha level we reject the null hypothesis and conclude that there is
# autocorrelation in the residuals. If the p value is greater than the
# alpha level we fail to reject the null hypothesis and conclude that
# there is no autocorrelation in the residuals.

# 3. If the assumptions are met, use the appropriate parametric test
# (t test, ANOVA)
# t test for comparing the means of two groups. It assumes normal
# distribution of the data and homogeneity of variances across groups.
# ANOVA test for comparing the means of three or more groups. It
# assumes normal distribution of the data and homogeneity of variances
# across groups.

# 4. If the assumptions are not met, use appropriate non-parametric
# test (Wilcoxon rank sum test, Kruskal-Wallis test, Tukey HSD test,
# Dunn test)

# Wilcoxon rank sum test is a non-parametric test used to compare the
# medians of two groups. It does not assume normal distribution of the
# data and homogeneity of variances across groups. It is used when the
# data is not normally distributed or when the variances are not equal
# across groups.

# Post hoc test is used to determine which groups are significantly
# different from each other after finding a significant result in an
# ANOVA test. It is used to compare the means of the groups pairwise
# and adjust for multiple comparisons. The most common post hoc test is
# the Tukey HSD test which stands for honestly significant difference
# test. It is used to compare the means of the groups pairwise and
# adjust for multiple comparisons. It is used when the assumptions of
# the ANOVA test are met. If the assumptions of the ANOVA test are not
# met, we can use a non-parametric post hoc test such as the Dunn test
# which is used to compare the medians of the groups pairwise and
# adjust for multiple comparisons.

# Tukey HSD test for post hoc analysis after finding a significant
# result in an ANOVA test. It is used to compare the means of the
# groups pairwise and adjust for multiple comparisons. It is used when
# the assumptions of the ANOVA test are met.
tukey_test <- TukeyHSD(anova_model)
plot(tukey_test)
print(tukey_test)

# Dunn test for post hoc analysis after finding a significant result in
# an ANOVA test when the assumptions of the ANOVA test are not met. It
# is used to compare the medians of the groups pairwise and adjust for
# multiple comparisons.

# 5. Interpret the results of the test and draw conclusions based on
# the research question and the data.
# ----------

# Effect Size for ANOVA Test
# Effect size for ANOVA test is the eta squared which is the
# proportion of the total variance in the dependent variable that is
# explained by the independent variable. It ranges from 0 to 1, with
# higher values indicating a larger effect size. The eta squared can be
# calculated using the ANOVA table as follows:
# Higher values of eta squared indicate a larger effect size, with
# values of 0.01, 0.06, and 0.14 often considered small, medium, and
# large effect sizes, respectively. This means that the independent
# variable explains 1%, 6%, and 14% of the variance in the dependent
# variable, respectively.
ss_between <- anova_table["cyl", "Sum Sq"]
ss_total <- sum(anova_table[, "Sum Sq"])
eta_squared <- ss_between / ss_total

# Correlation test
# Correlation and direction of linear relationship between two
# continuous variables. It ranges from -1 to 1, with values close to
# -1 indicating a strong negative correlation, values close to 1
# indicating a strong positive correlation, and values close to 0
# indicating no correlation. The correlation can be calculated using
# the cor function in R as follows:
cor(mtcars$mpg, mtcars$hp)
# Correlation between miles per gallon and horsepower in mtcars
corr_test <- cor.test(mtcars$mpg, mtcars$wt)
# Correlation test between miles per gallon and weight in mtcars
print(corr_test$p.value)
# p value for the correlation test
r <- 0.5
# correlation coefficient for the correlation test
n <- nrow(mtcars)
# sample size for the correlation test

# r=0 means no correlation between the two variables
# r=1 means a perfect positive correlation between the two variables
# r=-1 means a perfect negative correlation between the two variables
# The closer the r value is to 1 or -1, the stronger the correlation
# The p value for the correlation test indicates whether the
# correlation is statistically significant or not. If the p value is
# less than the alpha level we reject the null hypothesis and conclude
# that there is a significant correlation between the two variables. If
# the p value is greater than the alpha level we fail to reject the
# null hypothesis and conclude that there is no significant correlation
# between the two variables.
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Scatter Plot of Weight vs. Miles Per Gallon",
       x = "Weight",
       y = "Miles Per Gallon") +
  theme_minimal()

#heatmap of correlation matrix
numeric_data <- mtcars %>% select(where(is.numeric))
corr_matrix <- cor(numeric_data)
  
cor_melt <- melt(corr_matrix)

cor_melt$Var1 <- factor(cor_melt$Var1, levels = colnames(corr_matrix))
cor_melt$Var2 <- factor(cor_melt$Var2, levels = colnames(corr_matrix))

ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0) +
  labs(title = "Correlation Matrix Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
