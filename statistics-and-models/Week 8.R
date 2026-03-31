#week 8 
library(ggplot2) 
library(dplyr)
advertising <- read.csv("Advertising.csv")
summary(advertising)
#1.
# linear regression is a method to predict a continuous variable (y) based on one or more predictor variables (x).
# The goal of linear regression is to find the best-fitting line (or hyperplane in higher dimensions) that describes the relationship between the predictor(s) and the response variable.
# check if the variables are fit fro linear regressionn 
#1.1 repeat process for multiple columns 
ggplot(advertising, aes(x = Radio, y = Sales)) + 
  geom_point(size = 3, color = "blue") + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal () +
  labs(title = "Radio Advertising vs Sales", x = "Radio Advertising Budget", y = "Sales")

ggplot(advertising, aes(x = Newspaper, y = Sales)) + 
  geom_point(size = 3, color = "blue") + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal () +
  labs(title = "Newspaper Advertising vs Sales", x = "Newspaper Advertising Budget", y = "Sales")

# this is to take a look to see the correlation beetwenn variables 
cor(advertising[,c("TV", "Sales", "Radio","Newspaper")]) # what this does 

#2. simple regression plotting 
ggplot(advertising, aes(x = TV, y = Sales)) + 
  geom_point(size = 3, color = "blue") + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal () +
  labs(title = "TV Advertising vs Sales", x = "TV Advertising Budget", y = "Sales")
#2.1 extract predited values and residuals
model <- lm(Sales ~ TV, data = advertising) # this creates a linear regression model where Sales is the response variable and TV is the predictor variable, using the data from the advertising data frame. The lm() function fits a linear model to the data and returns an object that contains information about the coefficients, residuals, and other statistics related to the model.
advertising$predicted <- predict(model) # this adds a new column called "predicted" to the advertising data frame, which contains the predicted sales values based on the TV advertising budget using the linear regression model we just created. The predict() function takes the model object as input and generates predicted values for each observation in the data frame.
advertising$residuals <- residuals(model) # difference between the actual sales values and the predicted sales values for each observation. The residuals() function takes the model object as input and returns the residuals for each observation in the data frame.
# what this does is to add two new columns to the advertising data frame: "predicted" which contains the predicted sales values based on the TV advertising budget, and "residuals" which contains the differences between the actual sales and the predicted sales. This allows us to analyze how well the linear regression model fits the data and identify any patterns in the residuals that may indicate issues with the model.
#higheer residuals means that the model is not fitting the data well, while lower residuals indicate a better fit. By examining the predicted values and residuals, we can assess the accuracy of the linear regression model and identify any potential outliers or influential points that may be affecting the results.

#2.2 plot residuals
ggplot(advertising, aes(x = predicted, y = residuals)) + 
  geom_point(size = 3, color = "red") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "#ff00ee") + # this adds a horizontal dashed line at y = 0, which represents the ideal case where the predicted values perfectly match the actual values (i.e., no residuals). The linetype argument specifies that the line should be dashed, and the color argument sets the color of the line to a bright pink (#ff00ee).  
  theme_minimal () +
  labs(title = "Residuals vs Predicted Values", x = "Predicted Sales", y = "Residuals")

#3.qqplot its used to check if the residuals are normally distributed why is it important to check if the residuals are normally distributed?
# checking if the residuals are normally distributed is important because many statistical tests and confidence intervals in  linear regression rely on the assumption of normality. If the residuals are not normally distributed, it can indicate that the model may not be a good fit for the data, and the results of the regression analysis may be unreliable. Additionally, non-normal residuals can suggest the presence of outliers or other issues with the data that may need to be addressed before drawing conclusions from the model. By using a QQ plot to visualize the distribution of residuals, we can assess whether they follow a normal distribution and make informed decisions about the validity of our regression analysis.
qqnorm(advertising$residuals, main = "QQ Plot of Residuals") # this creates a QQ plot of the residuals from the linear regression model. The qqnorm() function takes the residuals as input and generates a plot that compares the distribution of the residuals to a normal distribution. If the points in the QQ plot approximately follow a straight line, it suggests that the residuals are normally distributed. Deviations from the straight line may indicate that the residuals are not normally distributed, which could affect the validity of the regression analysis.
qqline(advertising$residuals, col = "red") # this adds a reference  line to the QQ plot created by the qqnorm() function. The qqline() function takes the same residuals as input and adds a line to the plot that represents the expected values if the residuals were perfectly normally distributed. The col argument specifies that the line should be colored red. By comparing the points in the QQ plot to this reference line, we can visually assess how well the residuals follow a normal distribution. If the points closely follow the red line, it suggests that the residuals are approximately normally distributed.    

#calculating error rate  of a linear regression model
# the error rate of a linear regression model can be calculated using various metrics, such as Mean Absolute Error (MAE), Mean Squared Error (MSE), or Root Mean Squared Error (RMSE). Here’s how you can calculate these metrics in R:
#rmse is the most commonly used metric for evaluating the performance of a linear regression model, as it gives more weight to larger errors and is in the same units as the response variable. However, the choice of metric may depend on the specific context of your analysis and the importance of different types of errors in your application.
rmse_tv <- sqrt(mean(advertising$residuals^2))
mae_tv <- mean(abs(advertising$residuals))
mse_tv <- mean(advertising$residuals^2)     
#cat is used to concatenate and print the results in a readable format. The "\n" at the end of each line adds a newline character, ensuring that each metric is printed on a separate line for better readability.

#5. multiple linear regression 
#what difference between simple and multiple linear regression?
# simple linear regression involves one predictor variable and one response variable, while multiple linear regression involves two or more predictor variables.
#so that means that in multiple linear regression, we can assess the impact of multiple factors on the response variable simultaneously, which can provide a more comprehensive understanding of the relationships between variables and improve the accuracy of predictions compared to simple linear regression.
model_multiple <- lm(Sales ~ TV + Radio + Newspaper, data = advertising) # this creates a multiple linear regression model where Sales is the response variable and TV, Radio, and Newspaper are the predictor variables, using the data from the advertising data frame. The lm() function fits a linear model to the data and returns an object that contains information about the coefficients, residuals, and other statistics related to the model.
summary(model_multiple)$adj.r.squared # this provides the R-squared value of the multiple linear regression model we just created.  
#adj R-squared is a modified version of R-squared that takes into account the number of predictor variables in the model. It adjusts the R-squared value based on the number of predictors, providing a more accurate measure of how well the model fits the data, especially when comparing models with different numbers of predictors. A higher adjusted R-squared value indicates a better fit of the model to the data, while a lower value suggests that the model may not be capturing the underlying relationships effectively.
# --------------------------------------------------------------------------------------------------------------------
#question: does having more variables improve the model's performance?
# adding more variables to a linear regression model can potentially improve its performance by capturing more of the variability in the response variable. However, it is not guaranteed that adding more variables will always lead to a better model. In some cases, adding irrelevant or highly correlated variables can lead to overfitting, where the model becomes too complex and performs well on the training data but poorly on new, unseen data. It is important to carefully select predictor variables based on their relevance and contribution to the model, and to evaluate the model's performance using appropriate metrics and validation techniques to ensure that it generalizes well to new data.    
# in the context of the advertising dataset, adding more variables such as Radio and Newspaper advertising budgets may improve the model's performance in predicting Sales, as they may capture additional factors that influence sales. However, it is essential to evaluate the model's performance using metrics like adjusted R-squared, RMSE, or cross-validation to determine if the added variables actually enhance the model's predictive power without leading to overfitting.
# multiple linear regression plot:
advertising$predicted_multiple <- predict(model_multiple)
advertising$residuals_multiple <- residuals(model_multiple)

ggplot(advertising, aes(x = predicted_multiple, y = Sales)) + 
  geom_point(size = 3, color = "blue") + 
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  theme_minimal () +
  labs(title = "Actual vs Predicted Sales", x = "Predicted Sales", y = "Actual Sales") +
    theme(plot.title = element_text(hjust = 0.5))

summary(model_multiple)$adj.r.squared
#residual plotting for ulti linear regression 
ggplot(advertising, aes(x = predicted_multiple, y = residuals_multiple)) +          
    geom_point(size = 3, color = "red") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "#ff00ee") + 
    theme_minimal () +
    labs(title = "Residuals vs Predicted Values (Multiple Linear Regression)", x = "Predicted Sales", y = "Residuals") +
        theme(plot.title = element_text(hjust = 0.5))
        # the residual plot for the multiple linear regression model shows the relationship between the predicted sales values and the residuals (the differences between actual and predicted sales). In this plot, we look for patterns in the residuals to assess the fit of the model. Ideally, the residuals should be randomly scattered around the horizontal line at y = 0, indicating that the model's predictions are unbiased and that there is no systematic error. If we observe any patterns, such as a funnel shape or a clear trend, it may suggest issues with the model, such as heteroscedasticity or non-linearity, which could indicate that the model may not be capturing all relevant factors or that there may be outliers affecting the results.
#cofidence intervall and prediction interval
# confidence interval provides a range of values within which we can be confident that the true population parameter    
# lies, while a prediction interval provides a range of values within which we can expect a new observation to fall, given the predictor variables. In the context of linear regression, confidence intervals are used to estimate the range of possible values for the regression coefficients, while prediction intervals are used to estimate the range of possible values for a new response variable based on specific predictor values. Both intervals are important for understanding the uncertainty associated with our estimates and predictions in linear regression analysis.
# confidence interval for the coefficient of TV advertising budget
predict(model_multiple, newdata = data.frame(TV = 150, Radio = 20, Newspaper = 10), interval = "confidence") # this generates a confidence interval for the predicted sales value based on the specified values of TV, Radio, and Newspaper advertising budgets using the multiple linear regression model. The predict() function takes the model object as input, along with a new data frame containing the predictor values (TV = 150, Radio = 20, Newspaper = 10) for which we want to make a prediction. The interval argument specifies that we want to calculate a confidence interval for the predicted value. The output will include the predicted sales value along with the lower and upper bounds of the confidence interval, providing a range within which we can be confident that the true mean sales value lies for those specific predictor values.
# prediction interval for a new observation with the same predictor values
predict(model_multiple, newdata = data.frame(TV = 150, Radio = 20, Newspaper = 10), interval = "prediction") # this generates a prediction interval for a new observation based on the specified values of TV, Radio, and Newspaper advertising budgets using the multiple linear regression model. The predict() function takes the model object as input, along with a new data frame containing the predictor values (TV = 150, Radio = 20, Newspaper = 10) for which we want to make a prediction. The interval argument specifies that we want to calculate a prediction interval for the predicted value. The output will include the predicted sales value along with the lower and upper bounds of the prediction interval, providing a range within which we can expect a new observation to fall for those specific predictor values.

#diagnostic plots for multiple linear regression is used to assess the assumptions of the model, such as linearity, homoscedasticity, normality of residuals, and the presence of influential points. Common diagnostic plots include:
#1. Residuals vs Fitted Values Plot: This plot helps to check for linearity and homoscedasticity. We look for a random scatter of residuals around the horizontal line at y = 0, which indicates that the assumptions of linearity and constant variance are met.
#2. Normal Q-Q Plot: This plot assesses the normality of residuals. If the points in the Q-Q plot approximately follow a straight line, it suggests that the residuals are normally distributed. Deviations from the straight line may indicate non-normality.
par(mfrow = c(2, 2)) # this sets up the plotting area to display four diagnostic plots in a 2x2 grid. The par() function is used to specify graphical parameters, and the mfrow argument indicates that we want to arrange the plots in 2 rows and 2 columns. This allows us to visualize multiple diagnostic plots for the multiple linear regression model simultaneously, making it easier to assess the assumptions of the model and identify any potential issues with the fit.
plot(model_multiple) # this generates a series of diagnostic plots for the multiple linear regression model we created. The plot() function takes the model object as input and produces several plots that help to assess the assumptions of the linear regression model, such as residuals vs fitted values, normal Q-Q plot, scale-location plot, and residuals vs leverage plot. These plots are essential for diagnosing potential issues with the model, such as non-linearity, heteroscedasticity, non-normality of residuals, and influential points that may affect the validity of the regression analysis.
par(mfrow = c(1, 1)) # this resets the plotting area to its default state, which is a single plot. After generating the diagnostic plots for the multiple linear regression model, we use par(mfrow = c(1, 1)) to return the plotting area to its original configuration, allowing us to create new plots without the 2x2 grid layout. This is important for maintaining a clean and organized plotting environment for subsequent visualizations.
#last plot of the week 8 is 
##pairs plot 
pairs(advertising[, c("TV", "Radio", "Newspaper", "Sales")], main = "Pairs Plot of Advertising Data") # this creates a pairs plot (also known as a scatterplot matrix) for the specified columns in the advertising data frame. The pairs() function takes a subset of the data frame containing the columns "TV", "Radio", "Newspaper", and "Sales" and generates a matrix of scatterplots that show the relationships between each pair of variables. The main argument adds a title to the plot. This visualization allows us to quickly assess the correlations and potential linear relationships between the different advertising budgets and sales, helping us to identify patterns and insights in the data.       
