library(dplyr)
library(ggplot2)
library(psych)

data("mtcars")

mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
mtcars$cyl <- factor(mtcars$cyl)

head(mtcars)

#filter manual cars

manual_cars <- mtcars %>%
  filter(am == "Manual")
head (manual_cars)

# cyl = 6
cylinder_six <- mtcars %>%
  filter(cyl == 6)
head (cylinder_six)

# Filter cars with mpg greater than 25

mpg <- mtcars %>%
  filter(mpg > 25)
head (mpg)

#Produce a frequency table of transmission type.
table(mtcars$am)

#Produce a frequency table of cylinders.
table(mtcars$cyl)

#Create a cross-tabulation of cylinders by transmission.
table(mtcars$cyl, mtcars$am)

#Mean
mean(mtcars$mpg, na.rm = TRUE)

#Median
median(mtcars$mpg, na.rm = TRUE)

#Standard Deviation
sd(mtcars$mpg, na.rm = TRUE)

#Interquartile Range (IQR)
IQR(mtcars$mpg, na.rm = TRUE)

# Calculate mean mpg by transmission type.
mtcars %>%
  group_by(am) %>%
  summarise(
    mean_am = mean(mpg, na.rm = TRUE),
    n = n()
  )

# Calculate mean horsepower by number of cylinders.
mtcars %>%
  group_by(cyl) %>%
  summarise(
    mean_hp = mean(hp, na.rm = TRUE),
    n = n()
  )

#Histogram of mpg
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "steelblue", colour = "black") +
  labs(title = "Histogram of MPG", x = "Miles per gallon", y = "Count")

#Boxplot of mpg by transmission
ggplot(mtcars, aes(x = am, y = mpg)) +
  geom_boxplot(fill = "seagreen", colour = "black") +
  labs(title = "MPG by Transmission", x = "Transmission", y = "Miles per gallon")

#Scatterplot of weight (wt) vs mpg
ggplot(mtcars, aes(x = wt, y = mpg, colour = am)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Weight vs MPG", x = "Weight", y = "Miles per gallon")



