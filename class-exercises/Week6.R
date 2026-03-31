library(dplyr)
library(ggplot2)
library(palmerpenguins)

penguinsClean <- penguins %>%
  select(species, sex, body_mass_g) %>%
  na.omit()

summary(penguinsClean)

alpha  <- 0.05

# sample variation and standard error

mean_mass <- mean(penguinsClean$body_mass_g)
sd_mass <- sd(penguinsClean$body_mass_g)
n <- nrow(penguinsClean)

se_mass <- sd_mass / sqrt(n)

mean_mass
se_mass

# SE measure how much our sample mean would vary from the population
# Larg sample size = smaller standerd error = more precision

# working out the confidence interval
#95% = good indicator that test is valid on given data set

ci_test <- t.test(penguinsClean$body_mass_g)
ci_test

# RQ =  is the mean body mass diffrent when at 4000g
#H0 = u = 4000g
#H1 = u ! = 4000g

one_sample_test <- t.test(penguinsClean$body_mass_g,
                          mu = 4000,
                          alternative = "two.sided"
                          )

one_sample_test

t_val <- as.numeric(one_sample_test$statistic)
df_val <- as.numeric(one_sample_test$parameter)

t_val
df_val

#look into t critical value

t_crit <- qt(1 - alpha/2, df = df_val)
t_crit

x_vals <- seq(-4,4, length = 1000)
y_vals <- dt(x_vals, df = df_val)

plot(x_vals, y_vals, type = "l",  # type one means type one sample
     main = "One Sample t-test Sample Distirbution",
     ylab = "Density", xlab = "t" ,
     lwd = 2)
#shade the rejected  regions
x_right <- seq (t_crit, max(x_vals), length.out = 1000)
y_right <- dt(x_right, df = df_val)

polygon(c(t_crit, x_right, max(x_right)),
        c(0, y_right, 0),
        col = "lightcoral")

x_left <- seq(min(x_vals), -t_crit, length.out = 1000)
y_left <- dt(x_left, df = df_val)


polygon(c(min(x_left), x_left, -t_crit),
        c(0, y_left, 0),
        col = "lightcoral")

abline(v = t_val, col = "blue", lwd = 3)

if (abs(t_val) > t_crit) {
  print("Reject the null hypothesis")
} else {
  print("Fail to reject the null hypothesis")
}

# independence t- test

#2 groups
#group 1 independent to group 2
#the sample size for both must be same
# continuous data = independent variable

#RQ: do males and females differ in boady mass

#group 1: male pengiunes
#group 2: female pengunes
#continuous data: body mass


independent_test <- t.test(
  body_mass_g ~ sex,
  data = penguinsClean
)

independent_test 

ind_t_val <- as.numeric(independent_test$statistic)
ind_df_val <- as.numeric(independent_test$parameter)

ind_t_val
ind_df_val

lower_diff <- independent_test$conf.int[1]
upper_diff <- independent_test$conf.int[2]
mean_diff <- diff(independent_test$estimate)

diff_dataframe <- data.frame(mean = mean_diff,
                             lower = lower_diff,
                             upper = upper_diff)

ggplot(diff_dataframe, aes(x="mean difference", y= mean))+
  geom_point(size =4)+
  geom_errorbar(aes(ymin = lower, ymax = upper),
               width =0.2)+
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "purple")+
  labs(title = "95% CI for Diffrence " )



#compute t critical value with out independece t test

t_crit2 <- qt (1- alpha/2, df = ind_df_val)

x_vals2 <- seq(-4, 4, length = 1000)
y_vals2 <- dt(x_vals2, df = ind_df_val)

plot(x_vals2, y_vals2, type = "l",
     main = "Independent Sampling Distribution",
     ylab = "Densisty", xlab = "t",
     lwd = 2)

x_right2 <- seq(t_crit2, max(x_vals2), length.out = 1000)
y_right2 <- dt(x_right2, df= ind_df_val)

polygon(c(t_crit2, x_right2, max(x_right2)),
        c(0, y_right2, 0),
        col = "lightcoral")

x_left2 <- seq(min(x_vals2), -t_crit2, length.out = 1000)
y_left2 <- dt(x_left2, df = ind_df_val)

polygon(c(min(x_left2), x_left2, -t_crit2),
        c(0, y_left2, 0),
        col = "lightcoral")

abline(v = ind_t_val, col = "blue", lwd = 3)

if (abs(ind_t_val) > t_crit2) {
  print("Reject the null hypothesis")
} else {
  print("Fail to reject the null hypothesis")
}

