
# Finance
# open finans1_data.csv 
setwd("~/Desktop/Finance_Project/finans1")
library(tidyverse)

# Sourced_Files  ------------------
source("/Volumes/LaCie\ 1/Technical\ University\ of\ Denmark/1.\ Introduction\ to\ Statistics/1.\ A\ Introduction/11.\ Script/Functions1.R")

# Intro  ------------------
D <- read.table("finans1_data.csv", header = TRUE, sep = ";", as.is = TRUE) 

# Only keep the date and AGG, VAW, IWN, and SPY (4) columns. 
weekly_returns <- D[ ,c("t","AGG","VAW","IWN","SPY")]
weekly_returns

# Turn the column into dates.
weekly_returns$t <- as.Date(weekly_returns$t, format = '%Y-%m-%d')


# Question a  ------------------
summary(weekly_returns)
perform_descriptive_analysis(weekly_returns)


  
summary(weekly_returns$t)
are_there_NAs(weekly_returns)


# Question b ------------------

hist(weekly_returns$AGG,
     prob = TRUE, 
     main = "Histogram of weekly returns from\n The Exchange Trade Fund: AGG",
     family = "Avenir",
     xlab = "weekly returns",
     breaks = 10,
     col = viridisLite::cividis(7, alpha = 0.8))

lines(density(weekly_returns$AGG), # density plot
      lwd = 3, # thickness of line
      col = "chocolate")

calculate_coefficient_of_variation(weekly_returns$AGG) # ans 22.48 or 2248.6 %

# Question c ------------------

library(tidyverse)

a<- ggplot(data = weekly_returns, 
       mapping = aes(x = t, y = AGG)) +
      geom_point(size = 1, col = "steelblue2", alpha = 0.9, pch = 20)+
  geom_smooth(col = "mediumaquamarine")+
        theme_classic()+
  labs(
    subtitle = "Exchange Trading Fund AGG",
       x = "Time",
       y = "Weekly Returns")


b<- ggplot(data = weekly_returns, 
       mapping = aes(x = t, y = VAW)) +
  geom_point(size = 1, col = "cadetblue", alpha = 0.9, pch = 20)+
  geom_smooth(col = "mediumaquamarine")+
  theme_classic()+
  labs(
       subtitle = "Exchange Trading Fund VAW",
       x = "Time",
       y = "Weekly Returns")

c<- ggplot(data = weekly_returns, 
       mapping = aes(x = t, y = IWN)) +
  geom_point(size = 1, col = "chartreuse4", alpha = 0.9, pch = 20)+
  geom_smooth(col = "mediumaquamarine")+
  theme_classic()+
  labs(
       subtitle = "Exchange Trading Fund IWN",
       x = "Time",
       y = "Weekly Returns")

d<- ggplot(data = weekly_returns, 
       mapping = aes(x = t, y = SPY)) +
  geom_point(size = 1, col = "cyan4", alpha = 0.9, pch = 20)+
  geom_smooth(col = "mediumaquamarine")+
  theme_classic()+
  labs(
       subtitle = "Exchange Trading Fund SPY",
       x = "Time",
       y = "Weekly Returns")
       

library(patchwork)
a+b+c+d


# Question d ------------------
boxplot(weekly_returns$AGG, weekly_returns$VAW, weekly_returns$IWN, weekly_returns$SPY,
         names = c("AGG", "VAW", "IWN", "SPY"),
        col = viridisLite::viridis(4, alpha = 0.8),
        xlab = "Exchange Trading Fund", 
        ylab = "Weekly Return", 
        main = "Distribution of Weekly Return for each Fund")


# Question e ------------------
calculate_summary_statistics(weekly_returns$AGG)
calculate_summary_statistics(weekly_returns$VAW)
calculate_summary_statistics(weekly_returns$IWN)
calculate_summary_statistics(weekly_returns$SPY)


# Question f ------------------

qqnorm(weekly_returns$AGG)
qqline(weekly_returns$AGG)

check_observations_are_iid_and_normal_distributed(weekly_returns$AGG)
check_observations_are_iid_and_normal_distributed(weekly_returns$VAW)
check_observations_are_iid_and_normal_distributed(weekly_returns$IWN)
check_observations_are_iid_and_normal_distributed(weekly_returns$SPY)

# AGG
calculate_the_mean_of_the_normal_sampling_distribution(n = 454, mu = 0.00027 ,sigma = 0.006)
# The mean of the sample means is  0.0002551747 
# The standard deviation of the sample means is 0.0002794381 

#VAW 
calculate_the_mean_of_the_normal_sampling_distribution(n = 454, mu = 0.00179, sigma = 0.0036)
# The mean of the sample means is  0.001791923 
# The standard deviation of the sample means is 0.0001650772 

#IWN
calculate_the_mean_of_the_normal_sampling_distribution(n = 454, mu = 0.01188, sigma = 0.032)

#SPY
calculate_the_mean_of_the_normal_sampling_distribution(n = 454, mu = 0.00136, sigma = 0.025)


# Question g ------------------
calculate_confidence_interval_for_the_mean(weekly_returns$AGG)
#The 90% confidence interval is: -0.0001965042 0.0007280182 
#The 95% confidence interval is: -0.0002854073 0.0008169213 
#The 99% confidence interval is: -0.0004597165 0.0009912304 

calculate_confidence_interval_for_the_mean(weekly_returns$VAW)
# The 90% confidence interval is: -0.0009973998 0.00458498 
# The 95% confidence interval is: -0.001534208 0.005121788 
# The 99% confidence interval is: -0.002586708 0.006174288 

calculate_confidence_interval_for_the_mean(weekly_returns$IWN)
# The 90% confidence interval is: -0.001288877 0.003664236 
# The 95% confidence interval is: -0.001765174 0.004140533 
# The 99% confidence interval is: -0.002699032 0.005074391 

calculate_confidence_interval_for_the_mean(weekly_returns$SPY)
#The 90% confidence interval is: -0.0005572163 0.003277427 
#The 95% confidence interval is: -0.00092596 0.003646171 
#The 99% confidence interval is: -0.001648942 0.004369153 


# This is a one sample hypothesis test to find the confidence interval for the mean. 
t.test(weekly_returns$AGG, conf.level = 0.95)$conf.int



# Question h ------------------
t.test(weekly_returns$AGG, mu = 0)



# Question i ------------------
t.test(weekly_returns$VAW, weekly_returns$AGG)

#*

# Question j ------------------
# Opinion question 


# Question k ------------------
resultz <- cor(weekly_returns[c("AGG", "VAW", "IWN", "SPY")], use = "pairwise.complete.obs")

cor(weekly_returns$VAW, weekly_returns$IWN)

ggplot(data = weekly_returns ,
       mapping = aes(x = VAW, y = IWN))+
  geom_line(col = "grey")+
  geom_point(col = "darkblue")+
  theme_classic()+
  labs(
       subtitle = "Correlation of Exchange Trade Funds",
       x = "VAW",
       y = "IWN")

