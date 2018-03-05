### LIBRARIES

library(dplyr)
library(car)

### READING DATA
setwd("/Users/lukasmohs/Desktop/MA-data-analysis/") #adjust to directiory
data = read.csv("data.csv",sep=',')

### MODIFY DATA
data <- mutate(data, X1a = ifelse(data$X1a == 'Bachelor Student', 1,  ifelse(data$X1a == 'Master Student', 2,  ifelse(data$X1a == 'PHD Student', 3, 0))))
data <- mutate(data, X1d = ifelse(data$X1d == 'Public Transport', 1,  ifelse(data$X1d == 'Both: Car &Public Transport', 2,  ifelse(data$X1d == 'Car', 3, 0))))
data <- mutate(data, X1c = as.numeric(data$X.1c.))
data$X5a = strptime(data$X5a,format='%H:%M:%S')
data$X5b = strptime(data$X5b,format='%H:%M:%S')

### EXPLORATION
summary(data)

# Degree on Travel Distance
plot(data$X1a,data$X.1c.,xlab="Degree ", ylab="Travel Distance")
abline(lm(X.1c. ~ X1a, data=data),col="red")

# Travel Distance on Travel Option
hist(data$X1c,xlab="Travel Kilometers")
plot(data$X1c,data$X1d, xlab="Travel Distance", ylab="Traevl Option")
histabline(lm(X1d ~ X1c, data=data),col="red")

#Travel Time
hist(data$X5a,"mins",xlab="Arrival Time")
hist(data$X5b,"mins",xlab="Departure Time")

plot(data$X1c,data$X5a, xlab="Travel Distance", ylab="Arrival Time")
abline(tslm(X5a ~ X1c, data=data),col="red")
install.packages('forecast', dependencies = TRUE)