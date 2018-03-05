### LIBRARIES

library(dplyr)
library(car)

### READING DATA
setwd("/Users/lukasmohs/Desktop/MA-data-analysis/") #adjust to directiory
data = read.csv("data.csv",sep=',')

### MODIFY DATA
data <- mutate(data, X1a = ifelse(data$X1a == 'Bachelor Student', 1,  ifelse(data$X1a == 'Master Student', 2,  ifelse(data$X1a == 'PHD Student', 3, 0))))
data <- mutate(data, X1d = ifelse(data$X1d == 'Public Transport', 1,  ifelse(data$X1d == 'Both: Car &Public Transport', 2,  ifelse(data$X1d == 'Car', 3, 0))))
data$X.1c. = gsub(",", ".", data$X.1c.)
data$X.1c. = as.double(as.character(data$X.1c.))
data$X5a = strptime(data$X5a,format='%H:%M:%S')
data$X5b = strptime(data$X5b,format='%H:%M:%S')
data <- mutate(data, X5c = ifelse(data$X5c == '5 Minutes', 5,  ifelse(data$X5c == '10 Minutes', 10,  ifelse(data$X5c == '15 Minutes', 15, ifelse(data$X5c == '20 Minutes', 20, ifelse(data$X5c == '30 Minutes', 30, 40))))))
data <- mutate(data, X5d = ifelse(data$X5d == 'Less than 2 hours', 1,  ifelse(data$X5d == '2 - 12 hours', 2,  ifelse(data$X5d == '12 - 24 hours', 3, ifelse(data$X5d == 'more than 24 hours', 4, 0)))))
levels(data$X5e)
#[1] ""                              "All"                           "All of the above"              "Bank Transfer"                 "Bier"                         
#[6] "Cash"                          "Cash and bank transfer"        "Payment Service (i.e. Paypal)" "Using an app"  
data$X5e =  as.numeric(data$X5e)

levels(data$X5f)
#[1] ""                          "0 special characteristics" "Age group / study degree"  "Field of study"            "Gender"                    "Just distance"            
#[7] "Time of arrival"    
data$X5f =  as.numeric(data$X5f)

data$X6a = gsub(",", ".", data$X6a)
data$X6a = as.double(as.character(data$X6a))

### EXPLORATION
summary(data)

# Degree on Travel Distance
plot(data$X1a,data$X.1c.,xlab="Degree ", ylab="Travel Distance")
abline(lm(X.1c. ~ X1a, data=data),col="red")

# Travel Distance on Travel Option
hist(data$X.1c.,xlab="Travel Kilometers")
plot(data$X.1c.,data$X1d, xlab="Travel Distance", ylab="Traevl Option")
histabline(lm(X1d ~ X.1c., data=data),col="red")

#Travel Time
hist(data$X5a,"mins",xlab="Arrival Time")
hist(data$X5b,"mins",xlab="Departure Time")

startHour = strptime("7:00:00",format='%H:%M:%S')
endHour = strptime("12:00:00",format='%H:%M:%S')
plot(data$X.1c.,data$X5a, xlab="Travel Distance", ylab="Arrival Time", ylim=as.POSIXct(c(startHour,endHour)))
axis.POSIXct(2, at=startHour, labels=format(startHour, '%H:%M:%S'))
axis.POSIXct(2, at=endHour, labels=format(endHour, '%H:%M:%S'))

startHour = strptime("12:00:00",format='%H:%M:%S')
endHour = strptime("20:00:00",format='%H:%M:%S')
plot(data$X.1c.,data$X5b, xlab="Travel Distance", ylab="Departure Time", ylim=as.POSIXct(c(startHour,endHour)))
axis.POSIXct(2, at=startHour, labels=format(startHour, '%H:%M:%S'))
axis.POSIXct(2, at=endHour, labels=format(endHour, '%H:%M:%S'))

# Flexibility
hist(data$X5c)


# Travel Distance on Price Elasticity
hist(data$X.1c.,xlab="Willingsness to Pay")
plot(data$X.1c.,data$X6a, xlab="Travel Distance", ylab="Willingsness to Pay",ylim = c(0,8))
abline(lm(X6a ~ X.1c., data=data),col="red")

