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
data$X5a = as.POSIXct(strptime(data$X5a,format='%H:%M:%S'))
data$X5b = as.POSIXct(strptime(data$X5b,format='%H:%M:%S'))
data <- mutate(data, X5c = ifelse(data$X5c == '5 Minutes', 5,  ifelse(data$X5c == '10 Minutes', 10,  ifelse(data$X5c == '15 Minutes', 15, ifelse(data$X5c == '20 Minutes', 20, ifelse(data$X5c == '30 Minutes', 30, 40))))))
data <- mutate(data, X5d = ifelse(data$X5d == 'Less than 2 hours', 1,  ifelse(data$X5d == '2 - 12 hours', 2,  ifelse(data$X5d == '12 - 24 hours', 3, ifelse(data$X5d == 'more than 24 hours', 4, 0)))))
levelsX5e = levels(data$X5e)
#[1] ""                              "All"                           "All of the above"              "Bank Transfer"                 "Bier"                         
#[6] "Cash"                          "Cash and bank transfer"        "Payment Service (i.e. Paypal)" "Using an app"  
data$X5e =  as.numeric(data$X5e)

levelsX5f = levels(data$X5f)
#[1] ""                          "0 special characteristics" "Age group / study degree"  "Field of study"            "Gender"                    "Just distance"            
#[7] "Time of arrival"    
data$X5f =  as.numeric(data$X5f)

data$X6a = gsub(",", ".", data$X6a)
data$X6a = as.double(as.character(data$X6a))
data <- mutate(data, wppkm = ifelse(!is.na(data$X6a), X6a/X.1c. ,  NA))

data$X7a = gsub(",", ".", data$X7a)
data$X7a = as.double(as.character(data$X7a))
data <- mutate(data, eppkm = ifelse(!is.na(data$X7a), X7a/X.1c. ,  NA))

data$X7b = gsub(",", ".", data$X7b)
data$X7b = as.double(as.character(data$X7b))
data <- mutate(data, epdkm = ifelse(!is.na(data$X7b), X7b/10 ,  NA))

levelsX9a = levels(data$X9a)
#[1] ""    [2] "0 Need"   [3] "0t interested financially"   [4] "Loss of flexibility"                                                
# [5] "Loss of privacy"    [6] "Public transport is more convenient"                                
# [7] "Public transport is more convenient and especially more sustainable"
# [8] "Traffic jams"     [9] "Umwelt" 
data$X9a = as.numeric(data$X9a)
data <- mutate(data, X9a = ifelse(X9a!=1,X9a ,  NA))

### EXPLORATION
summary(data)

# Degree on Travel Distance
plot(data$X1a,data$X.1c.,xlab="Degree ", ylab="Travel Distance")
abline(lm(X.1c. ~ X1a, data=data),col="red")

# Travel Distance on Travel Option
hist(data$X.1c.,xlab="Travel Kilometers")
plot(data$X.1c.,data$X1d, xlab="Travel Distance", ylab="Traevl Option")
abline(lm(X1d ~ X.1c., data=data),col="red")

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


# Travel Distance on Willingsness to Pay
hist(data$wppkm,xlab="Willingsness to Pay in Euro per Commute KM ",breaks=40)
plot(data$X.1c.,data$X6a, xlab="Travel Distance", ylab="Willingsness to Pay for total Distance",ylim = c(0,8),xlim = c(0,90))
abline(lm(X6a ~ X.1c., data=data),col="red")
mean(data$wppkm, na.rm=TRUE)
median(data$wppkm, na.rm=TRUE)

# Travel Distance on Expected Payout
hist(data$eppkm,xlab="Expected Payout in Euro per Commute KM",breaks=40)
plot(data$X.1c.,data$X7a, xlab="Travel Distance", ylab="Expected Payout in Euro for total Distance")
abline(lm(X7a ~ X.1c., data=data),col="red")
mean(data$eppkm, na.rm=TRUE)
median(data$eppkm, na.rm=TRUE)

# Detour Distance on Expected Payout
hist(data$epdkm,xlab="Expected Payout in Euro per Detour KM",breaks=20)
plot(data$X.1c.,data$X7b, xlab="Travel Distance", ylab="Expected Payout in Euro per 10 Detour KM")
abline(lm(X7b ~ X.1c., data=data),col="red")
mean(data$epdkm, na.rm=TRUE)
median(data$epdkm, na.rm=TRUE)



# Reasons for not Founding / Joingin a Carpool
hist(data$X9a,xlab="Reasons for not Founding / Joingin a Carpool")
legend("topright", 95, legend=c(levelsX9a), cex=.8)

