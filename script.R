### LIBRARIES

library(dplyr)
library(plyr)
library(car)

### READING DATA
setwd("/Users/lukasmohs/Desktop/MA-data-analysis/") #adjust to directiory
data = read.csv("data.csv",sep=',')


### MODIFY DATA
levels(data$X1a)
#[1] "Alumni"           "Bachelor Student" "Master Student"   "PHD Student"   
data <- mutate(data, X1a2 = as.numeric(data$X1a))


data <- mutate(data, X1d2 = ifelse(data$X1d == 'Public Transport', 1,  ifelse(data$X1d == 'Both: Car &Public Transport', 2,  ifelse(data$X1d == 'Car', 3, 0))))
data <- mutate(data, X1d3 = ifelse(data$X1d == 'Car', 1,  0))


data$X.1c. = gsub(",", ".", data$X.1c.)
data$X.1c. = as.double(as.character(data$X.1c.))
data <- mutate(data, X.1c.2 = ifelse(data$X2a == 1, X.1c.,  NA))

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
#[1] ""    [2] "0 Need"   [3] "0t interested financially"   [4] "Loss of flexibility" [5] "Loss of privacy"    [6] "Public transport is more convenient"                                
# [7] "Public transport is more convenient and especially more sustainable"  [8] "Traffic jams"     [9] "Umwelt" 
data$X9a = as.numeric(data$X9a)
data <- mutate(data, X9a = ifelse(X9a!=1,X9a ,  NA))





### EXPLORATION
#Population
n = length(data$X1a2) # 136
numberCarDrivers = length(which(data$X1d2 == 3)) # 35
numberNonCarDrivers = length(which(data$X1d != 3)) # 101

#Gender
length(which(data$X1b == "Male")) #101
length(which(data$X1b == "Female")) #35
#Percent Male
length(which(data$X1b == "Male")) / n # 0.7426471

#Degree #[1] "Alumni"           "Bachelor Student" "Master Student"   "PHD Student"   
count(data$X1a2)
#x freq
#1    1
#2   87
#3   46
#4    2

#Sum Carpool already
length(which(data$X3a == 0)) #4
#Perent Carpool already
length(which(data$X3a == 0)) / n # 0.02941176

# Percent Intrerested
length(which(data$X2a == 1)) / n # 0.595

# Percent Car Intrerested
length(which(data$X2a == 1 & data$X1d2 == 3)) / numberCarDrivers # 0.828

# Percent Not Car Intrerested
length(which(data$X2a == 1 & data$X1d != 3)) / numberNonCarDrivers # 0.514

# Travel Distance
mean(data$X.1c) # 23.82794
median(data$X.1c) #18.1
hist(data$X.1c.,xlab="Travel Kilometers",main="", breaks=50, col="red")
hist(data$X.1c.2,add=T, col="green", breaks=50)
legend("topright", c("All Respondents", "Interested Respondents"), fill=c("red", "green"))

# Travel Distance on Travel Option
plot(data$X.1c.,data$X1d, xlab="Travel Distance", ylab="Traevl Option")
abline(lm(X1d ~ X.1c., data=data),col="red")

#Travel Time
startHour = strptime("6:001:00",format='%H:%M:%S')
endHour = strptime("21:45:00",format='%H:%M:%S')
hist(data$X5b,"mins",xlab="Arrival and Departure Time",col=rgb(1,0,0,0.4), breaks=50, xlim=as.POSIXct(c(startHour,endHour)))
hist(data$X5a,"mins",xlab="Arrival Time",add=T, col=rgb(0,1,0,0.4), breaks=35)
axis(side=1, at=seq(startHour,endHour, 10), labels=seq(0,1000,100))

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
plot(data$X.1c.,data$X6a, xlab="Travel Distance", ylab="Willingsness to Pay for total Distance",xlim = c(0,90))
wtplm = lm(X6a ~ X.1c., data=data) #linear model by fitting
summary(wtplm)$sigma  # residual standard deviation:  2.343433
abline(wtplm,col="red")
abline(0,0.3,col="blue")
abline(0,0.6,col="green")
legend("topright", c("Average Traveling Costs by Car (60 cents/km)","German Tax Refunds (30 cents/km)","Fitted Willingsness to Pay"), fill=c("green","blue", "red"),cex = 1.2)
mean(data$wppkm, na.rm=TRUE) # 0.1817779
median(data$wppkm, na.rm=TRUE) # 0.1296794

# Travel Distance on Expected Payout
hist(data$eppkm,xlab="Expected Payout in Euro per Commute KM",breaks=40)
plot(data$X.1c.,data$X7a, xlab="Travel Distance", ylab="Expected Payout in Euro for total Distance")
abline(lm(X7a ~ X.1c., data=data),col="red")
abline(0,0.3,col="blue")
mean(data$eppkm, na.rm=TRUE) # 0.1262702
median(data$eppkm, na.rm=TRUE) # 0.09353147

# Detour Distance on Expected Payout
hist(data$epdkm,xlab="Expected Payout in Euro per Detour KM",breaks=40)
plot(data$X.1c.,data$X7b, xlab="Travel Distance", ylab="Expected Payout in Euro per 10 Detour KM")
abline(lm(X7b ~ X.1c., data=data),col="red")
mean(data$epdkm, na.rm=TRUE)
median(data$epdkm, na.rm=TRUE)


# Reasons for not Founding / Joingin a Carpool
hist(data$X9a,xlab="Reasons for not Founding / Joingin a Carpool")
legend("topright", 95, legend=c(levelsX9a), cex=.8)

count(data$X9a)
#x freq
#NA   82
#2    3
#3    6
#4   13
#5    1
#6   28
#7    1
#8    1
#9    1
levelsX9a
#[1] ""    [2] "No Need"   [3] "Not interested financially"   [4] "Loss of flexibility"                                                
# [5] "Loss of privacy"    [6] "Public transport is more convenient"                                
# [7] "Public transport is more convenient and especially more sustainable"
# [8] "Traffic jams"     [9] "Umwelt" 

### Influence on Ride-sharing
lm = lm(X2a ~ X1a + X1b + X.1c. + X1d2, data=data)
summary(lm)

lr <- glm(X2a ~ X.1c. + X1d2, family=binomial(link='logit'),data=data)
summary(lr)
dataTest <- data[c(which( colnames(data)=="X.1c."), which( colnames(data)=="X1d2"))]
lrAccuracy <- (sum(predict(lr, dataTest, type="response")>0.5 & data$X2a==1) 
               + sum(predict(lr, dataTest, type="response")<0.5 & data$X2a==0) ) / nrow(data)
lrAccuracy # 0.6470588
