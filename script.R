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


data <- mutate(data, X1d4 = as.numeric(data$X1d))
hist(data$X1d4)
data <- mutate(data, X1d2 = ifelse(data$X1d == 'Public Transport', 1,  
  ifelse(data$X1d == 'Both: Car &Public Transport', 2,  ifelse(data$X1d == 'Car', 3, 0))))
data <- mutate(data, X1d3 = ifelse(data$X1d == 'Car', 1,  0))



data$X.1c. = gsub(",", ".", data$X.1c.)
data$X.1c. = as.double(as.character(data$X.1c.))
data <- mutate(data, X.1c.2 = ifelse(data$X2a == 1, X.1c.,  NA))

data$X5a = as.POSIXct(strptime(data$X5a,format='%H:%M:%S'))
data$X5b = as.POSIXct(strptime(data$X5b,format='%H:%M:%S'))
data <- mutate(data, X5c2 = ifelse(data$X5c == '5 Minutes', 1,  ifelse(data$X5c == '10 Minutes', 2,  
  ifelse(data$X5c == '15 Minutes', 3, ifelse(data$X5c == '20 Minutes', 4, ifelse(data$X5c == '30 Minutes', 5, 6))))))
data <- mutate(data, X5d = ifelse(data$X5d == 'Less than 2 hours', 1,  ifelse(data$X5d == '2 - 12 hours', 2,  
  ifelse(data$X5d == '12 - 24 hours', 3, ifelse(data$X5d == 'more than 24 hours', 4, 0)))))
levelsX5e = levels(data$X5e)
data$X5e =  as.numeric(data$X5e)

data <- mutate(data, X5g = as.numeric(data$X5f))
data <- mutate(data, X5g = ifelse(X5g!=1,X5g ,  NA))
data <- mutate(data, X5g = ifelse(X5g==6,0,ifelse(X5g==4,2,ifelse(X5g==3,3,ifelse(X5g==5,4,ifelse(X5g==2,5,6))))))

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
data$X9a = as.numeric(data$X9a)
data <- mutate(data, X9a = ifelse(X9a!=1,X9a ,  NA))
data <- mutate(data, X9b = ifelse(X9a==6,1,ifelse(X9a==4,3,ifelse(X9a==3,4,ifelse(X9a==2,5,
  ifelse(X9a==9,7,ifelse(X9a==7,6,X9a)))))))





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
hist(data$X.1c.,xlab="Travel distance in kilometers",main="", breaks=50, col="gray")
hist(data$X.1c.2,add=T, col="#f9af23", breaks=50)
legend("topright", c("Interested", "Not Interested"), fill=c("#f9af23","gray"))

# Travel Distance on Travel Option
plot(data$X.1c.,data$X1d, xlab="Travel Distance", ylab="Traevl Option")
abline(lm(X1d ~ X.1c., data=data),col="red")

#Travel Time
startHour = strptime("6:00:00",format='%H:%M:%S')
endHour = strptime("22:00:00",format='%H:%M:%S')
series <- seq(from = startHour, to = endHour, by = "hour")
hist(data$X5b,"mins",ylab="Frequency",main="", xlab="Arrival and departure time",
     col=rgb(.5,.5,.5,0.4), breaks=50, xlim=as.POSIXct(c(startHour,endHour)),xaxt='n',yaxt='n')
hist(data$X5a,"mins",add=T, col=rgb(.98,.69,.14,0.6), breaks=35, xaxt='n',yaxt='n')
axis.POSIXct(1, at=c(series), labels=format(series, '%H:%M'))
axis(2, at=c(0,0.000015625,0.00003125,0.000046875,0.0000625,0.000078125,0.00009375,0.000109375,0.000125,
  0.000140625,0.00015625,0.000171875,0.0001875), labels=c("0","5","10","15","20","25","30","35","40","45","50","55","60"))
legend("topright", c("Arrival time","Departure time"), fill=c(rgb(.98,.69,.14,0.6),rgb(.5,.5,.5,0.4)),cex = 1.3)


startHour = strptime("7:00:00",format='%H:%M:%S')
endHour = strptime("12:00:00",format='%H:%M:%S')
plot(data$X.1c.,data$X5a, xlab="Travel distance", ylab="Arrival time", ylim=as.POSIXct(c(startHour,endHour)))
axis.POSIXct(2, at=startHour, labels=format(startHour, '%H:%M:%S'))
axis.POSIXct(2, at=endHour, labels=format(endHour, '%H:%M:%S'))

startHour = strptime("12:00:00",format='%H:%M:%S')
endHour = strptime("20:00:00",format='%H:%M:%S')
plot(data$X.1c.,data$X5b, xlab="Travel distance", ylab="Departure time", ylim=as.POSIXct(c(startHour,endHour)))
axis.POSIXct(2, at=startHour, labels=format(startHour, '%H:%M:%S'))
axis.POSIXct(2, at=endHour, labels=format(endHour, '%H:%M:%S'))

# Flexibility
count(data$X5c2)
hist(data$X5c2, breaks=0:6, xaxt='n',  xlab="Time flexibility of respondents", main="",col="gray")
axis(1,c(0.5:5.5),c("5 minutes","10 minutes","15 minutes","20 minutes","30 minutes",">30 minutes"))


# Travel Distance on Willingsness to Pay
hist(data$wppkm,xlab="Willingsness to pay in euro per commute kilometer ",breaks=40,main="",xaxt='n')
axis(1,c(seq(0, 1, 0.05)),c(seq(0, 1, 0.05)),ylab="bla")
plot(col=rgb(.98,.69,.14,1), data$X.1c.,data$X6a, xlab="Travel distance in kilometers", 
     ylab="Willingsness to pay in euro for total distance",xlim = c(0,90))
wtplm = lm(X6a ~ X.1c., data=data) #linear model by fitting
summary(wtplm)$sigma  # residual standard deviation:  2.343433
abline(wtplm,col=rgb(.98,.69,.14,1))
abline(0,0.15,col="blue")
abline(0,0.6,col="green")
legend("topright", c("Average traveling costs by car (0.6 euro/km)","German tax refunds (0.15 euro/km)",
  "Fitted willingsness to pay"), fill=c("green","blue",rgb(.98,.69,.14,1)),cex = 1.2)
mean(data$wppkm, na.rm=TRUE) # 0.1817779
median(data$wppkm, na.rm=TRUE) # 0.1296794

# Travel Distance on Expected Payout
hist(col="darkgray",data$eppkm,xlab="Expected Payout in Euro per Commute Kilometer",breaks=40)
plot(col="darkgray", data$X.1c.,data$X7a, xlab="Travel distance in kilometer", 
     ylab="Expected payout in euro for total distance", xlim = c(0,120),ylim = c(0,10))
eppklm = lm(X7a ~ X.1c., data=data)
summary(eppklm)$sigma #1.066353
abline(eppklm,col="darkgray")
abline(0,0.3,col="blue")
abline(0,0.6,col="green")
legend("topright", c("Average traveling costs by car (0,6 euro/km)","German tax refunds (0,3 euro/km)",
  "Fitted expected payout"), fill=c("green","blue", "darkgray"),cex = 1.2)
mean(data$eppkm, na.rm=TRUE) # 0.1262702
median(data$eppkm, na.rm=TRUE) # 0.09353147

# Detour Distance on Expected Payout
hist(col="gray", data$epdkm,xlab="Expected payout in euro per detour kilometer",breaks=40, main="",xaxt='n')
axis(1,c(seq(-0.025, 2, 0.1)),c(seq(0, 2, 0.1)),ylab="bla")
plot(data$X.1c.,data$X7b, xlab="Travel distance", ylab="Expected payout in euro per 10 detour kilometer")
abline(lm(X7b ~ X.1c., data=data),col="red")
mean(data$epdkm, na.rm=TRUE) # 0.4479167
median(data$epdkm, na.rm=TRUE) # 0.5


# Combined Drive & Passenger Prices
plot(data$X.1c.,data$X7a, xlab="Travel distance in kilometers", ylab="Expected payout in euro for total distance",
     xlim = c(0,120),ylim = c(0,10),col="darkgray")
points(data$X6a,col=rgb(.98,.69,.14,1))
axis(4,c(seq(0, 10, 2)),c(seq(0, 10, 2)),ylab="bla",col=rgb(0.54,0.17,0.89,0.4))
mtext("Willingness to pay in euro", side=4, col=rgb(.98,.69,.14,1), line = -2)
eppklm = lm(X7a ~ X.1c., data=data)
summary(eppklm)$sigma #1.066353
abline(eppklm,col="darkgray")
abline(wtplm,col=	rgb(.98,.69,.14,1))
abline(0,0.15,col="blue")
abline(0,0.6,col="green")
legend("topright", c("Average traveling costs by car (0.6 euro/km)","German tax refunds (0.15 euro/km)",
  "Fitted expected payout","Fitted willingness to pay"), fill=c("green","blue", "gray",rgb(.98,.69,.14,1)),cex = 1.2)


# Reasons for not Founding / Joining a Carpool
hist(data$X9a,xlab="Reasons for not being interested in ride-sharing",xaxt='n',ylim=c(0, 50), main="")
axis(1,c(2.5:8.5),c("1", "2", "3", "4", "5", "6", "7"))
legend("topright", 95, legend=c("1: No Need"  ,"2: Not interested financially","3: Loss of privacy",
  "4: Public transport is more convenient", "5: Public transport is more convenient and especially more sustainable",
  "6: Traffic jams","7: Environment"), cex=.95)


hist(data$X9b,xlab="Reasons for not being interested in ride-sharing",xaxt='n',ylim=c(0, 50), main="", breaks = 9, col="gray")
axis(1,c(1.5:7.5),c("1", "2", "3", "4", "5", "6", "7"))
legend("topright", 85, legend=c("1: Public transport is more convenient"  ,"2: Not interested financially",
  "3: No Need","4: Loss of privacy", "5: Public transport is more convenient and especially more sustainable",
  "6: Traffic jams","7: Environment"), cex=.95)

hist(data$X5g,xlab="Preferred matching characteristic", xaxt='n',ylim=c(0, 50), main="",  col="gray", breaks=8)
axis(1,c(0.5:5.5),c("1", "2", "3", "4", "5", "6"))
legend("topright", 85, legend=c("1: Just distance"  ,"2: Field of study","3: Age group / study degree","4: Gender",
  "5: Time of arrival","6: No special characteristics"), cex=.95)


### Influence on Ride-sharing
lm = lm(X2a ~  X.1c. + X1d2, data=data)
summary(lm)

lr <- glm(X2a ~ X.1c. + X1d2, family=binomial(link='logit'),data=data)
summary(lr)
dataTest <- data[c(which( colnames(data)=="X.1c."), which( colnames(data)=="X1d2"))]
lrAccuracy <- (sum(predict(lr, dataTest, type="response")>0.5 & data$X2a==1) 
               + sum(predict(lr, dataTest, type="response")<0.5 & data$X2a==0) ) / nrow(data)
lrAccuracy # 0.6470588
lmAccuracy <- (sum(predict(lm, dataTest, type="response")>0.5 & data$X2a==1) 
               + sum(predict(lm, dataTest, type="response")<0.5 & data$X2a==0) ) / nrow(data)
lmAccuracy #  0.6323529
