attach(airline)

names(airline)

tapply(fare, year, mean)
sapply(airline[c("fare", "passen", "dist")],mean)


plot(dist,fare, 
     main = "The fare price of airline based on distance travelled",
     xlab = "Distance travelled",
     ylab = "Fare", pch=16, col="blue")
abline(lm(fare~dist), col="black")

barplot(tapply(fare, year, mean),
        main = "Average fare per year",
        xlab = "Year",
        ylab = "Fare", col="red",
        ylim = c(0,200))

cbind(fare=tapply(fare,year,mean), passengers=tapply(passen,year,mean))

plot(data$bmktshr,data$fare)

#fare prices in the year 2000

mod.1=lm(fare~y00,data=airline)
summary(mod.1)    

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  175.721      1.272 138.110  < 2e-16 ***
#   y00           12.302      2.545   4.835 1.38e-06 ***

f.year=factor(airline$year)

mod.2=lm(passen~f.year,data=airline)
summary(mod.2)

cbind(mean.fare=tapply(fare,year,mean), sd.fare=tapply(fare,year,sd),
      obs.fare=tapply(fare,year,length))

# fare from chicago to illinois
chicago.orig <- subset(airline, origin=="CHICAGO, IL")
chicago.dest <- subset(airline, destin=="CHICAGO, IL")

mean(chicago.orig$fare)
mean(chicago.dest$fare)
sd(chicago.orig$fare)
sd(chicago.dest$fare)

#linear regression showing the dependence of fare on year, distance and passengers

mod.3= lm(fare~f.year+ldist+lpassen)
summary(mod.3)
