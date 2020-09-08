# Research Project- Impact of Ontario rent control policy on rental housing
#starts
#Interrupted time series analysis


#the following packages need to be installed
install.packages("tableone") #create tableone package
install.packages("car") #car package
install.packages("lmtest") #linear test package
install.packages("prais") #prais winsten package
install.packages("ggplot2") #GG Plot package
install.packages("stargazer") #stargazer package

# activate the packages above

library(tableone)
library(car)
library(lmtest)
library(prais)
library(ggplot2)
library(stargazer)

names(HSR)
View(HSR)

#First intervention-rent control-1975

attach(HSR)

#descriptive statistics

HSR$rent_control_1975= factor(HSR$rent_control_1975,
                              labels = c("before 1975","after 1975"))

HSR_1975_Mean<- tapply(HSR$OHSrental,HSR$rent_control_1975, mean)
HSR_1975_Mean

HSR_1975_SD<- tapply(HSR$OHSrental,HSR$rent_control_1975, sd)
HSR_1975_SD

HSR_1975_obs<-HSR_1975_SD<- tapply(HSR$OHSrental,HSR$rent_control_1975, length)
HSR_1975_obs

cbind(HSR_1975_Mean,HSR_1975_SD,HSR_1975_obs)

#Visualizing rent control-1975

plot(HSR$OHSrental_units, col="blue", pch=19,
     main = "Plot1: 1975 rent control",
     ylab = "Rental housing starts in Ontario",
     xlab = "year",
     xlim = c(0,50),
     ylim = c(0,50000),
     xaxt="n")
     axis(1, at=1:367, labels = HSR$year)
     abline(v=7, lty=2)
     
     # The plot depicts that the rental housing starts were declining prior to the 
     # introduction of rent control in 1975(first intervention).
     # However, after the intervention, rental housing starts increased slightly 
     # to 20000 units and again decreased and increased till 1991. 
     # However, after 1991, there was a sharp decline in rental housing starts 
     # and it never increased to 10000 units approximately which were at the
     # time (1975) when the law was imposed.

#linear models for rent control 1975

attach(HSR)

mod.1975=lm(HSR$OHSrental_units~time+rent_control_1975+`1975_trend`,
            data = HSR)
summary(mod.1975)

# At the beginning of time that is at time 0(before 1969) the rental starts 
# were 46494 units. Over time, the rental starts decreased by 2529 units 
# with each additional year, 
# but only up to the intervention (1975). The result is significant with 
# t value>1.96 and p value<0.05. At the intervention, 
# the rental starts declined by 17850 units. After the intervention 
# that is, after 1975, the rental starts increased by 2261 units per year. 
# The rental starts at the intervention and after the intervention 
# define a statistically significant relationship which is evident from 
# the respective t and p values.

# DURBIN-WATSON TEST(TO TEST SERIAL AUTOCORRELATION ) AND PRAIS WINSTON 
#TEST-RENT CONTROL LAW 1975
library(car)
library(lmtest)
library(prais)

durbin.watson(mod.1975, max.lag=50, alternative="two sided")
dt.1975<-dwt(mod.1975, max.lag=50)
dt.1975

#The model suggests that there exists serial autocorrelation between 
#variables as the value of d-w statistic lies between 0 and 2 showing
#positive autocorrelation


#PLOTTING RESIDUALS

plot(residuals(mod.1975),
     type = 'o',
     pch=16,
     main = "1975 Rent Control Residuals",
     xlab = "time",
     ylab="Residuals",
     col="purple",
     xaxt="n")
abline(h=0, lty=2)
axis(1,at=1:367, labels=HSR$year)
acf(residuals(mod.1975), main="Residuals for 1975 rent control model")
acf(residuals(mod.1975), type = "partial", main="Residuals(partial)
    for 1975 rent control model")

#################################################
#SECOND INTERVENTION IN 1991, POLICY AMMENDENTMENT
##################################################

#Descriptive Statistics

attach(HSR)


HSR$RC1991=factor(HSR$`1991_policy`, labels=c("Before 1991","After 1991"))

HSR_1991_MEAN= tapply(HSR$OHSrental_units, HSR$RC1991, mean)
HSR_1991_MEAN

HSR_1991_SD= tapply(HSR$OHSrental_units, HSR$RC1991, sd)
HSR_1991_SD

#visualizing the 1991 policy impact

attach(HSR)

plot(HSR$OHSrental_units, type = 'o', col="dark green", pch=19,
     main = "Plot2:1991 policy",
     ylab = "Rental housing starts of Ontario",
     xlab = "year",
     xlim = c(0,50),
     ylim = c(0,50000),
     xaxt="n")                                                       
axis(1, at=1:367, labels= HSR$year)                                                       
abline(v=23, col="black",lty=2)                                                       

# The plot depicts that the rental housing starts declined from the year 
# 1972 to 1975. After 1975, the trend of rental housing starts increased 
# and decreased till the year 1991. However, after the intervention, 
# rental housing starts decreased gradually till the year 1997 where 
# the rental units nearly touched zero. Further, after 1997, 
# the rental housing starts increased to 5000 units in the year 2003. 
# Prior to the intervention, from the year 1989 to 1991, the housing 
# rental starts were increasing and after the intervention the units declined.


#linear models

mod.1991<-lm(HSR$OHSrental_units~time+`1991_policy`+`1991_trend`,data = HSR)
summary(mod.1991)

# The model suggests that, at the beginning of time that at time 0, 
# the rental starts were 37250 units. Over time, the rental starts 
# decreased by 1562 units with each additional year, but only up to 
# the intervention (1991). The result is significant with t value>1.96 
# and p value<0.05. At the intervention, the rental starts increased by 
# 1955 units but the difference is not statistically significant. 
# After the intervention that is, after 1991, the rental starts 
# increased by 1546 units per year. The rental starts after the 
# intervention define a statistically significant relationship 
# which is evident from the respective t and p values. 
# The model explains 75% of the variance.

#DURBIN WATSON TEST

durbinWatsonTest(mod.1991, max.lag = 50, alternative = "two sided")
dt.1991<- dwt(mod.1991, max.lag=50)

#PLOTTING THE RESIDUALS

plot(residuals(mod.1991),
     type = 'o',
     pch=16,
     main = "1991 policy impact residuals",
     xlab = "Time",
     ylab = "Residuals",
     col="Dark Green",
     xaxt="n")

abline(h=0, lty=2)
axis(1, at=1:367, labels = HSR$year)

acf(residuals(mod.1991), main="Residuals for 1991 policy impact model")
acf(residuals(mod.1991),type = "partial",main="residuals(partial) 
    for 1991 policy impact model")



####################################################################
#Third Intervention Policy Ammendment:1997
###################################################################

#Descriptive Statistics

attach(HSR)
names(HSR)
HSR$RC1997=factor(HSR$`1997_policy`, labels=c("Before 1997","After 1997"))

HSR_1997_MEAN= tapply(HSR$OHSrental_units, HSR$RC1997, mean)
HSR_1997_MEAN

HSR_1997_SD= tapply(HSR$OHSrental_units, HSR$RC1997, sd)
HSR_1997_SD

HSR_1997_OBS= tapply(HSR$OHSrental_units, HSR$RC1997, length)
HSR_1997_OBS

#VISUALIZATION OF 1997 POLICY IMPACT

plot(HSR$OHSrental_units, type = 'o', col="dark green", pch=19,
     main = "Plot3:1997 policy impact",
     ylab = "Rental housing starts of Ontario",
     xlab = "year",
     xlim = c(0,50),
     ylim = c(0,50000),
     xaxt="n")                                                       
axis(1, at=1:367, labels= HSR$year)                                                       
abline(v=29, col="black",lty=2)            

#linear models- 1997 policy impact
mod.1997<-lm(HSR$OHSrental_units~time+`1997_policy`+`1997_trend`,data = HSR)
summary(mod.1997)

#DURBIN WATSON TEST

durbinWatsonTest(mod.1997, max.lag = 50, alternative = "two sided")
dt.1997<- dwt(mod.1997, max.lag=50)

#PLOTTING THE RESIDUALS

plot(residuals(mod.1997),
     type = 'o',
     pch=16,
     main = "1997 policy impact residuals",
     xlab = "Time",
     ylab = "Residuals",
     col="Dark Green",
     xaxt="n")

abline(h=0, lty=2)
axis(1, at=1:367, labels = HSR$year)

acf(residuals(mod.1997), main="Residuals for 1997 policy impact model")
acf(residuals(mod.1997),type = "partial",main="residuals(partial) 
    for 1991 policy impact model")

##############################################################
# 4th intervention-policy ammendement:2017
##############################################################

#Descriptive statistics-2017 Policy Impact


attach(HSR)
names(HSR)
HSR$RC2017=factor(HSR$`2017_policy`, labels=c("Before 2017","After 2017"))

HSR_2017_MEAN= tapply(HSR$OHSrental_units, HSR$RC2017, mean)
HSR_2017_MEAN

HSR_2017_SD= tapply(HSR$OHSrental_units, HSR$RC2017, sd)
HSR_2017_SD

HSR_1997_OBS= tapply(HSR$OHSrental_units, HSR$RC2017, length)
HSR_1997_OBS

#VISUALIZATION OF 2017 POLICY IMPACT

plot(HSR$OHSrental_units, type = 'o', col="dark green", pch=19,
     main = "Plot 4:2017 policy impact",
     ylab = "Rental housing starts of Ontario",
     xlab = "year",
     xlim = c(0,50),
     ylim = c(0,50000),
     xaxt="n")                                                       
axis(1, at=1:367, labels= HSR$year)                                                       
abline(v=49, col="black",lty=2)            

#linear models- 2017 policy impact
mod.2017<-lm(HSR$OHSrental_units~time+`2017_policy`+`2017_trend`,data = HSR)
summary(mod.2017)

#DURBIN WATSON TEST

durbinWatsonTest(mod.2017, max.lag = 50, alternative = "two sided")
dt.2017<- dwt(mod.2017, max.lag=50)

#PLOTTING THE RESIDUALS

plot(residuals(mod.2017),
     type = 'o',
     pch=16,
     main = "1997 policy impact residuals",
     xlab = "Time",
     ylab = "Residuals",
     col="Dark Green",
     xaxt="n")

abline(h=0, lty=2)
axis(1, at=1:367, labels = HSR$year)

acf(residuals(mod.2017), main="Residuals for 1997 policy impact model")
acf(residuals(mod.2017),type = "partial",main="residuals(partial) 
    for 1991 policy impact model")
