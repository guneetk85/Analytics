#interrupted time series analysis

#In 1989 california passed a law to increase tax on cigratte sales to 
#reduce smoking
#38 US states serve as potential controls meaning where no tax on cigratte 
#was imposed
#The data analyzed is from 1970 to 2000


cigsales_2<-read.csv("cigsales_2.csv")

install.packages("sandwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)

#variable names in the dataset

names(cigsales_2)

# California model
mod1 <- lm(cigsale ~ time + tax_dummy + tax_trend, data=cigsales_2, 
           subset=(state=="California"))
summary(mod1)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 132.2258     2.2866  57.826  < 2e-16 ***
#   time         -1.7795     0.2170  -8.199 8.36e-09 ***
#   tax_dummy   -20.0581     3.7471  -5.353 1.18e-05 ***
#   tax_trend    -1.4947     0.4846  -3.084  0.00467 ** 

#tax_dummy=1; the year tax was imposed and thereafter
#tax_trend- the year after the tax was imposed

#story of analysis: 
#all coefficients are statistically significant

# Before the imposition of tax, cigratte sales were decreasing at a rate of
#1.8 packets per capita
#at the intervention that is,
#when the tax was imposed in 1989, cigratte sales further dropped by 20 packs
#per capita
#after the tax was imposed, sales of cigrattes dropped by 1.5 packs per capita

# Treated California, Rest are controls
#impact of tax on california

mod2 <- lm(cigsale ~ time + california + cal_trend + tax_dummy + tax_trend + 
             cal_tax_dummy+cal_tax_trend, data=cigsales_2)

summary(mod2)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   135.4995     2.0783  65.198  < 2e-16 ***
#   time           -0.5478     0.1973  -2.777  0.00557 ** 
#   california     -3.2737    12.9789  -0.252  0.80091    
# cal_trend      -1.2317     1.2319  -1.000  0.31759    
# tax_dummy     -17.2517     3.4057  -5.066 4.71e-07 ***
#   tax_trend      -0.5035     0.4405  -1.143  0.25322    
# cal_tax_dummy  -2.8064    21.2686  -0.132  0.89505    
# cal_tax_trend  -0.9911     2.7508  -0.360  0.71867 

#story of analysis:
#in california, cigratte sales were decreasing at the rate of 3.2 packs per
#capita before the imposition of tax
#the year tax was imposed, cigratte sales dropped by 17.2 packs per capita
#the year tax was imposed, cigratte sales in california dropped by
#2.8 packs per capita but the coefficient is not statistically significant

#This means the decline in california was indistinguishable as compared to the
#controls where no tax was imposed

#comparison of cig sales in california, colarado, montana

mod3 <- lm(cigsale ~ time + california + cal_trend + tax_dummy + tax_trend + 
            cal_tax_dummy+cal_tax_trend, 
          data = subset(cigsales_2, state=="California" | state=="Colorado" | 
                          state == "Idaho" | state == "Montana"))

summary(mod3)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   130.4946     2.0355  64.111  < 2e-16 ***
#   time           -1.4645     0.1932  -7.580 9.25e-12 ***
#   california      1.7312     4.0709   0.425   0.6714    
# cal_trend      -0.3150     0.3864  -0.815   0.4167    
# tax_dummy     -13.5887     3.3355  -4.074 8.49e-05 ***
#   tax_trend       0.4746     0.4314   1.100   0.2735    
# cal_tax_dummy  -6.4694     6.6710  -0.970   0.3342    
# cal_tax_trend  -1.9693     0.8628  -2.282   0.0243 *  

#on an average, 1 person smokes 130 packs of cigratte in year
#in california, 1 person smokes 1.7 packs of cigratte in a year
#with an onset of tax imposition, cigratte sales decreased by 13.5 packs
#per year
#in california, cig sales decreased by 1.9 packs per year after the tax
#was imposed

#visualization

newdata = subset(cigsales_2, state=="California" | state=="Colorado" | 
                   state == "Idaho" | state == "Montana")
newdata$state <- factor(newdata$state)
newdata$california =factor(newdata$california,
                           labels = c("Controls", "California"))
mod4 <- lm(cigsale ~ time + california + cal_trend + tax_dummy + tax_trend + 
             cal_tax_dummy+cal_tax_trend, data = newdata)

newdata$pred <- predict(mod4)
# aggregate data

aggdata <-aggregate(newdata, by=list(newdata$california, newdata$year), 
                    FUN=mean, na.rm=TRUE)

# The above creates a new data set with Group.1 for the treatment indicator
# and Group.2 for year.

install.packages("ggplot2")
library(ggplot2)
ggplot(aggdata, aes(x = Group.2, y = cigsale,color=Group.1)) +
  geom_point(size=2) +
  geom_smooth(data = subset(aggdata, Group.2 <= 1989),  method="lm", se=F) +
  geom_smooth(data = subset(aggdata, Group.2 > 1989),  method="lm", se=F) +
  theme_bw() +  labs(colour="") +
  geom_vline(xintercept = 1989, linetype="dashed") +
  labs(title = "Impact of new cigarette taxes in California", 
       subtitle = "Comparing California with comparable States",
       y = "Sales of packs of cigarettes smoked per person", x= "Year")

ggplot(newdata, aes(x = year, y = pred,color=california)) +
  geom_point() +
  theme_bw() +  labs(colour="") +
  geom_vline(xintercept = 1989, linetype="dashed") +
  labs(title = "Impact of new cigarettes taxes in California", 
       subtitle = "Comparing California with controls" ,
       caption = " Application of Interrupted Time Series Analysis" ,
       y = "Packs of cigarettes smoked per person")



