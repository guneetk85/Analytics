#load a new dataset

#names of variables

names(teachingratings)

# [1] "minority"    "age"         "gender"      "credits"     "beauty"     
# [6] "eval"        "division"    "native"      "tenure"      "students"   
# [11] "allstudents" "prof"

#attach/commit data to memory

attach(teachingratings)

#summarize data set

summary(teachingratings)

mean(age)
mean(eval)
mean(students)

#introducing the apply family

sapply(teachingratings[c("age","eval","students","beauty")],mean)

#disable scientific notation

options(scipen=9)

sapply(teachingratings[c("age","eval","students","beauty")],mean)

#round the output to 2 digits

my.means=sapply(teachingratings[c("age","eval","students","beauty")],mean)
my.means
round(my.means,2)

#means by groups

tapply(eval,gender,mean)

#installing a new package

install.packages("tableone")

#load the package

library(tableone)

CreateTableOne(data = teachingratings)

my.vars=c("beauty","eval","age","students")

CreateTableOne(my.vars,data = teachingratings)

#is there systematic difference in teacher evaluations based on gender

CreateTableOne(my.vars, strata = "gender",data=teachingratings)

#analysis with caegorical variables

cat.vars=c("minority","tenure")

CreateTableOne(my.vars,strata = "tenure",factorVars = cat.vars,
               data=teachingratings)

CreateTableOne(my.vars,strata = "minority",factorVars = cat.vars,
               data=teachingratings)

#data visualization

#plot eval and beauty
plot(eval~beauty,
     main = "Relationship between teaching evaluation and beauty scores",
     xlab = "standardized beauty scores of instructors",
     ylab="teaching evaluation score",
     cex.main=0.75, cex.lab=0.9, cex.axis=0.8)
abline(lm(eval~beauty),col="red",lwd=2) #regression line (y~x)
lines(lowess(eval~beauty),col="blue",lwd=3) #lowess line (x~y)

#with lattice package

install.packages("lattice")
library(lattice)

attach(teachingratings)

xyplot(eval~beauty|gender, pch=19, col="dark grey",
       main= "Relationship between teaching evaluation score and beauty score",
       ylab = "Teaching evaluation score",
       xlab = "Standardized beauty score")

xyplot(eval~beauty|gender, pch=19, groups = tenure,
       main= "Relationship between teaching evaluation score and beauty score",
       ylab = "Teaching evaluation score",
       xlab = "Standardized beauty score")

install.packages("ggplot2")
library(ggplot2)

attach(teachingratings)
ggplot(teachingratings,aes(y=eval, x=beauty, color=tenure)) + geom_point()+
  labs(title="Relationship between teaching evaluation score and beauty score",
       y="Teaching evaluation score", x="Standardized beauty score") +
       theme(plot.title=element_text(size = 10, face = "bold"))
  
ggplot(teachingratings,aes(y=eval, x=beauty, color=tenure)) + geom_point()+
  facet_grid(~gender)+
  labs(title="Relationship between teaching evaluation score and beauty score",
       y="Teaching evaluation score", x="Standardized beauty score") +
  theme(plot.title=element_text(size = 10, face = "bold"))

ggplot(teachingratings,aes(y=eval, x=beauty, color=tenure)) + geom_point()+
  facet_grid(~gender)+
  labs(title="Relationship between teaching evaluation score and beauty score",
       y="Teaching evaluation score", x="Standardized beauty score") +
  geom_smooth(method="loess",size=1.0)+theme_bw()+
  theme(plot.title=element_text(size = 10, face = "bold"))

#with linear trend

ggplot(teachingratings,aes(y=eval, x=beauty, color=tenure)) + geom_point()+
  facet_grid(~gender)+
  labs(title="Relationship between teaching evaluation score and beauty score",
       y="Teaching evaluation score", x="Standardized beauty score") +
  geom_smooth(method="lm",size=1.0)+theme_bw()+
  theme(plot.title=element_text(size = 10, face = "bold"))


#box plots

p<- ggplot(teachingratings, aes(x=tenure, y=eval))+
  geom_boxplot()+
  labs(title="Relationship between teaching evaluation and tenure status",
       y="Teaching evaluation score", x="Tenure status")
p+theme(plot.title=element_text(size=10, face = "bold"))


# rotate the box plot

p+ coord_flip() + theme(plot.title=element_text(size = 10, face="bold"))


#notched box plot

ggplot(teachingratings, aes(x=tenure, y=eval, col=tenure))+
  geom_boxplot(notch = TRUE)+
  labs(title="Relationship between evaluation and tenure status",
       y="Teaching evaluation score", x="Tenure status")+
       theme(plot.title=element_text(size=10, face="bold"))

#plotting average teaching evaluation for males and females

tab.1<-tapply(eval,gender,mean)
tab.1

barplot(tab.1,
        main = "Evaluation scores by gender",
        xlab = "Gender of Instructor",
        ylab = "Teaching evaluation score",
        ylim = c(0,5),col = "Red")

#introducing the table command

table(gender)

table(gender,tenure)

#introducing proportions for tables

tab.2=table(gender,tenure)
tab.2


tab.r=prop.table(tab.2,1) #summing percentages by rows
tab.c=prop.table(tab.2,2) #summing percentages by columns

round(tab.r,2)

plot(tab.r,
     main="The percentage of tenured teachers based on gender")

#plotting one categorical variable

table(gender,tenure)

barplot(table(gender,tenure),
ylim=c(0,300),
xlab="tenure status",
ylab="number",
main="gender and tenure")

#statistical tests

tapply(eval,gender,mean)

t.test(eval~gender)

#regression

mod.1=lm(eval~gender)
summary(mod.1)

#all things being equal, male teachers receive higher teaching
#evaluation than female teachers by 0.16 and the coefficients
#is statistically significant (t>1.96)

# regression model 2

mod.2=lm(eval~gender+tenure+minority+age+beauty)
summary(mod.2)

#all else being equal, tenured professors receive lower teaching ratings
#than non-tenured professors by 0.19 and the coefficient is
#statistically significant

#all else being equal, tecahers with higher beauty score receive
#higher rating as compared to those with lower beauty score

install.packages("stargazer")
library(stargazer)

stargazer(mod.2,type="text",no.space=T)
