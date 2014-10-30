#############Regression in India Population####################################

#http://www.r-bloggers.com/thats-smooth/
#http://people.stat.sfu.ca/~cschwarz/Consulting/Trinity/Phase2/TrinityWorkshop/Workshop-handouts/TW-04-Intro-splines.pdf
#http://ggplot2.org/book/qplot.pdf

library(ggplot2)
library(caret)
library(splines)
library(mgcv)


#The sequence of column in CSV does matter the smoot_spline method to work properly
if(Sys.info()["user"] == 'achoudhary'){
  data = read.csv("D:/Work/RWorkSpace/Git/pop.csv")
}else{
  data = read.csv("/Users/abhishekchoudhary/MachineLearning/pop.csv")
}
str(data) # it says in a data frame
View(data)  #check the data in tabular form

qplot(year,population,color=year,data=data)


model_plot = lm(year~population,data=data)
summary(model_plot)

#it leads to non linear plot for our data
qplot(year,population,color=year,data=data,geom=c("point","smooth"), span=1)

#Histogram and Density plot
qplot(population,data=data,geom="histogram")
qplot(population,data=data,geom="density")


##follwing lm is meant for straight line , but our data doesn't suit that
qplot(year,population,color=year,data=model_plot,geom=c("point","smooth"), method="lm") 

#so spline package will help here
dat <- gamSim(1,n=150,scale=2)



#model_loess <- loess(formula = data$year ~ data$population, data = data, span = 0.5,
#      degree = 1)
#qplot(year,population,color=year,data=model_loess,geom=c("point","smooth"), method="lm")



#removing all NA from data
noNadata <- data[complete.cases(data),]

#READ ABOUT KNOTS
#http://stats.stackexchange.com/questions/7316/setting-knots-in-natural-cubic-splines-in-r
#I increased it to 50 , because here finally I found an expected output properly
model_smooth <- smooth.spline(noNadata$population~noNadata$year,nknots=50)


qplot(year,population,color=year,data=model_smooth,geom=c("point","smooth"), span=1)
##Now we can check the confidence level
newplotdata <- data.frame(year=2090)
predict(model_smooth, newplotdata) 

