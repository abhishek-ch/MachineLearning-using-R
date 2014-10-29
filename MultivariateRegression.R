library(ggplot2)
library(GGally)
#refrerence
#http://ww2.coastal.edu/kingw/statistics/R-tutorials/multregr.html
data(state)
View(state.x77)
st = as.data.frame(state.x77)
#lets rename the column without space
colnames(st)[4] = "Life.Exp"         # no spaces in variable names, please
colnames(st)[6] = "HS.Grad"

cor(st) #relation with each other
plotmatrix(st[,1:2])
pairs(st)
ggpairs(st, alpha=0.4)


##
model1 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder +
                                      HS.Grad + Frost + Area , data=st)

##after this ,things will be very obvious as Population and Murder have influence over Life.Exp
#higher population increases the life expectancy and higher murder rates strongly opposed Life exp

########################################Basic Data Analysis#####################################
#so following are smooth plot with dataset
st[,9] = st$Income < 4000
colnames(st)[9] = "Extra"
qplot(Life.Exp, Population,color=factor(Extra), data=st, geom=c("smooth", "point"))
qplot(Life.Exp, Area,color=factor(Extra), data=st, geom=c("smooth", "point"))
qplot(Life.Exp, Murder,color=factor(Extra), data=st, geom=c("smooth", "point"))
qplot(Population, Murder,color=factor(Extra), data=st, geom=c("smooth", "point"))

##wel we can surely derive a pattern , so naturally its not that good data
res <- qplot(st$Area, resid(lm(Life.Exp ~ Area,data=st)))
res+geom_hline(yintercept=0)

##residual plot to see how good the data for murder, great spread of data , doing good
res <- qplot(st$Murder, resid(lm(Life.Exp ~ Murder,data=st)))
res+geom_hline(yintercept=0)

#better than Area
res <- qplot(st$Population, resid(lm(Life.Exp ~ Population,data=st)))
res+geom_hline(yintercept=0)


#Data is bit disperersed so I can find atleast some relation
res <- qplot(st$Murder, resid(lm(Population ~ Murder,data=st)))
res+geom_hline(yintercept=0)




###########################Minimizing Predictors################################
#logically we must work with only significant data and we will be exluding all less important
#predictors
 
#lets start with Area
model2 = update(model1,. ~.-Area) #removes area
summary(model2)
model2 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder +
              HS.Grad + Frost , data=st)



#lets remove Illiteracy
model3 = update(model2,. ~.-Illiteracy) #removes area
summary(model3)
model3 = lm(Life.Exp ~ Population + Income  + Murder +
              HS.Grad + Frost , data=st)

##comparision of 2 models
anova(model2,model3)

#try to find more differences above which can justify how much significance


#lets remove Murder and you can find a significant drop in P-Value
##so we have the answer that Murder is important
modelNull = update(model3,. ~.-Murder) #removes area
summary(modelNull)
modelNull = lm(Life.Exp ~ Population + Income   +
              HS.Grad + Frost , data=st)

##model 3 shows that income has least preference 
model4 = update(model3,. ~.-Income) #removes area
summary(model4)
model4 = lm(Life.Exp ~ Population   + Murder +
              HS.Grad + Frost , data=st)

#ok now HS. Grad got more attention or I must say more influence over the dataset
anova(model3,model4)


model5 = update(model4,. ~.-Population) #removes area
model5 = lm(Life.Exp ~ Murder +
              HS.Grad + Frost , data=st)
summary(model5)


##Now our model is in We have reached the minimal adequate model.


###########Plotting the smooth graph##########################

qplot(Life.Exp, Murder +
        HS.Grad + Frost, data = model5, geom = c("point", "smooth"),
      method = "lm")


qplot(Life.Exp,Population + Income + Illiteracy + Murder +
        HS.Grad + Frost, data = model1, geom = c("point", "smooth"),
      method = "lm")

##to find how good is the model or fit or validity of the model, run the residual plot
res <- qplot(fitted(model5), resid(model5))
res+geom_hline(yintercept=0)


##Isn't it obvious from above .. how good is model5 








######################Confidence Level############################################
#http://stattrek.com/statistics/dictionary.aspx?definition=Confidence%20interval
#http://www.r-tutor.com/elementary-statistics/simple-linear-regression/confidence-interval-linear-regression
confint(model5)
confint(model1,level=0.99)

newdata = data.frame(Murder=20.8, HS.Grad=80 , Frost=10)
predict(model5, newdata, interval="confidence") 

#So abobe output for confidence level says that Life.Exp will fall within the lwr and upr
#range having 95% confidence level
#So it can be used to predict the output

#lets try a value which crosses the boundary, which is not ideal with linear regression
newdata2 = data.frame(Murder=120.8, HS.Grad=180 , Frost=210)
predict(model5, newdata2, interval="confidence") 