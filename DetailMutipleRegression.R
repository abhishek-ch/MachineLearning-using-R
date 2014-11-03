#http://www.princeton.edu/~otorres/Regression101R.pdf
library(car)
library(ggplot2)
data(Prestige)
View(Prestige)

#Data analysis of 1 set of algorithm
model <- lm(prestige ~ education + log2(income)+women	,data=Prestige)
residualPlots(model, ~ 1, fitted=TRUE) #Residuals vs fitted
residualPlots(model, ~ education, fitted=TRUE) #Residuals vs education
res <- qplot(fitted(model), resid(model))
res+geom_hline(yintercept=0)
qplot(prestige ,education + log2(income)  +women + census, data = Prestige, geom = c("point", "smooth"),
      method = "lm")
#https://www.youtube.com/watch?v=unpTjhlLfzQ
avPlots(model, id.n=2, id.cex=0.7)


model2 <- lm(prestige ~ education*type +log2(income)*type, data = Prestige)
res <- qplot(fitted(model2), resid(model2))
res+geom_hline(yintercept=0)
qplot(prestige ,education*type +log2(income)*type, data = model2, geom = c("point", "smooth"),
      method = "lm")
residualPlots(model2, ~ 1, fitted=TRUE) #Residuals vs fitted
residualPlots(model2, ~ education , fitted=TRUE) #Residuals vs education
avPlots(model2, id.n=2, id.cex=0.7)
#read details about plot
#http://connectmv.com/tutorials/r-tutorial/investigating-outliers-discrepancies-and-other-influential-points/
#http://www.stat.columbia.edu/~martin/W2024/R7.pdf

qqPlot(model2$res,id.n=3)
hist(model2$res,freq=FALSE)
influenceIndexPlot(model2,id.n=3)
influencePlot(model2,id.n=3)