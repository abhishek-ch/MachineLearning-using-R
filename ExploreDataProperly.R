library(Quandl)
library(ggplot2)
library(xts)

library(car)
library(GGally)
library(splines)
df <- read.csv("https://www.quandl.com/api/v1/datasets/FRED/GDP.csv")


quandldata = Quandl("NSE/OIL", collapse="monthly", start_date="2011-01-01", authcode="xxhAhJr9JLtYzgjoK4J1",type="xts")
plot(quandldata[,1])

colnames(quandldata)[7] <- "Turnover"
colnames(quandldata)[6] <- "Total"
View(quandldata)
write.csv(quandldata, file = "test.csv")


datefield = index(quandldata)


##########################################################
library(Quandl)
library(ggplot2)
library(xts)

library(car)
library(GGally)
library(splines)
df <- read.csv("test.csv")
View(df)
model = lm(Total~I(Open ** 2.0)+High+Last, 
           data = df)
summary(model)

model1 = lm(High~Open+Low+Close, 
            data = df)

#to find how many variables suit the best
step(model1, direction="backward")
#Plot the dataset and check how the smooth linear regression works
#that looks fantastic
qplot(High , Open+Low +Close, 
      data = model1, geom = c("point", "smooth"),
      method = "lm")
##Now lets verify the data normalization using residual plot

res <- qplot(fitted(model1), resid(model1))
res+geom_hline(yintercept=0)

#compare relation between each of the variable
avPlots(model1, id.n=2, id.cex=0.7)
#to find the normal distribution of data
qqPlot(model1,id.n=3)


#Above all shouting that I am in right track but yeah data is not that disperesed
main <- model1

######Native Bayes############################


