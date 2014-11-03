library(ggplot2)
library(splines)
library(reshape2)


if(Sys.info()["user"] == 'achoudhary'){
  data = read.csv("D:/Work/RWorkSpace/icici.csv")
}else{
  
  data = read.csv("/Users/abhishekchoudhary/Google Drive/BIG/FinancialAnalyst/icici.csv")
}

qplot(Deposit,data=data,geom="histogram")


sum(data$Deposit,na.rm = TRUE)
sum(data$Withdrawal,na.rm = TRUE)

#length gives item with PAYMENT  in CLeanse column
length(which(data$CLEANSE=="PAYMENT"))
aggregate( cbind( Deposit , Withdrawal ) ~ CLEANSE=="PAYMENT" , data = data , FUN = sum )


aggregate( Deposit ~ (CLEANSE=="PAYMENT"), data = data, sum)
aggregate( Deposit ~ (CLEANSE=="BHOPAL"), data = data, sum)
aggregate( Deposit ~ (CLEANSE=="IRCTC"), data = data, sum)


#1st Column is same as 2nd, so can skip any of that
cdata = update(data,. ~.-Value.Date) #removes Value.Date
cdata <- data[,!(names(data) %in% "Value.Date")]

#So I withdrawn so much money from my account .. Gosh
aggregate( Withdrawal ~ (CLEANSE=="ATM/CASH"), data = cdata, sum)

#Split the data Month wise
datef <- as.Date(cdata$Transaction.Date, format = "%d/%m/%y")
mo <- strftime(datef, "%m")

##month wise deposit and withdrawl to feel embarras
month.dep <- aggregate(Deposit ~ mo , cdata, FUN = sum)
month.with <- aggregate(Withdrawal ~ mo , cdata, FUN = sum)

#adding the column 
month.dep["Withdrawl"] <- month.with$Withdrawal

merge(month.dep,month.with,by.x=c("mo","Deposit"),by.y=c("Withdrawal"))

ggplot(data=month.dep,aes(x=mo,y=Deposit,colour=mo))+geom_line()+geom_point()
qplot(mo, Deposit,data=month.dep, geom="bar", stat="identity", fill=as.factor(Deposit))

#WHY DID I use melt
#In general, ggplot2 prefers data in long format. In this case, Deposit and Withdrawal 
#are two categories of currency values. So we use melt to put those two categories 
#into a single column called variable that ggplot uses for the colour aesthetic, 
#while the currency values likewise go into a new value column, each value going with 
#its corresponding category in the variable column
month.dep.m = melt(month.dep, id.var="mo")
ggplot(month.dep.m, aes(x=mo, y=value, colour=variable))+ylab("Transactions") +xlab("Month-Wise") +geom_point(aes(size=4.5),shape=21)+
  geom_line(aes(group=variable),size=1)


##############Playing With Prediction#################################
#Now I wanted to know when I am going to find withdraw for specific deposit

#Now lets play with the model

View(cdata)
datef <- as.Date(cdata$Transaction.Date, format = "%d/%m/%y")
model1 <- lm(Withdrawal ~ datef	
             +Deposit	+Balance,data=cdata)

step(model1)
#found the required dataset which actually matters
model2 <- update(model1 ,~.-datef )
model2 <- lm(Withdrawal ~ Deposit	+Balance,data = cdata)


##This plot says poorest kind of data spread so ideally data is not
#fit for prediction , but even I poked my nose to find something
#Something is better than nothing
qplot(Withdrawal, Deposit  +Balance, data = model2, geom = c("point", "smooth"),
      method = "lm")

qplot(Withdrawal, Deposit  +Balance, data = model2, geom = c("point", "smooth"),
      method = "lm",formula = y ~ ns(x, 3))


###Lets do the test actually
noNadata <- cdata[complete.cases(cdata),]
model_smooth <- smooth.spline(noNadata$Withdrawal~ noNadata$Deposit+noNadata$Balance,spar=0.75,nknots=30)

#y.loess <- loess(y ~ x, span=0.75, data.frame(x=noNadata$Withdrawal, y=noNadata$Deposit+noNadata$Balance))


newplotdata <- data.frame(Deposit=1800,Balance=351893)
predict(model_smooth, newplotdata) 
predict(model2, newplotdata) 

#to find the normal distribution of data
qqPlot(model2,id.n=3)
hist(model2$res)
