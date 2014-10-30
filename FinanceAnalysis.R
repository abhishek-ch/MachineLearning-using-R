library(ggplot2)
library(splines)


if(Sys.info()["user"] == 'achoudhary'){
  data = read.csv("D:/Work/RWorkSpace/icici.csv")
}else{
  
  data = read.csv("/Users/abhishekchoudhary/Google Drive/BIG/FinancialAnalyst/icici.csv")
}

qplot(Deposit,data=data,geom="histogram")
qplot(population,data=data,geom="histogram")


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

variable <- c(month.dep$Deposit,month.with$Withdrawal)
