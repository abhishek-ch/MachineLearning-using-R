library(ggplot2)
library(splines)


if(Sys.info()["user"] == 'achoudhary'){
  data = read.csv("D:/Work/RWorkSpace/icici.csv")
}else{
  data = read.csv("/Users/abhishekchoudhary/MachineLearning/icici.csv")
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
