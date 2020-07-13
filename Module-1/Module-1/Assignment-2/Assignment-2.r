# To read the csv files
BoeingStock <- read.csv("BoeingStock.csv")
View(BoeingStock)

CocaColaStock <- read.csv("CocaColaStock.csv")
View(CocaColaStock)

GEStock <- read.csv("GEStock.csv")
View(GEStock)

IBMStock <- read.csv("IBMStock.csv")
View(IBMStock)

ProcterGambleStock <- read.csv("ProcterGambleStock.csv")
View(ProcterGambleStock)

# To convert the date to the Date Object
IBMStock$Date = as.Date(IBMStock$Date, "%m/%d/%y")
GEStock$Date = as.Date(GEStock$Date, "%m / %d/ %y")
CocaColaStock$Date = as.Date(CocaColaStock$Date, "%m / %d/ %y")
ProcterGambleStock$Date = as.Date(ProcterGambleStock$Date, "%m / %d/ %y")
BoeingStock$Date = as.Date(BoeingStock$Date, "%m / %d/ %y")

#Q1
str(BoeingStock)

#Q2.3
summary(BoeingStock)

#Q4
mean(IBMStock$StockPrice)

#Q5
min(GEStock$StockPrice)

#Q6
max(CocaColaStock$StockPrice)

#Q7
median(BoeingStock$StockPrice)

#Q8
sd(ProcterGambleStock$StockPrice)

plot(CocaColaStock$Date, CocaColaStock$StockPrice)
#Q9
CocaColaStock$Date[which.max(CocaColaStock$StockPrice)]

#Q10
CocaColaStock$Date[which.min(CocaColaStock$StockPrice)]

lines(ProcterGambleStock$Date, ProcterGambleStock$StockPrice)
plot(CocaColaStock$Date,CocaColaStock$StockPrice,col="red")
plot(CocaColaStock$Date,CocaColaStock$StockPrice,type="l",col="red")
lines(ProcterGambleStock$Date, ProcterGambleStock$StockPrice,lty=2,col="blue")
#Q11,13 is done based on the above plot and line graph
#Q12
abline(v=as.Date(c("1983-01-01")), lwd=2)


plot(CocaColaStock$Date[301:432], CocaColaStock$StockPrice[301:432], type="l ", col="red ", ylim=c(0,210))
lines(ProcterGambleStock$Date, ProcterGambleStock$StockPrice,lty=2,col="blue")
lines(GEStock$Date,GEStock$StockPrice,lty=3,col="green",lwd=2)
lines(IBMStock$Date,IBMStock$StockPrice,col="purple",lty=4)
lines(BoeingStock$Date,BoeingStock$StockPrice,col="orange",lty=5,lwd=3)
#Q14
abline(v=as.Date(c("2000-03-01")), lwd=2)

#Q15,17 is done based on the above plot and line graph

#Q16
abline(v=as.Date(c("1997-09-01")), lwd=2,col="steelblue")
abline(v=as.Date(c("1997-11-01")), lwd=2,col="steelblue")


tapply(GEStock$StockPrice,months(GEStock$Date),mean)
tapply(ProcterGambleStock$StockPrice,months(ProcterGambleStock$Date),mean)
tapply(BoeingStock$StockPrice,months(BoeingStock$Date),mean)
tapply(CocaColaStock$StockPrice,months(CocaColaStock$Date),mean)

#Q18
tapply(IBMStock$StockPrice,months(IBMStock$Date),mean)
tapply(IBMStock$StockPrice,months(IBMStock$Date),mean)>mean(IBMStock$StockPrice)


#Q19
which.max(tapply(GEStock$StockPrice,months(GEStock$Date),mean))
which.max(tapply(CocaColaStock$StockPrice,months(CocaColaStock$Date),mean))

#Q20
summary(CocaColaStock)

