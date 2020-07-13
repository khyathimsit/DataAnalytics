baseball = read.csv("baseball.csv")

#Q 1.1:
str(baseball)
#Ans: 1232

#Q 1.2:
length(unique(baseball$Year))
(OR)
length(table(baseball$Year))
#Ans: 47

#Q 1.3
baseball = subset(baseball, Playoffs==1)
str(baseball)
#Ans: 244

#Q 1.4
unique(table(baseball$Year))
#Ans: 2,4,8,10

#Q 2.1
PlayoffTable = table(baseball$Year)
PlayoffTable
names(PlayoffTable)

#Q 2.2
PlayoffTable[c("1990", "2001")]

#Q 2.3
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

#Q 2.4
table(baseball$NumCompetitors)
#Ans: 128

#Q3.1
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)
#Ans: 197

#Q 3.2
summary(glm(WorldSeries~Year, data=baseball, family="binomial"))
summary(glm(WorldSeries~RA , data=baseball, family="binomial"))
summary(glm(WorldSeries~NumCompetitors  , data=baseball, family="binomial"))