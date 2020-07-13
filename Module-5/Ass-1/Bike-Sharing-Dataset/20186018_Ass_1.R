#Question1

str(BSDH)
nrow(BSDH)

#Question2

names(BSDH)

#Question3

max(BSDH$temp) - min(BSDH$temp)

#Question4

nrow(subset(BSDH, BSDH$workingday == 1))

#Question5

nrow(subset(BSDH, season == 1))

#Question6

summary(BSDH)

#Question7

BSDH$tdeday = DateConvert
sub = subset(BSDH, DateConvert >= "2012-05-18" & DateConvert <= "2012-05-21")
sum(sub$cnt)

#Question8

head(sort(tapply(BSDH$cnt, BSDH$dteday, mean)))

#Question9

sub2011 = subset(BSDH, BSDH$yr == 0)
sort( tapply(sub2011$cnt, sub2011$weekday, mean))

#Question10

boxplot(BSDH$cnt ~ BSDH$yr)

#Question11

meanval = mean(BSDH$cnt)
sub2012 = subset(BSDH, BSDH$yr == 1)
great = subset(sub2012, sub2012$cnt > meanval)
counter = table(great$hr)
sum(counter)/sum(BSDH$hr)

#Question12

sub20120to8 = subset(sub2012, sub2012$hr < 8 & sub2012$hr >=0)
mean(sub20120to8$cnt)
sub20128to18 = subset(sub2012, sub2012$hr < 18 & sub2012$hr >=8)
mean(sub20128to18$cnt)sub201218to23 = subset(sub2012, sub2012$hr <= 23 & sub2012$hr >18)
mean(sub201218to23$cnt)

