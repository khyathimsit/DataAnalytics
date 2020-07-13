songs = read.csv("songs.csv")

#Q1:
mj = subset(songs, artistname == "Michael Jackson")
nrow(mj)
(OR)
table(songs$artistname == "Michael Jackson")
#Ans: 18

#Q2:
cnt = subset(songs, year == 2010)
nrow(cnt)
(OR)
table(songs$year==2010)
#Ans: 373

#Q3:
subset(songs, songs$artistname == "Michael Jackson" & songs$Top10 == TRUE)
#Ans: You Are Not Alone, You Rock My World

#Q4:
unique(songs$timesignature)
#Ans: 3 4 5 7 1 0

#Q5:
table(songs$timesignature)
#Ans:
# 0    1    3    4    5    7 
# 10  143  503 6787  112   19

#Q6:
which.max(songs$tempo)
songs$songtitle[6206]
#Ans: Wanna Be Startin' Somethin'

#Q7,8:
songsTrain = subset(songs, songs$year <=2009)
songsTest = subset(songs, songs$year == 2010)
str(songsTrain)
#Ans: 7201

#Q9:
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
songsTrain = songsTrain[ , !(names(songsTrain) %in% nonvars) ]
songsTest = songsTest[ , !(names(songsTest) %in% nonvars) ]
Songsmodel1 = glm(Top10 ~ ., data=songsTrain, family=binomial)
summary(Songsmodel1)
#Ans: 4827.2(AIC)

#Q13:
cor(songsTrain$loudness, songsTrain$energy)
#Ans: 0.73991

#Q14:
Songsmodel2 = glm(Top10 ~ . - loudness, data=songsTrain, family=binomial)
summary(Songsmodel2)

#Q15:
Songsmodel3 = glm(Top10 ~ . - energy, data=songsTrain, family=binomial)
summary(Songsmodel3)

#Q16,17:
predict1 = predict(Songsmodel3, newdata = songsTest, type = "response")
table(songsTest$Top10, predict1 >= 0.45)

(309+19)/nrow(songsTest)
#Ans: 0.8793566

#Q19:
19/(19+40)
#Ans: 0.3220339

#Q20:
309/(309+5)
#Ans: 0.9840764