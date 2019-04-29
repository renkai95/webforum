install.packages("ggplot2")
install.packages("tidyverse")
install.packages("psych")
install.packages("dplyr")
install.packages("lubridate")
setwd("C:\\Users\\User\\Google Drive\\Monash units\\2019 Sem 1\\FIT3152\\Assignment 1\\webforum")
library(lubridate)
library(tidyverse)
library(psych)
library(dplyr)
gdata <- read.csv("C:\\Users\\User\\Google Drive\\Monash units\\2019 Sem 1\\FIT3152\\Assignment 1\\webforum.csv")
Threads = aggregate(data.frame(count = gdata$ThreadID), list(value = gdata$ThreadID), length)
Authors = aggregate(data.frame(count = gdata$AuthorID), list(value = gdata$AuthorID), length)
AuthorsNamed = Authors [2:nrow(Authors),]
#with(gdata, plot(AuthorID,Tone , col =ThreadID ,pch=as.numeric(ThreadID), main = ("webforum data")))
#with(gdata, plot(Date,money , col =head(sort(filter(gdata$AuthorID,AuthorID>=0),decreasing=TRUE),3) , main = ("webforum data")))
#qplot(carat, price, data = gdata, color = color, size =clarity, alpha = cut)
dates <- as.Date(gdata$Date, "%Y-%m-%d")
data=gdata
y= as.POSIXlt(data$Time,format="%H:%M")
data$year <- as.numeric(format(dates, "%Y"))
x=hour(y)

data$timehour=x
test=aggregate(data[7:30], data[31], mean)

#newdata = head(gdata[order(length(gdata$AuthorID)),],30)
newdata = data %>% 
  group_by(AuthorID) %>%
  filter(n()>=97)
threadcount = data %>%
  group_by(AuthorID,ThreadID) %>%
  summarize(n())
AuthorIDCount = threadcount %>%
  group_by(AuthorID) %>%
  summarize(n())
AuthorIDCount= cbind(AuthorIDCount,aggregate(threadcount[3],threadcount[1],sum))
AuthorIDCount = AuthorIDCount[,c(1,2,4)]
names(AuthorIDCount) [3] = "postcount"
names(AuthorIDCount)[2] = "threadcount"
AuthorIDCount = AuthorIDCount[2:nrow(AuthorIDCount),]
sociallyconnected= filter(AuthorIDCount,threadcount>36)

newdata2 = data %>% 
  group_by(AuthorID) %>%
  filter(n()<97)
anons = data %>%
  group_by(AuthorID) %>%
  filter(AuthorID<0)
newdata = filter(newdata,AuthorID>=0)

newdatamean=aggregate(newdata[,6:30],list(newdata$timehour),mean)
less100mean=aggregate(newdata2[,6:30],list(newdata2$timehour),mean)
newdatameanyear=aggregate(newdata[,6:30],list(newdata$year),mean)
less100meanyear=aggregate(newdata2[,6:30],list(newdata2$year),mean)
#qplot(Date,Tone,data=newdata,color = factor(AuthorID))
#qplot(year,money,data=newdata,color = factor(AuthorID))
plot(density(AuthorsNamed[,2]),main="Distribution of posts")
plot(density(newdata$timehour),main="Distribution of posts")
lines(density(newdata2$timehour),main="Distribution of posts")

normGraph = ggplot() + geom_density(data =newdata,mapping = aes(x = timehour,color="top posters n = 4963"))+geom_density(data =newdata2,mapping = aes(x = timehour,color="other posters n = 13929")) + ggtitle("Distribution of posts by hour")
normGraph
normGraphYear = ggplot() + geom_density(data =newdata,mapping = aes(x = year,color="top posters n = 4963"))+geom_density(data =newdata2,mapping = aes(x = year,color="other posters n = 13929")) + ggtitle("Distribution of posts by year") 
normGraphYear
normGraphthreads = ggplot() + geom_density(data =AuthorIDCount,mapping = aes(x = threadcount,color="dist")) + ggtitle("Total number of threads participated in")
normGraphthreads
qplot(threadcount,postcount,data = AuthorIDCount)
quantile(AuthorIDCount$threadcount, probs = c(0, 0.25, 0.5, 0.75, 0.90,0.95,0.99,1))
cor(AuthorIDCount$threadcount,AuthorIDCount$postcount)
g = ggplot(data = newdata) + geom_point(mapping= aes (x = year,y = affect,color = factor(AuthorID))) +  facet_wrap(~ThreadID)+ theme(legend.position = "none")
g

newdata1 = data %>% 
  group_by(ThreadID) %>%
  filter(n()>=300)
  
g1 = ggplot(data = newdata1) + geom_point(mapping= aes (x = Date,y = AuthorID,color = factor(ThreadID)))
g1


g2  =  ggplot(data  = data) + geom_point(mapping = aes ( x = timehour , y = posemo),color = "blue",position = position_jitter(w = 0.1, h = 0))
g2  = g2+ geom_point(mapping = aes ( x = timehour , y = negemo),color = "red") + ylab("posemo(blue) and negemo(red)")
g2

g3  =  ggplot() + geom_line(data  = newdatamean,mapping = aes ( x = as.numeric(row.names(newdatamean)) , y = anger),color = "blue") +ggtitle("Mean anger(blue) and affect(red) for users with >97 posts")
g3  = g3+ geom_line(data  = newdatamean,mapping = aes (x= as.numeric(row.names(newdatamean)), y = affect),color = "red") + ylab("anger/sentiment")+xlab("hour") + scale_x_continuous(limits = c(0, 23))+ scale_x_continuous("hour", labels = as.numeric(row.names(newdatamean)), breaks = as.numeric(row.names(newdatamean)))
g3
g3  =  ggplot() + geom_line(data  = less100mean,mapping = aes ( x = as.numeric(row.names(newdatamean)) , y = anger),color = "blue") +ggtitle("Mean anger(blue) and affect(red) for users with <97 posts")
g3  = g3+ geom_line(data  = less100mean,mapping = aes (x= as.numeric(row.names(newdatamean)), y = affect),color = "red") + ylab("anger/sentiment")+xlab("hour") + scale_x_continuous(limits = c(0, 23))+ scale_x_continuous("hour", labels = as.numeric(row.names(newdatamean)), breaks = as.numeric(row.names(newdatamean)))
g3
g3  =  ggplot() + geom_line(data  = newdatamean,mapping = aes ( x = as.numeric(row.names(newdatamean)) , y = negemo),color = "blue",position = position_jitter(w = 0.1, h = 0)) +ggtitle("Mean Negative Emotions for active posters(blue) and regular posters (red)")
g3  = g3+ geom_line(data  = less100mean,mapping = aes (x= as.numeric(row.names(newdatamean)), y = negemo),color = "red") + ylab("posemo")+xlab("hour") + scale_x_continuous(limits = c(0, 23))+ scale_x_continuous("hour", labels = as.numeric(row.names(newdatamean)), breaks = as.numeric(row.names(newdatamean)))
g3
g3  =  ggplot(data  = newdatamean) + geom_line(mapping = aes ( x = as.numeric(row.names(newdatamean)) , y = posemo),color = "blue") +ggtitle("Mean Positive Emotions for posters with >97 posts")
g3  = g3 + scale_x_continuous(limits = c(0, 23))+ scale_x_continuous("hour", labels = as.numeric(row.names(newdatamean)), breaks = as.numeric(row.names(newdatamean)))
g3

g3  =  ggplot(data  = less100mean) + geom_line(mapping = aes ( x = as.numeric(row.names(less100mean)) , y = posemo),color = "blue",position = position_jitter(w = 0.1, h = 0))
g3  = g3+ geom_line(mapping = aes (x= as.numeric(row.names(less100mean)), y = negemo),color = "red") + ylab("posemo(blue) and negemo(red)")+xlab("hour") + scale_x_continuous(limits = c(0, 23))+ scale_x_continuous("hour", labels = as.numeric(row.names(less100mean)), breaks = as.numeric(row.names(less100mean)))
g3
g3  =  ggplot(data  = less100mean) + geom_line(mapping = aes ( x = as.numeric(row.names(less100mean)) , y = posemo),color = "blue")+ggtitle("Mean Positive Emotions for posters with <100 posts")
g3  = g3 + scale_x_continuous(limits = c(0, 23))+ scale_x_continuous("hour", labels = as.numeric(row.names(less100mean)), breaks = as.numeric(row.names(less100mean)))
g3

g4  =  ggplot() + geom_line(data  = newdatameanyear,mapping = aes ( x = newdatameanyear[,1] , y = negemo),color = "blue") +ggtitle("Mean Positive Emotions for active posters(blue) and regular posters (red)")
g4  = g4+ geom_line(data  = less100meanyear,mapping = aes (x= newdatameanyear[,1], y = negemo),color = "red") + ylab("posemo")+xlab("hour")# +  scale_x_continuous("hour", labels = newdatameanyear, breaks = as.numeric(row.names(newdatameanyear)))
g4


g5 = qplot(posemo,timehour,newdatamean)
g5
#temp = group_by(newdata,AuthorID)
#s = summarize(temp,count=n(),)
capture.output(s, file = "myfile.txt")


by(newdata, data$AuthorID, function(df) summary(df) )

by(data,data$year, function (df) cor(df$Analytic,df$WC))
boxplot(money~year,data = data)

t.test(data$anger,anons$anger,alternative="great")

s = mapply(t.test, x= data[,6:30], y = newdata[,6:30], SIMPLIFY = F)
capture.output(s, file = "population-topposters.txt")
s = mapply(t.test, x= data[,6:30], y = anons[,6:30], SIMPLIFY = F)
capture.output(s, file = "population-anon.txt")
cor(newdatamean$posemo,less100mean$posemo)
