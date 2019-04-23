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
#with(gdata, plot(AuthorID,Tone , col =ThreadID ,pch=as.numeric(ThreadID), main = ("webforum data")))
#with(gdata, plot(Date,money , col =head(sort(filter(gdata$AuthorID,AuthorID>=0),decreasing=TRUE),3) , main = ("webforum data")))
#qplot(carat, price, data = gdata, color = color, size =clarity, alpha = cut)
dates <- as.Date(gdata$Date, "%Y-%m-%d")
data=gdata
y= as.POSIXlt(data$Time,format="%H:%M")
data$year <- format(dates, "%Y")
x=hour(y)

data$timehour=x
test=aggregate(data[7:30], data[31], mean)

#newdata = head(gdata[order(length(gdata$AuthorID)),],30)
newdata = data %>% 
  group_by(AuthorID) %>%
  filter(n()>=100)
anons = data %>%
  group_by(AuthorID) %>%
  filter(AuthorID<0)
newdata = filter(newdata,AuthorID>=0)
#qplot(Date,Tone,data=newdata,color = factor(AuthorID))
#qplot(year,money,data=newdata,color = factor(AuthorID))

g = ggplot(data = newdata) + geom_point(mapping= aes (x = year,y = affect,color = factor(ThreadID))) +  facet_wrap(~AuthorID)+ theme(legend.position = "none")
g

newdata1 = data %>% 
  group_by(ThreadID) %>%
  filter(n()>=300)
  
g1 = ggplot(data = newdata1) + geom_point(mapping= aes (x = Date,y = AuthorID,color = factor(ThreadID)))
g1


g2  =  ggplot(data  = anons) + geom_point(mapping = aes ( x = timehour , y = posemo),color = "red",position = position_jitter(w = 0.1, h = 0))
g2  = g2+ geom_point(mapping = aes ( x = timehour , y = negemo),color = "blue")
g2

#temp = group_by(newdata,AuthorID)
#s = summarize(temp,count=n(),)
capture.output(s, file = "myfile.txt")


by(newdata, data$AuthorID, function(df) summary(df) )

by(data,data$year, function (df) cor(df$Analytic,df$WC))
boxplot(money~year,data = data)

t.test(data$anger,anons$anger,alternative="great")

t.test_results <- mapply(t.test, x= data[,6:30], y = newdata[,6:30], SIMPLIFY = F)
      