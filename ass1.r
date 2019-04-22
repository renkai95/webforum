install.packages("ggplot2")
install.packages("tidyverse")

library(tidyverse)

gdata <- read.csv("C:\\Users\\User\\Google Drive\\Monash units\\2019 Sem 1\\FIT3152\\Assignment 1\\webforum.csv")
Threads = aggregate(data.frame(count = gdata$ThreadID), list(value = gdata$ThreadID), length)
Authors = aggregate(data.frame(count = gdata$AuthorID), list(value = gdata$AuthorID), length)
#with(gdata, plot(AuthorID,Tone , col =ThreadID ,pch=as.numeric(ThreadID), main = ("webforum data")))
#with(gdata, plot(Date,money , col =head(sort(filter(gdata$AuthorID,AuthorID>=0),decreasing=TRUE),3) , main = ("webforum data")))
#qplot(carat, price, data = gdata, color = color, size =clarity, alpha = cut)
dates <- as.Date(gdata$Date, "%Y-%m-%d")
data=gdata
data$year <- format(dates, "%Y")

test=aggregate(data[7:30], data[31], mean)

#newdata = head(gdata[order(length(gdata$AuthorID)),],30)
newdata = data %>% 
  group_by(AuthorID) %>%
  filter(n()>=100)

newdata = filter(newdata,AuthorID>=0)
#qplot(Date,Tone,data=newdata,color = factor(AuthorID))
#qplot(year,money,data=newdata,color = factor(AuthorID))

g = ggplot(data = newdata) + geom_point(mapping= aes (x = Date,y = WC,color = factor(ThreadID)))
g

newdata1 = data %>% 
  group_by(ThreadID) %>%
  filter(n()>=100)

by(newdata, data$AuthorID, function(df) summary(df) )

by(data,data$year, function (df) cor(df$Analytic,df$WC))
boxplot(money~year,data = data)


