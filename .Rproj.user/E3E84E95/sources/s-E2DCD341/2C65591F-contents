install.packages("ggplot2")
install.packages("tidyverse")

library(tidyverse)

gdata <- read.csv("C:\\Users\\User\\Google Drive\\Monash units\\2019 Sem 1\\FIT3152\\Assignment 1\\webforum.csv")
Threads = aggregate(data.frame(count = gdata$ThreadID), list(value = gdata$ThreadID), length)
Authors = aggregate(data.frame(count = gdata$AuthorID), list(value = gdata$AuthorID), length)
with(gdata, plot(AuthorID,Tone , col =ThreadID ,pch=as.numeric(ThreadID), main = ("webforum data")))
with(gdata, plot(Date,money , col =filter(gdata$AuthorID,AuthorID>=0) , main = ("webforum data")))
#qplot(carat, price, data = gdata, color = color, size =clarity, alpha = cut)
dates <- as.Date(gdata$Date, "%Y-%m-%d")
data=gdata
data$year <- format(dates, "%Y")

test=aggregate(data[7:30], data[31], mean)


by(data,data$year, function (df) cor(df$Analytic,df$WC))
boxplot(money~year,data = data)
