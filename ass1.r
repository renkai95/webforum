install.packages("ggplot2")
library(ggplot2)


gdata <- read.csv("C:\\Users\\User\\Google Drive\\Monash units\\2019 Sem 1\\FIT3152\\Assignment 1\\webforum.csv")
Threads = aggregate(data.frame(count = gdata$ThreadID), list(value = gdata$ThreadID), length)
Authors = aggregate(data.frame(count = gdata$AuthorID), list(value = gdata$AuthorID), length)
with(gdata, plot(AuthorID,Tone , col =ThreadID ,pch=as.numeric(ThreadID), main = ("webforum data")))
with(gdata, plot(Date,ThreadID , col =filter(gdata$AuthorID,AuthorID>=0) ,pch=as.numeric(ThreadID), main = ("webforum data")))
