install.packages("ggplot2")
install.packages("tidyverse")

library(tidyverse)

gdata <- read.csv("C:\\Users\\User\\Google Drive\\Monash units\\2019 Sem 1\\FIT3152\\Assignment 1\\webforum.csv")
Threads = aggregate(data.frame(count = gdata$ThreadID), list(value = gdata$ThreadID), length)
Authors = aggregate(data.frame(count = gdata$AuthorID), list(value = gdata$AuthorID), length)
with(gdata, plot(AuthorID,Tone , col =ThreadID ,pch=as.numeric(ThreadID), main = ("webforum data")))
with(gdata, plot(Date,Tone , col =filter(gdata$AuthorID,AuthorID>=0) ,pch=as.numeric(AuthorID), main = ("webforum data")))
