install.packages("ggplot2")
library(ggplot2)


gdata <- read.csv("C:\\Users\\User\\Google Drive\\Monash units\\2019 Sem 1\\FIT3152\\Assignment 1\\webforum.csv")
with(gdata, plot(Date,Tone , col = ThreadID,pch=as.numeric(ThreadID), main = ("webforum data"), xlab ="Data", ylab =("WC")))
