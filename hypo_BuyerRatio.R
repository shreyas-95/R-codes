setwd("C:/Users/Anupama Raghavendra/Desktop/ExcelR/ASSIGNMENTS/7.Hypothesis Testing")


library(readxl)
library(xlsx) 
library(openxlsx)
library(WriteXLS)

getwd()
BuyerRatio<-read.csv("BuyerRatio.csv")
View(BuyerRatio)
library(dplyr)
BR1<-select(BuyerRatio,-1)
View(BR1)
row.names(BR1) <- c("male","female")
#Apply chisq test
chisq.test(BR1) 
#X-squared = 1.5959, df = 3, p-value = 0.6603