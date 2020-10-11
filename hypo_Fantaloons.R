Fantaloons<-read.csv(file.choose())
View(Fantaloons)
attach(Fantaloons)

Weekdays<-ifelse(Fantaloons$Weekdays=="Male",1,0)
Weekend<-ifelse(Weekend=="Male",1,0) #Fantaloons$Weekend
Weekdays_Weekend<-cbind.data.frame(Weekdays,Weekend)
View(Weekdays_Weekend)
str(Weekdays_Weekend)
stacked_Weekdays_Weekend <- stack(Weekdays_Weekend)
attach(stacked_Weekdays_Weekend)
View(stacked_Weekdays_Weekend)
table(stacked_Weekdays_Weekend)
#chisq test
chisq.test(table(stacked_Weekdays_Weekend))
# since the male versus females walking into the store differ based on the days of week