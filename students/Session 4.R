library(nycflights13)
data("airquality")
head(airquality)
comp<-complete.cases(airquality)
mean(comp)
model<-lm(data = airquality,Ozone~Temp+Wind+Solar.R)
summary(model)

model1<-lm(data = airquality,log(Ozone)~log(Temp)+log(Wind)+log(Solar.R)) #1
summary(model1)

model2<-lm(data = airquality,Ozone~Temp+Wind)
summary(model4)

model3<-lm(data = airquality,log(Ozone)~log(Temp)+log(Wind))
summary(model5)


VIM::aggr(airquality)

model1<-lm(data = airquality,log(Ozone)~log(Temp)+log(Wind)) #1
summary(model1)

library(ggplot2)

ggplot(airquality,aes(x = log(Ozone),y = log(Temp))) + geom_point() + geom_smooth()
