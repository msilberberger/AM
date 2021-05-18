library(stargazer)

###   Loading Data  ####


data(airquality)
head(airquality)
summary(airquality)
comp <- complete.cases(airquality)
mean(comp)

####   Regression Models    #####

#Made linear Models of each variable for the independent variable Ozone

RegT <- lm( Ozone ~ Temp, data=airquality)
RegW<- lm( Ozone ~ Wind, data=airquality)
RegS <- lm( Ozone ~ Solar.R, data=airquality)

#Linear Model with all dependent variables, sans month & day

Reg1 <- lm( Ozone ~ Temp+Wind+Solar.R, data=airquality)

summary(RegT)
summary(RegW)
summary(RegS)
summary(Reg1)

stargazer(list(RegT,RegW,RegS,Reg1),type="text",omit.stat=c("f", "ser"))

#New Model reomving Solar.R due to numerous missing observations

Reg2 <- lm( Ozone ~ Temp+Wind, data=airquality)
summary(Reg2)

#Pretty table to view linear models side-by-side
stargazer(list(Reg1,Reg2), type = "text", omit.stat = c("f","ser"))
