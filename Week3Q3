##3

library(MASS)
data(UScrime)

?UScrime

##3(a)
 
crime.reg <- lm(UScrime$y ~ UScrime$Ed + UScrime$LF + UScrime$GDP)
summary(crime.reg)

##3(b)

crime.reg2 <- lm(UScrime$y ~ UScrime$Ed)
summary(crime.reg2)

crime.reg3 <- lm(UScrime$y ~ UScrime$LF)
summary(crime.reg3)

crime.reg4 <- lm(UScrime$y ~ UScrime$GDP)
summary(crime.reg4)
