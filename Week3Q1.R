#Week 3 Homework

library(readr)
nutrition <- read_csv("/Users/salmansikandar/Documents/Regresion_Analysis/nutrition.csv")

y <- nutrition$woh
x <- nutrition$age

##1(a) 

#Plot the data
plot(x,y,ylab="Boy Weight/Height Ratio", xlab="Boy Age")

#overlay least squares line

abline(lm(y~x), col="red", lty=1, lwd=2)

#input 95% confidence interval
reg <- lm(y~x)
pi <- predict(reg, interval="prediction", level=0.95)
lines(sort(x), pi[order(x), "lwr"], lwd=2, col="blue", lty=1)
lines(sort(x), pi[order(x), "upr"], lwd=2, col="blue", lty=1)

legend("bottomright", col=c("red","blue"), lwd=2,lty=1, legend=c("LSE Line","95% Prediction"))

##1(b)

#Plot residuals
reg.res = resid(reg)
plot(x,reg.res, ylab="Residuals", xlab="Age") 
abline(0,0)

#Log Data Transformation
log.reg <- lm(log(y)~log(x))
plot(log(x),log(y), ylab="log(woh)", xlab="log(age)")

summary(log.reg)

log.pi <- predict(log.reg, interval="prediction", level=0.95)
lines(sort(log(x)), log.pi[order(x), "lwr"], lwd=2, col="blue", lty=1)
lines(sort(log(x)), log.pi[order(x), "upr"], lwd=2, col="blue", lty=1)

##1(c)

abline(log.reg)
log.reg.res = resid(log.reg)
plot(log(x),log.reg.res, ylab="Residuals", xlab="log(age)") 
abline(0,0)
