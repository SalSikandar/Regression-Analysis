##2(a) Plot the data

par(mfrow=c(1,3))
plot(beef$SIZE,beef$YES, ylab="Yes", xlab="Size") 
plot(beef$VAL,beef$YES, ylab="Yes", xlab="Val") 
plot(beef$SIZE,beef$VAL, ylab="Val", xlab="Size") 

##2(a) Fit a regression 

beef.reg <- lm(beef$YES~beef$SIZE+beef$VAL)
summary(beef.reg)

##2(b) Fit SIZE change depending on log(VAL)

beef.reg2 <- lm(beef$SIZE~log(beef$VAL))
summary(beef.reg2)
plot(log(beef$VAL),beef$SIZE, ylab="Size", xlab="Log(Val)")
