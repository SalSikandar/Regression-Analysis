##Final Project

##Histograms

data = read.csv("C:\\Users\\80215492\\Desktop\\data_Pol.csv", header=T)
summary (data)

##Visualize the Data with BoxPlots
boxplot(data$pm2.5 ~ data$year, ylab="Pollution", main="Year", cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
##None

par(mfrow=c(3,3))
boxplot(data$pm2.5 ~ data$month, ylab="Pollution", main="Month", cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
boxplot(data$pm2.5 ~ data$hour, ylab="Pollution", main="Hour", cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
##Maybe slight dip during the middle of the day.
boxplot(data$pm2.5 ~ data$Iws, ylab="Pollution", main="Cul. Wind Speed", cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
##Good linear relationship b/t wind speed and pollution
boxplot(data$pm2.5 ~ data$Ir, ylab="Pollution", main="Hour of Rain", cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
##Look at this one as well
##Can't log rain since a lot of values are zero. Log of zero is infinity.
boxplot(data$pm2.5 ~ data$Is, ylab="Pollution", main="Hour of Snow", cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
##Little support on this one??
boxplot(data$pm2.5 ~ data$cbwd, ylab="Pollution", main="Wind Dir", cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
##Sound more than north??
##Change between north and not north wind
boxplot(data$pm2.5 ~ data$PRES, ylab="Pollution", main="Pressure", cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
##something here. Middle pressures are highest Pollution rates
boxplot(data$pm2.5 ~ data$TEMP, ylab="Pollution", main="Temperature", cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
##Might be something but might not be.
boxplot(data$pm2.5 ~ data$DEWP, ylab="Pollution", main="Dew Point", cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
##Something here for sure.

##Other Plots
plot(lm(log(data$pm2.5),log(data$Iws)))
abline(lm(log(data$pm2.5)~log(data$Iws)),col=4)


##Change Data
data2 = read.csv("C:\\Users\\80215492\\Desktop\\data_Pol2.csv", header=T)
par(mfrow=c(2,1))
plot(data2$hour,data2$pm2.5)

##Training Data
##This is a time-series data series, so we will be using the first three years and 9 months for our training data and the last 3 months as our test data.
print(acf(data2$pm2.5))
pollution=  lm(data$pm2.5[2:39594] ~ data$pm2.5[1:39593])
summary(pollution)
##Ends up being a random walk model
polresiduals=  lm(data$pm2.5[2:41758] ~ data$pm2.5[1:41757])
acf(polresiduals$residuals)


##Training Data without using the time consideration
set.seed(2)
train.samples = sample.int(nrow(data), 0.7*nrow(data))
train = data[train.samples,]
test = data[-train.samples,]
full = lm(pm2.5 ~ ., data=train)
empty = lm(pm2.5 ~ 1, data=train)
full2= lm(pm2.5~.+.^2,data=train)

n=nrow(train)
fwdBIC <- step(empty, scope=formula(full), direction="forward", k=log(n))
summary(fwdBIC)
fwdBIC2 <- step(empty, scope=formula(full2), direction="forward", k=log(n))
summary(fwdBIC2)
backBIC=step(full2,direction="backward", k=log(n))
summary(backBIC)


