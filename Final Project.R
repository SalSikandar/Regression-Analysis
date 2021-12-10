##Final Project

##Histograms

data = read.csv("/Users/salmansikandar/Documents/Regresion_Analysis/project/Data_Pol.csv", header=T)
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
nrow(data)
set.seed(2)
train.samples = sample.int(nrow(data), 0.7*nrow(data))
train = data[train.samples,]
test = data[-train.samples,]

null = lm(log(pm2.5) ~ 1,data=train)
full = lm(log(pm2.5) ~ ., data=train)
full2= lm(log(pm2.5)~.+.^2,data=train)

n=nrow(train)
n


# Forward BIC with: null -> full
fwdBICn2f <- step(null, scope=formula(full), direction="forward", k=log(n))
summary(fwdBICn2f)

# Forward BIC with: null -> full2
fwdBICn2f2 <- step(null, scope=formula(full2), direction="forward", k=log(n))
summary(fwdBICn2f2)

# Backwards BIC from full to null
backBICf2n <- step(full, direction="backward", k=log(n))
summary(backBICf2n)

#Backwards BIC from full2 to null
backBICf22n <- step(full2, direction="backward", k=log(n))
summary(backBICf22n)

summary(fwdBICn2f)
extractAIC(fwdBICn2f, k=log(n))[2]

summary(fwdBICn2f2)
extractAIC(fwdBICn2f2, k=log(n))[2]

summary(backBICf2n)
extractAIC(backBICf2n, k=log(n))[2]

summary(backBICf22n)
extractAIC(backBICf22n, k=log(n))[2]

###AIC

# Forward AIC with: null -> full
fwdAICn2f <- step(null, scope=formula(full), direction="forward")
summary(fwdAICn2f)

# Forward AIC with: null -> full2
fwdAICn2f2 <- step(null, scope=formula(full2), direction="forward")
summary(fwdAICn2f2)

# Backwards AIC from full to null
backAICf2n <- step(full, direction="backward")
summary(backAICf2n)

#Backwards AIC from full2 to null
backAICf22n <- step(full2, direction="backward")
summary(backAICf22n)

summary(fwdAICn2f)
extractAIC(fwdAICn2f)[2]
summary(fwdAICn2f2)
extractAIC(fwdAICn2f2)[2]
summary(backAICf2n)
extractAIC(backAICf2n)[2]
summary(backAICf22n)
extractAIC(backAICf22n)[2]


help("extractAIC")

fwdBIC <- step(empty, scope=formula(full), direction="forward", k=log(n))
summary(fwdBIC)
fwdBIC2 <- step(empty, scope=formula(full2), direction="forward", k=log(n))
summary(fwdBIC2)
backBIC=step(full2,direction="backward", k=log(n))
summary(backBIC)


## LASSO
library(glmnet)
X <- data.matrix(train[, c('year','month', 'day', 'hour', 'DEWP', 'TEMP', 'PRES', 'cbwd','Iws','Is','Ir')])

X <- X[,-1]

Y <- log(train$pm2.5)

lasso.fit <- cv.glmnet(x = X[train.samples,], 
                       y = Y, family="gaussian", 
                       alpha=1, standardize=FALSE)

summary(lasso.fit)
lasso.fit
betas <- coef(lasso.fit, s = "lambda.1se")
betas
model <- which(betas[2:length(betas)]!=0)
colnames(X)[model]

post.lasso <- lm(Y ~ X[train.samples,model])
summary(post.lasso)


# Prediction and Model selection

#Test Y
test_Y <- log(test$pm2.5)


#BIC model selected based on BIC value:
summary(backBICf22n)
extractAIC(backBICf22n, k=log(n))[2]
errorBIC <- predict(backBICf22n, newdata=test)-test_Y


#AIC model selected based on the AIC value:
summary(backAICf22n)
extractAIC(backAICf22n)[2]
errorAIC <- predict(backAICf22n, newdata=test)-test_Y


#Lasso Error
refitting.data <- data.frame(Y, X[,model])
post.lasso <- lm(Y ~ ., data=refitting.data[train.samples,])
errorLASSO <- predict(post.lasso, newdata=refitting.data[-train.samples,]) - test_Y

errorBIC
errorAIC
errorLASSO


#Comparing the errors:
MSE <- c(
  BIC=mean(errorBIC^2), 
  AIC=mean(errorAIC^2), 
  lasso=mean(errorLASSO^2)
)

round(MSE,4)
which(MSE==min(MSE))
which.min(MSE)