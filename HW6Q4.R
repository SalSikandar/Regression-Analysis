# HW 6 Q4

# loading data

furn <- read.csv("/Users/salmansikandar/Documents/Regresion_Analysis/furniture.csv")

summary(furn)

furn

# Part a

furn$time <- seq_along(furn[,1])
plot(furniture$time,furniture$sales, xlab="time", ylab="sales", type="l", col=3, lwd=2)


# Part b
reg1 <- lm (furn$sales ~ furn$time)
abline(reg1,lwd=1.5)


# Part c

plot(furn$time,resid(reg1), pch=20)

# Part d

plot(furniture$time,furniture$sales, xlab="time", ylab="sales", type="l", col=3, lwd=2)
points(furn[furn$month==12,"time"],furn[furn$month==12,"sales"], col="blue", pch=19)


res<-resid(reg1)

plot(furn$time,res, pch=20)

for (i in c(12,24,36,48,60,72,84,96,108,120))
{
  points(furn[furn$time==i,"time"], res[i], col="blue", pch=19)
}


# Part e

# Split into training and test data
train <- furn[1:108,]
test <- furn[109:120,]

# Step 2
trainsales <- train$sales
traintime <- train$time

# this is subtle. don't use train$sales and train$time here
reg2 <- lm (sales ~ time, data=train)
reg2_predict <- predict(reg2, newdata=test)
error1 <- reg2_predict-test$sales
error1


# Part f
print(acf(furn$sales))
