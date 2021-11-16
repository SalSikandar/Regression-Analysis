 #HW 5

data <- read.csv("/Users/salmansikandar/Documents/Regresion_Analysis/bikeSharing.csv")

data[data$weather==i,]
summary(data)



summary(casual.reg <- lm(data$casual ~ data$humidity, data=data))
summary(registered.reg <- lm(data$registered ~ data$humidity, data=data))
summary(casual.reg1 <- lm(casual ~ humidity, data=bike[bike$humidity>0,]))
summary(registered.reg1 <- lm(registered ~ humidity, data=bike[bike$humidity>0,]))

data[data$count > 750,]

plot (data$weather, data$count)
plot (data$weather, data$registered)

weather <- as.factor(data$weather)
boxplot (weather, data$casual)
for (i in 1:4)
{
  print(nrow (data[data$weather==i,]),i)
}

reg1 <- lm(data$count ~ data$weather)
abline(reg1, col=2, lwd=2)


tmp<-data[data$weather==3,]
tmp[tmp$count>70,]

#part b

set.seed(2)
training.samples <- sample.int(nrow(data), 0.5*nrow(data))
train <- data[training.samples,] 
test <- data[-training.samples,]

nrow(data)
nrow(test)
nrow(train)


# part c

# Creating an empty column for hours
train$hours = 0

# Creating a 1 column dataframe with all hours
trainHours <- substr(train$datetime, 12, 13)

# Appending the hours column to the training set column
train$hours <- trainHours

boxplot(train$count~train$hours,xlab="Hour of day", ylab="Number of users")

names(train)

plot (train$hours, train$count, pch=20)
reg2 <- lm (train$count ~ train$hours)
summary(reg2)

plot(coef(reg2), pch=20, xlab="Hour of day", ylab="Coeffecient Values for regression (Count ~ Hours)")


reg3 <- lm (train$casual ~ train$hours)
summary(reg3)

plot(coef(reg3), pch=20, xlab="Hour of day", ylab="Coeffecient Values for regression (Casual ~ Hours)")

reg4 <- lm (train$registered ~ train$hours)
summary(reg4)

plot(coef(reg4), pch=20, xlab="Hour of day", ylab="Coeffecient Values for regression (Registered ~ Hours)")

# Do the important hours change with season

reg5 <- lm (train$count ~ train$hours + train$season)
summary(reg5)


# Dummy variables
#dataf$Disc_A <- ifelse(dataf$discipline == 'A', 1, 0)

train$commute <- ifelse(train$hours >= "07" & train$hours <= "09" | train$hours >= "16" & train$hours <= "19", 1, 0)
train$business <- ifelse(train$hours >= "10" & train$hours <= "15", 1, 0)
train$night <- ifelse(train$hours >= "20" & train$hours <= "23" | train$hours >= "00" & train$hours <= "06", 1, 0)


train$time_type[train$commute==1]="commute"
train$time_type[train$business==1]="business"
train$time_type[train$night==1]="night"

train

reg6 <- lm (train$count ~ train$time_type)
summary(reg6)

reg7 <- lm (train$registered ~ train$time_type)
summary(reg7)

reg8 <- lm (train$casual ~ train$time_type)
summary(reg8)


# Part f. Partial F Test
anova(reg4,reg7)



