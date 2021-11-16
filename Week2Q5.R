# Week 2 Question 5

rm(list=ls())
library(MASS)

## Sample size
n <- 800

## Measurement error variances
## setting these to zero means the true variable is observed (i.e. X.tilde = X or Y.tilde=Y)
delta.y.Squared <- 0
delta.x.Squared <- 0.5

# Note: if you change these too wildly, the plots won't look right anymore. 
# So if you try delta=10 or something, the plots may look weird and make you 
# reach the wrong conclusion. 

## No need to edit below here




## set regression coefficients and other parameters  
beta.0 <- 1
beta.1 <- 1
sigmaSquared <- 4  #the variance of the epsilons


# Note: if you change these too wildly, the plots won't look right anymore. 
# So if you try beta.1=50 or something, the plots may look weird and make you 
# reach the wrong conclusion. 



X <- rnorm(n)
X.tilde <- X + rnorm(n, mean=0, sd=sqrt(delta.x.Squared))


#True standard deviations, for comparison
sd.true.b.1 <- sqrt(sigmaSquared/sum((X - mean(X))^2))
sd.b.1 <- sqrt(sigmaSquared/sum((X.tilde - mean(X.tilde))^2))


## Initialize and perform the simulations
in.confint <- true.se.b.1.estimates <- true.b.1.estimates <- se.b.1.estimates <- b.1.estimates <- NULL
i <- 0

while(i<2000) {
  
  ## sample from the SLR
  i <- i+1
  
  epsilon <- rnorm(n, sd=sqrt(sigmaSquared))
  Y <- beta.0 + beta.1*X + epsilon
  Y.tilde <- Y + rnorm(n, mean=0, sd=sqrt(delta.y.Squared))
  
  ## Run the model on the variables we can actually observe, get the coefficient and standard error
  fit <- lm(Y.tilde ~ X.tilde)
  b.1.estimates <- rbind(b.1.estimates, coef(fit)[2])
  se.b.1.estimates <- rbind(se.b.1.estimates, coef(summary(fit))[2,2])
  
  ## is the true beta.1 in the confidence interval? This should happen 95% of the time.
  in.confint <- rbind(in.confint, ((coef(fit)[2] - 2*coef(summary(fit))[2,2] <= beta.1) & (beta.1 <= coef(fit)[2] + 2*coef(summary(fit))[2,2])))
  
  ## This is what we would like to do, but can't
  true <- lm(Y ~ X)
  true.b.1.estimates <- rbind(true.b.1.estimates, coef(true)[2])
  true.se.b.1.estimates <- rbind(true.se.b.1.estimates, coef(summary(true))[2,2])
  
}


## plotting the results
par(mfrow=c(1,1), mar=c(5,4,4,1))
plot.range.1 <- abs(mean(b.1.estimates) - beta.1)
plot.range.2 <- 3*max(mean(se.b.1.estimates), mean(true.se.b.1.estimates), sd.true.b.1, sd.b.1)
x.upper.lim <- beta.1 + plot.range.1 + plot.range.2
x.lower.lim <- beta.1 - plot.range.1 - plot.range.2



## plot the density of the estimates. 
slope.estimates <- round(50*b.1.estimates,0)/50
x.values <- sort(unique(slope.estimates))
density <- table(slope.estimates)/(length(slope.estimates)/50)

y.upper.lim <- max(density,dnorm(0, mean=0, sd=mean(true.se.b.1.estimates))) #need this to make sure the no-measurement error
# density is in the plot

plot(x=seq(from=(beta.1-4), to=(beta.1+4),length.out=10) , col=0, ylim=c(0,y.upper.lim), xlim=c(x.lower.lim, x.upper.lim), xlab="Slope Coefficient", ylab="", main="Frequency", yaxt="n", cex.lab=1.25, xaxt="n")
#     axis(side=1, at=c(beta.1), labels=c("true beta.1"))
axis(side=1, at=round(seq(from=x.lower.lim, to=x.upper.lim,length.out=5),1))
abline(v=beta.1, col="green", lty="longdash" , lwd=2)
segments(x0 = x.values , y0=0, y1 = density, col="darkgrey", lty=1, lwd=5)

## Curves of what we did get and what we should get
curve(dnorm(x, mean=beta.1, sd=mean(se.b.1.estimates)), from=(beta.1-4*mean(se.b.1.estimates)), to=(beta.1+4*mean(se.b.1.estimates)), add=TRUE, col="blue", lwd=2)
curve(dnorm(x, mean=beta.1, sd=mean(true.se.b.1.estimates)), from=(beta.1-4*mean(true.se.b.1.estimates)), to=(beta.1+4*mean(true.se.b.1.estimates)), add=TRUE, col="red", lwd=2)    
legend("topright",  legend=c("What actually happened", "N(true beta.1, estimated se.b.1)", "If we had no measurement error"), fill=c("grey","blue", "red"))


## percent of the time you selected the full model (model 2)
print(paste("The true beta.1 was in the confidence interval",100*mean(in.confint), "percent of the time (should be 95%)"))
