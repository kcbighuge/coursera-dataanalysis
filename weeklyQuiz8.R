#### WEEK 8: Multiple Testing, Simulation/Model Checking, P-values

## testing with 0% TRUE Positives
set.seed(1010093) 
pValues <- rep(NA,1000)
for(i in 1:1000){
    y <- rnorm(20)
    x <- rnorm(20)
    pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}
# Controls false positive rate
sum(pValues < 0.05)
## Controls FWER-Family Wise Error Rate, Bonferroni
sum(p.adjust(pValues,method="bonferroni") < 0.05)
## Controls FDR-False Discovery Rate, Benjamini-Hochberg (BH)
sum(p.adjust(pValues,method="BH") < 0.05)

    
## testing with 50% TRUE Positives
set.seed(1010093)
pValues <- rep(NA,1000)
for(i in 1:1000) {
    x <- rnorm(20)
    # First 500 beta=0, last 500 beta=2
    if (i <= 500) {
        y <- rnorm(20)
        } else { 
        y <- rnorm(20,mean=2*x)
        } 
    pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}
trueStatus <- rep(c("zero","not zero"),each=500)
table(pValues < 0.05, trueStatus)  ## 24 FALSE Positives in 2nd 500 samples
# Controls FWER
table(p.adjust(pValues,method="bonferroni") < 0.05,trueStatus)
# Controls FDR
table(p.adjust(pValues,method="BH") < 0.05,trueStatus)

## P-values vs adjusted P-values
dev.off()
par(mfrow=c(1,2))
plot(pValues,p.adjust(pValues,method="bonferroni"),pch=19)
plot(pValues,p.adjust(pValues,method="BH"),pch=19)


### SIMULATION
set.seed(44333)
x <- rnorm(50)
e <- rnorm(50)
b0<-1;b1<-2 
y<-b0+b1*x+e

# Violating Assumptions
set.seed(44333)
x <- rnorm(50)
e <- rnorm(50); e2 <- rcauchy(50)
b0<-1;b1<-2 
y<-b0+b1*x+e
y2<- b0+b1*x+e2
# plot
par(mfrow=c(1,2))
plot(lm(y ~ x)$fitted,lm(y~x)$residuals,pch=19,xlab="fitted",ylab="residuals")
plot(lm(y2 ~ x)$fitted,lm(y2~x)$residuals,pch=19,xlab="fitted",ylab="residuals")

# Repeated Simulations
set.seed(44333)
betaNorm <- betaCauch <- rep(NA,1000)
for(i in 1:1000) {
    x <- rnorm(50); e <- rnorm(50); e2 <- rcauchy(50); b0 <- 1; b1 <- 2 
    y<- b0+b1*x+e
    y2<-b0+b1*x+e2
    betaNorm[i] <- lm(y ~ x)$coeff[2]
    betaCauch[i] <- lm(y2 ~ x)$coeff[2]
}
quantile(betaNorm)
quantile(betaCauch)  ## MUCH Fatter Tails in error terms, HIGH variability
boxplot(betaNorm,betaCauch,col="blue",ylim=c(-5,5))

## Simulation on Data Set (Galton)
library(UsingR); data(galton); nobs <- dim(galton)[1]
lm1 <- lm(galton$child ~ galton$parent)
parent0 <- rnorm(nobs,sd=sd(galton$parent),mean=mean(galton$parent))
child0 <- lm1$coeff[1] + 
    lm1$coeff[2]*parent0 + rnorm(nobs,sd=summary(lm1)$sigma)
par(mfrow=c(1,2))
plot(galton$parent,galton$child,pch=19)  ## actual
plot(parent0,child0,pch=19,col="blue")  ## simulated

## More Complicated Simulation
library(bootstrap); data(stamp); nobs <- dim(stamp)[1]
hist(stamp$Thickness,col="grey",breaks=100,freq=F)
dens <- density(stamp$Thickness)  ## calc density
lines(dens,col="blue",lwd=3)

plot(density(stamp$Thickness),col="black",lwd=3) 
for(i in 1:10){
    newThick <- rnorm(nobs,mean=stamp$Thickness,sd=dens$bw)  ## simulate dens
    lines(density(newThick),col="grey",lwd=3) 
}



## QUIZ: Question #2
set.seed(3343)
pValues = rep(NA,100)
for(i in 1:100){
    z = rnorm(20)
    x = rnorm(20)
    y = rnorm(20,mean=0.5*x)
    pValues[i] = summary(lm(y ~ x))$coef[2,4]
}
sum(pValues < 0.1)
sum(p.adjust(pValues,method="bonferroni") < 0.1)  ## control FWER
sum(p.adjust(pValues,method="BH") < 0.1)  ## control FDR

## Question #3
x <- rnorm(50); z <-rnorm(50); e <- rnorm(50)
x <- sort(x)
b0<-1;b1<-2 ; b2 <- 3
y <- b0 + b1*x + e
y <- sort(y)
y2<-b0+b1*x+b2*z+e

lm(y ~ x)
lm( y ~ c(x[-(41:50)], rnorm(10)) )
lm( c(y[-(41:50)], rnorm(10, sd=2)) ~ x)

rlm(y~x)
rlm( y ~ c(x[-(41:50)], rnorm(10)) )
rlm( c(y[-(41:50)], rnorm(10, sd=2)) ~ x)


