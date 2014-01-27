### Week 7: Smoothing, Bootstrapping
## pmax() <- tukey, loess()

## question 2
library(splines)
set.seed(53535)
xValues = seq(0,2*pi,length=100)
yValues = rnorm(100) + sin(xValues)
head(yValues)
?rmse

ns1 <- ns(xValues, df=4)
lm1 <- lm(yValues ~ ns1)
summary(lm1)  
sqrt(mean(lm1$residuals^2))  ## df=1 

plot(xValues, yValues,pch=19,cex=0.1)
points(xValues,lm1$fitted,col="blue",pch=19,cex=0.5)

## bootstrap
set.seed(333); x <- rnorm(30)
bootMean <- rep(NA,1000); sampledMean <- rep(NA,1000)
for(i in 1:1000){bootMean[i] <- mean(sample(x,replace=TRUE))}
for(i in 1:1000){sampledMean[i] <- mean(rnorm(30))}
plot(density(bootMean)); lines(density(sampledMean),col="red")

## boot package
library(boot)
set.seed(333); x <- rnorm(30);
sampledMean <- rep(NA,1000)
for(i in 1:1000){sampledMean[i] <- mean(rnorm(30))}
meanFunc <- function(x,i){mean(x[i])}
bootMean <- boot(x,meanFunc,1000) ; bootMean
plot(density(bootMean$t), col="blue"); lines(density(sampledMean),col="red")

## Nuclear costs -- linear model
library(boot); data(nuclear)
nuke.lm <- lm(log(cost) ~ date,data=nuclear)
plot(nuclear$date,log(nuclear$cost),pch=19)
abline(nuke.lm,col="red",lwd=3)

par(mfrow=c(1,3)) 
for(i in 1:3){
    nuclear0 <- nuclear[sample(1:dim(nuclear)[1],replace=TRUE),]  ## w/ replace
    nuke.lm0 <- lm(log(cost) ~ date,data=nuclear0)
    plot(nuclear0$date,log(nuclear0$cost),pch=19)
    abline(nuke.lm0,col="red",lwd=3)
}

## Bootstrap Distribution
dev.off()  ## re-set par
bs <- function(data, indices,formula) {
    d <- data[indices,]
    fit <- lm(formula, data=d)
    return(coef(fit))
}
results <- boot(data=nuclear, statistic=bs, R=1000, formula=log(cost) ~ date)
plot(density(results$t[,2]),col="red",lwd=3)
lines(rep(nuke.lm$coeff[2],10),seq(0,8,length=10),col="blue",lwd=3)

boot.ci(results)  ## Bootstrap confidence intervals, Non-/Less PARAMETRIC

## Bootstrapping from a Model
resid <- rstudent(nuke.lm)
fit0 <- fitted(lm(log(cost) ~ 1,data=nuclear)) 
newNuc <- cbind(nuclear,resid=resid,fit0=fit0) 
bs <- function(data, indices) {
    return(coef(glm(data$fit0 + data$resid[indices] ~ data$date,data=data))) 
}
results <- boot(data=newNuc, statistic=bs, R=1000)
plot(density(results$t[,2]),lwd=3,col="blue")  ## centered on 0
lines(rep(coef(nuke.lm)[2],10),seq(0,3,length=10),col="red",lwd=3)  ## observed

## EMPIRICAL p-value
B <- dim(results$t)[1]
(1 + sum((abs(results$t[,2]) > abs(coef(nuke.lm)[2]))))/(B+1)  ## p = 0.1838

## Bootstrapping NON-LINEAR stats
set.seed(555); x <- rnorm(30); sampledMed <- rep(NA,1000)
for(i in 1:1000) {
    sampledMed[i] <- median(rnorm(30))
}
medFunc <- function(x,i) {
    median(x[i])
} 
bootMed <- boot(x,medFunc,1000)
plot(density(bootMed$t),col="red",lwd=3)  ## bootstrapped distrib needs more n
lines(density(sampledMed),lwd=3)  ## sample distrib is flatter than boot

## CAN'T Bootstrap -- MAX values
set.seed(333); x <- rnorm(30); sampledMax <- rep(NA,1000)
for(i in 1:1000){sampledMax[i] <- max(rnorm(30))}
maxFunc <- function(x,i) { max(x[i]) }
bootMax <- boot(x,maxFunc,1000)
plot(density(bootMax$t),col="red",lwd=3,xlim=c(1,3))
lines(density(sampledMax),lwd=3)

## Bootstrapping Prediction Errors
library(boot); data(nuclear)
nuke.lm <- lm(log(cost) ~ date,data=nuclear)
plot(nuclear$date,log(nuclear$cost),pch=19)
abline(nuke.lm,col="red",lwd=3)

newdata <- data.frame(date = seq(65,72,length=100))
nuclear <- cbind(nuclear,resid=rstudent(nuke.lm),fit=fitted(nuke.lm))
nuke.fun <- function(data,inds,newdata){
    lm.b <- lm(fit + resid[inds] ~ date,data=data) 
    pred.b <- predict(lm.b,newdata) 
    return(pred.b)
}
nuke.boot <- boot(nuclear,nuke.fun,R=1000,newdata=newdata)
head(nuke.boot$t)

pred <- predict(nuke.lm,newdata)
predSds <- apply(nuke.boot$t,2,sd)
plot(newdata$date,pred,col="black",type="l",lwd=3,ylim=c(0,10))
lines(newdata$date,pred + 1.96*predSds,col="red",lwd=3)
lines(newdata$date,pred - 1.96*predSds,col="red",lwd=3)

## BAGGED loess (BAGGING, Boostrap Aggregation)
library(ElemStatLearn); data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]  ## put data in order by $ozone
head(ozone, 3) ; dim(ozone)

ll <- matrix(NA,nrow=10,ncol=155)  ## 10 sample bootstraps, ozone levels 1:155
for(i in 1:10) {
    ss <- sample(1:dim(ozone)[1],replace=T)  ## create index of random values
    ozone0 <- ozone[ss,];  ## take sample using randomized index
    ozone0 <- ozone0[order(ozone0$ozone),]  ## re-order by $ozone
    loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)  ## fit loess
    ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))  ## add to matrix
}

## Plot Bagging (loess)
plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10) {
    lines(1:155,ll[i,],col="grey",lwd=2)  ## plot each of 10 bootstraps
} 
lines(1:155,apply(ll,2,mean),col="red",lwd=2)  ## plot mean of 10 bootstraps


## BAGGING Trees
data(iris)
library(ipred)
## 25 bootstraps
bagTree <- bagging(Species ~., data=iris, coob=TRUE) # coob -> out-of-bag data
print(bagTree)
bagTree$mtrees[[2]]$btree
names(bagTree)


## RANDOM FORESTS -- Accuracy vs Speed, Interoperability, Overfitting
# 1. Bootstrap samples
# 2. At each split, bootstrap variables
# 3. Grow multiple trees and vote
library(randomForest)
forestIris <- randomForest(Species~ Petal.Width + Petal.Length, 
                           data=iris,prox=TRUE)
forestIris
# Type of random forest: classification Number of trees: 500
# No. of variables tried at each split: 1
# OOB estimate of error rate: 3.33% 
# Confusion matrix:
#             setosa versicolor virginica class.error
# setosa      50      0           0       0.00
# versicolor  0       47          3       0.06
# virginica   0       2           48      0.04

getTree(forestIris, k=2)

## Class "centers"
iris.p <- classCenter(iris[,c(3,4)], iris$Species, forestIris$prox)
plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4], 
     bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))], 
     main="Iris Data with Prototypes")
points(iris.p[,1], iris.p[,2], pch=21, cex=2, bg=c("red", "blue", "green"))

## COMBINING Random Forests
forestIris1 <- randomForest(Species~Petal.Width + Petal.Length, 
                            data=iris,prox=TRUE,ntree=50)
forestIris2 <- randomForest(Species~Petal.Width + Petal.Length, 
                            data=iris,prox=TRUE,ntree=50)
forestIris3 <- randomForest(Species~Petal.Width + Petal.Length, 
                            data=iris,prox=TRUE,nrtee=50)
combine(forestIris1,forestIris2,forestIris3)

## Random Forests: PREDICTING with NEW data
newdata <- data.frame(Sepal.Length<- rnorm(1000,mean(iris$Sepal.Length), 
                                           sd(iris$Sepal.Length)),
                      Sepal.Width <- rnorm(1000,mean(iris$Sepal.Width), 
                                           sd(iris$Sepal.Width)),
                      Petal.Width <- rnorm(1000,mean(iris$Petal.Width), 
                                           sd(iris$Petal.Width)),
                      Petal.Length <- rnorm(1000,mean(iris$Petal.Length), 
                                            sd(iris$Petal.Length)))
pred <- predict(forestIris,newdata) ; table(pred)
predOrig <- predict(forestIris, iris) ; table(predOrig, iris$Species)

## PLOT prediction of random forest
plot(newdata[,4], newdata[,3], pch=21, xlab="Petal.Length",ylab="Petal.Width", 
     bg=c("red", "blue", "green")[as.numeric(pred)],main="newdata Predictions")
    ## RandomForest captures boundaries not separated by lines or normal shapes


## COMBINING Classifiers (Ensembles)
library(devtools)
install_github("medley","mewo2") 
library(medley)
set.seed(453234)
y <- rnorm(1000)
x1 <- (y > 0); x2 <- y*rnorm(1000)
x3 <- rnorm(1000,mean=y,sd=1); x4 <- (y > 0) & (y < 3)
x5 <- rbinom(1000,size=4,prob=exp(y)/(1+exp(y)))
x6 <-(y<-2)|(y>2)
data <- data.frame(y=y,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6)
train <- sample(1:1000,size=500)
trainData <- data[train,]
testData <- data[-train,]
## Models
library(tree)
lm1 <- lm(y ~.,data=trainData)  ## lm1
rmse(predict(lm1,data=testData),testData$y)
tree1 <- tree(y ~.,data=trainData)  ## tree1
rmse(predict(tree1,data=testData),testData$y)
tree2 <- tree(y~.,data=trainData[sample(1:dim(trainData)[1]),])  ## tree2
rmse(predict(tree2,data=testData),testData$y)
## COMBINE Models
# combine1
combine1 <- predict(lm1,data=testData)/2 + predict(tree1,data=testData)/2 
rmse(combine1,testData$y)
# combine2
combine2 <- (predict(lm1,data=testData)/3 + predict(tree1,data=testData)/3 + 
                 predict(tree2,data=testData)/3)
rmse(combine2,testData$y)

## MEDLEY Package
library(medley)
library(e1071) 
library(randomForest)
x <- trainData[,-1] 
y <- trainData$y 
newx <- testData[,-1]
# Blending models (part 1)
m <- create.medley(x, y, errfunc=rmse)
for (g in 1:10) {
    m <- add.medley(m, svm, list(gamma=1e-3 * g)); 
}
# Blending (part2)
for (mt in 1:2) {
    m <- add.medley(m, randomForest, list(mtry=mt));
}
m <- prune.medley(m, 0.8)
rmse(predict(m,newx),testData$y)



##  Question 3
library(simpleboot) 
data(airquality)
attach(airquality)

summary(airquality$Wind)
sumWind <- as.matrix(summary(airquality$Wind))
set.seed(883833);
quant75 <- function(x) { quantile(x, .75) }
bootAir <- one.boot(airquality$Wind, quant75, 1000)
plot(density(bootAir$t), col="blue",lwd=3)
lines(rep(sumWind[5],10),seq(0,8,length=10),col="red",lwd=3)
apply(bootAir$t, 2, sd)

## Question 4
library(tree)
data(Cars93,package="MASS")
set.seed(7363);
treeFun <- function(x) { tree(DriveTrain ~ Price + Type, Cars93)}
newdata = data.frame(Type = "Large",Price = 20)

carSamp1 = Cars93[sample(nrow(Cars93), replace=TRUE),]
carSamp2 = Cars93[sample(nrow(Cars93), replace=TRUE),]
carSamp3 = Cars93[sample(nrow(Cars93), replace=TRUE),]

tree1 <- tree(DriveTrain ~ Price + Type, carSamp1)
tree2 <- tree(DriveTrain ~ Price + Type, carSamp2)
tree3 <- tree(DriveTrain ~ Price + Type, carSamp3)
summary(tree1) ; summary(tree3) ; summary(tree3)
pred1 <- predict(tree1, newdata) ; pred1
pred2 <- predict(tree2, newdata) ; pred2
pred3 <- predict(tree3, newdata) ; pred3

## use FOR loop to sample 3 times
par(mfrow=c(1,3))
for(i in 1:3) {
    cars0 <- Cars93[sample(1:dim(Cars93)[1],replace=TRUE),] 
    carsTree <- tree(DriveTrain ~ Price + Type, Cars93)
    plot(carsTree)
}

## Question 5
library(randomForest)
library(e1071)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

forestVowel <- randomForest(y ~ ., data=vowel.train, prox=TRUE)
forestPred <- predict(forestVowel, vowel.train)
forestVowel ; table(vowel.train$y, forestPred)

svmVowel <- svm(y ~ ., data= vowel.train)
svmPred <- predict(svmVowel, vowel.train)
svmVowel ; table(vowel.train$y, svmPred)

forestTest <- predict(forestVowel, vowel.test)
svmTest <- predict(svmVowel, vowel.test)

1 - sqrt(mean(forestTest==vowel.test$y)^2)
1 - mean(svmTest==vowel.test$y)

svmCombine <- svmTest==vowel.test$y
forestCombine <- forestTest==vowel.test$y
1 - mean(svmCombine==forestCombine)


