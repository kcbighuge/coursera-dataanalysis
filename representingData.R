firstName <- "Jeff"
class(firstName)

numSons = 1L ## use "L" to classify as "integer"
class(numSons)
numD = 11
class(numD) ## classified as "numeric"

myM <- matrix(c(1:6), byrow=T, nrow=2)
myM

v1 <- (c(6:9))
v2 <- (c("Jeff", "yo", "Al", "barn"))
myDF <- data.frame(heights=v1, names=v2) ; myDF
## data frame rows must match all vectors

v1Fac <- as.factor(v1) ; v1Fac

v3 <- c((1:5), NA) ;v3
is.na(v3)

## SUBSETTING
v1[c(1,2,4)]
myDF[1, 1:2]
myDF$names

## LOGICAL subsetting
myDF[myDF$names == "yo", ]
myDF[myDF$heights < 8, ]

args(dbinom)

x = c(0:10) ; x

args(sample)
heights <- rnorm(10, 188, 3) ; heights
sample(heights, 10, replace=T) ## some sample values duplicated

probs <- c(.4, .3, .2, .1, 0, 0, 0, 0, 0, 0)
sum(probs)
sample(heights, 10, replace=T, prob=probs)

set.seed(12345)
rnorm(5, 0, 1)

## QUIZ 1, #3
set.seed(31);
heightsCM = rnorm(30,mean=188, sd=5);
weightsK = rnorm(30,mean=84,sd=3); 
hasDaughter = sample(c(TRUE,FALSE),size=30,replace=T); 
dataFrame = data.frame(heightsCM,weightsK,hasDaughter);
## insert code here...
dataFrameSubset <- dataFrame[heightsCM > 188,]
mean(dataFrameSubset$weightsK)

## question #4
set.seed(41)
cauchyValues <- rcauchy(100)

set.seed(415)
sample(cauchyValues,10,replace=T)

