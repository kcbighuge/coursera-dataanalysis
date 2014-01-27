## Cleaned Data Set
library(kernlab)
data(spam)
dim(spam)

# divide into train + test data
set.seed(3435)
trainIndicator = rbinom(4601,size=1,prob=0.5)
table(trainIndicator)

trainSpam = spam[trainIndicator==1,]
testSpam = spam[trainIndicator==0,]
dim(trainSpam)
head(trainSpam[,11:20])
names(trainSpam)
table(trainSpam$type) # view nonspam vs spam

# Plot capital letter vs type (spam or ham)
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)
plot(log10(trainSpam[, 1:4] + 1))

hCluster = hclust(dist(t(trainSpam[, 1:57]))) # cluster 57 col var's
plot(hCluster)

hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated) # plot 55 col var's

# Statistical Model
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x, y) {
    sum(x != (y > 0.5))
}
cvError = rep(NA, 55)
library(boot)
for (i in 1:55) {
    lmFormula = as.formula(paste("numType~", names(trainSpam)[i], sep=""))
    glmFit    = glm(lmFormula, family = "binomial", data = trainSpam)
    cvError[i]= cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}
which.min(cvError) # col 53 contains min error
names(trainSpam)[which.min(cvError)] # var charDollar, col 53 has min error

# Get measure of uncertainty
predictionModel = glm(numType~charDollar, family="binomial", data=trainSpam)

predictionTest  = predict(predictionModel, testSpam)
predictedSpam   = rep("nonspam", dim(testSpam)[1])

predictedSpam[predictionModel$fitted > 0.5] = "spam"
table(predictedSpam, testSpam$type)

(61 + 458)/(1346 + 458 + 61 + 449) # calc total errors = 61+458/TOTAL = 0.224

precisionNonspam <- 1346/(1346+458) ; precisionNonspam
precisionSpam    <- 449/(449+61)    ; precisionSpam   
recallNonSpam <- 1346/(1346+61) ; recallNonSpam
recallSpam    <- 449/(449+458)  ; recallSpam

# Organizing data analysis, FOLDERS
1) Data: Raw, Processed
2) Figures: Exploratory, Final
3) R code: Raw, Final, R Markdown
4) Text: Readme, Text of analysis












