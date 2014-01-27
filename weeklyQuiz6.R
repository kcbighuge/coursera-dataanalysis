#### Week 6: Study Design, Cross-validation, regression + trees

## Predict with iris data
table(iris$Species) ; names(iris)
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
legend(1,4.5,legend=unique(iris$Species), 
       col=unique(as.numeric(iris$Species)),pch=19)

## alternative to library(tree) is library(rpart)
library(tree)
?tree()
tree1 <- tree(Species ~ Sepal.Width + Petal.Width,data=iris)
summary(tree1)
#  Classification tree:
#  tree(formula = Species ~ Sepal.Width + Petal.Width, data = iris)
#  Number of terminal nodes:     5 
#  Residual mean deviance:       0.204 = 29.57 / 145 
#  Misclassification error rate: 0.03333 = 5 / 150 
plot(tree1)
text(tree1)

## CART model: Classification And Regression Tree model
plot(iris$Petal.Width,iris$Sepal.Width, pch=19, col=as.numeric(iris$Species))
?partition.tree()
partition.tree(tree1, label="Species", add=TRUE)
legend(1.75,4.5, legend=unique(iris$Species), 
       col=unique(as.numeric(iris$Species)), pch=19)

## PREDICTING new values
set.seed(32313)
newdata <- data.frame(Petal.Width= runif(20, 0.1,2.5), 
                      Sepal.Width= runif(20, 2,4.4))
?predict()
pred1 <- predict(tree1, newdata) ; pred1

## OVERLAY new values
palette("default")
palette(gray(seq(0.1,0.8, len = 3)))  # change plot colors
pred1 <- predict(tree1, newdata, type="class") ; pred1  # returns "class"
plot(newdata$Petal.Width,newdata$Sepal.Width, col=as.numeric(pred1), pch=19)dddd
partition.tree(tree1, "Species", add=TRUE)

## EXAMPLE: Cars, Pruning trees
data(Cars93,package="MASS") ; head(Cars93)
treeCars <- tree(DriveTrain ~ MPG.city + MPG.highway + AirBags + EngineSize + 
                     Width + Length + Weight + Price + Cylinders + 
                     Horsepower + Wheelbase, data=Cars93)
plot(treeCars) ; text(treeCars)

## Plot ERRORS
par(mfrow=c(1,2))
?cv.tree()
plot(cv.tree(treeCars, FUN=prune.tree, method="misclass"))  # plot misclass
plot(cv.tree(treeCars))  # plot deviance

## PRUNE the tree
dev.off()  # turn off device
?prune.tree()
pruneTree <- prune.tree(treeCars, best=4)
plot(pruneTree) ; text(pruneTree)

## Show resubstitution error
table(Cars93$DriveTrain, predict(pruneTree, type="class"))
table(Cars93$DriveTrain, predict(treeCars, type="class"))
table(Cars93$DriveTrain, Cars93$MPG.city)


##################### QUIZ questions ##############################

## Question 3
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1], size= dim(SAheart)[1]/2, replace= F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
# look at data
dim(SAheart) ; names(SAheart) ; head(SAheart, 3)
# glm1 model on trainSA data
glm1 <- glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
            family= "binomial", data= trainSA)
predTrain <- predict(glm1, trainSA, type="response") ; predTrain
predTest <- predict(glm1, testSA, type="response") ; predTest
# define function to calc misclassification rate
missClass = function(values,prediction) {
    sum(((prediction > 0.5)*1) != values)/length(values)
    }
# use missClass function to calc misclassification rate
missClass(trainSA$chd, predTrain)  # Train error
missClass(testSA$chd, predTest)    # Test error


## Question 4
library(pgmm)
data(olive)
olive = olive[,-1]
head(olive, 3)
# train tree for olive data
treeOlive <- tree(Area ~ Palmitic + Palmitoleic + Stearic + Oleic + Linoleic +
                      Linolenic + Arachidic + Eicosenoic, data=olive)
plot(treeOlive) ; text(treeOlive)

pruneOlive <- prune.tree(treeOlive, best=4)
plot(pruneOlive) ; text(pruneOlive)

table(olive$Area, predict(pruneOlive, olive))
table(olive$Area, predict(treeOlive, olive))

newData = data.frame(Palmitic = 1200, Palmitoleic = 120, Stearic=200,
                     Oleic=7000, Linoleic = 900, Linolenic = 32, 
                     Arachidic=60, Eicosenoic=6)




