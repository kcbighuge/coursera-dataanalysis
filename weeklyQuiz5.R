###3 Weekly Quiz 5 ####

data(warpbreaks)
names(warpbreaks)
lmWarp <- lm(breaks ~ ., data=warpbreaks)
anova(lmWarp)
summary(lmWarp)

## question 3,4
library(glm2)
data(crabs)
head(crabs)
lmSat <- glm(Satellites ~ Width, data=crabs, family="poisson")
summary(lmSat)
lmSat

## question 5
library(MASS)
head(quine)
lm1 = lm(log(Days + 2.5) ~., data=quine)
aicForm <- step(lm1)






