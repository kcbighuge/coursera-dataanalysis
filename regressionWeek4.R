### REGRESSION: Least Squares Method
setwd("./DataAnalysis")
library(UsingR); data(galton)
names(galton)
dim(galton)
str(galton) ; summary(galton)

par(mfrow=c(1,2))
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)

boxplot(galton$child~galton$parent)
plot(galton$child~galton$parent)
cor(galton$child~galton$parent)
qqnorm(galton$child)
qqline(galton$child)

### plot mean child height
hist(galton$child,col="blue",breaks=100)
meanChild <- mean(galton$child)
lines(rep(meanChild,100),seq(0,150,length=100),col="red",lwd=5)

### Plot/Jittered Plot
plot(galton$parent,galton$child,pch=19,col="blue")
set.seed(1234)
?jitter ## add NOISE to NUMERIC vector
plot(jitter(galton$parent,factor=2),jitter(galton$child,factor=2),pch=19,col="blue")

### FITTING a LINE
plot(galton$parent,galton$child,pch=19,col="blue")
lm1 <- lm(galton$child ~ galton$parent)
lines(galton$parent, lm1$fitted, col="red",lwd=3)
?lines ## add connected line segments to plot
summary(lm1) ;  names(lm1)

### PLOT Residuals
plot(galton$parent,lm1$residuals,col="blue",pch=19)
abline(c(0,0),col="red",lwd=5)

### CREATE Population of 1 million families
newGalton <- data.frame(parent=rep(NA,1e6),child=rep(NA,1e6))
    ## 1 million row x 2 col dim
newGalton$parent <- rnorm(1e6,mean=mean(galton$parent),sd=sd(galton$parent))
    ## rnorm values using galton$parent
newGalton$child <- lm1$coeff[1] + lm1$coeff[2]*newGalton$parent + rnorm(1e6,sd=sd(lm1$residuals))
    ## derive $child values using y = a + b*x + e
smoothScatter(newGalton$parent,newGalton$child)
abline(lm1,col="red",lwd=3)

### Take a SAMPLE of 1 million rows
set.seed(134325); sampleGalton1 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm1 <- lm(sampleGalton1$child ~ sampleGalton1$parent)
plot(sampleGalton1$parent,sampleGalton1$child,pch=19,col="blue")
lines(sampleGalton1$parent,sampleLm1$fitted,lwd=3,lty=2)
abline(lm1,col="red",lwd=3)

### MANY Samples (ie, 100 samples of n=50)
sampleLm <- vector(100,mode="list")
for(i in 1:100){
    sampleGalton <- newGalton[sample(1:1e6,size=50,replace=F),]
    sampleLm[[i]] <- lm(sampleGalton$child ~ sampleGalton$parent)
}
smoothScatter(newGalton$parent, newGalton$child)
for(i in 1:100){abline(sampleLm[[i]], lwd=3, lty=2)}
abline(lm1, col="red", lwd=3)

### HISTOGRAM of Estimates
par(mfrow=c(1,2))
hist(sapply(sampleLm,function(x){coef(x)[1]}),col="blue",xlab="Intercept",main="")
hist(sapply(sampleLm,function(x){coef(x)[2]}),col="blue",xlab="Slope",main="")

### Estimating Values: SEE (Std Error Estimate)
sampleGalton4 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm4 <- lm(sampleGalton4$child ~ sampleGalton4$parent)
summary(sampleLm4)

### Standardized coefficients
Degrees of Freedom ~ 
    number of samples - number of things you estimated
^b0 - b0 / SE_^b0 ~ t_n-2  ## b0 estimate - b0 actual / SE b0 est
^b1 - b1 / SE_^b1 ~ t_n-2  ## b1 estimate - b1 actual / SE b1 est

### t-Distribution vs Normal
x <- seq(-5,5,length=100)
plot(x,dnorm(x),type="l",lwd=3, col="red") ## normal
?dt  ## t distribution
lines(x,dt(x,df=3),lwd=3,col="yellow")  ## t w/ 3 DF
lines(x,dt(x,df=10),lwd=3,col="blue")  ## t w/ 10 DF

### Confidence intervals:
summary(sampleLm4)$coeff
confint(sampleLm4,level=0.95)

### How you REPORT the inference
sampleLm4$coeff
confint(sampleLm4,level=0.99)
#    A one inch increase in parental height is associated with a 
#    <^b1> inch increase in child's height
#    (99% CI: 0.14 - 1.23 inches).

### P-Values: Galton data
## NULL Distribution
x <- seq(-20,20,length=100)
plot(x, dt(x, df=(928-2)),col="blue",lwd=3,type="l")

## NULL distribution + Observed Statistic
arrows(summary(lm1)$coeff[2,3],0.25,summary(lm1)$coeff[2,3],0,col="red",lwd=4)

### Calculating p-values
summary(lm1)


### REGRESSION with FACTOR variables: MOVIES data
download.file("http://www.rossmanchance.com/iscam2/data/movies03RT.txt",destfile="./data/movies.txt")
movies <- read.table("./data/movies.txt",sep="\t",header=T,quote="")
head(movies) ; dim(movies)

lm1 <- lm(movies$score ~ as.factor(movies$rating))
summary(lm1)
confint(lm1)

## What is avg diff b/w PG13 & R movies?
lm2 <- lm(movies$score ~ relevel(movies$rating, ref="R")) ## ref="R"
summary(lm2)
confint(lm2)

## Is there any diff in score between any of the movie ratings?
anova(lm1)  ## analysis of variance table
    ?anova  ## Df, Sum Sq, Mean Sq, F value, Pr(>F)
    ## F value = MSR / MSE (ie, as.factor(movies$rating) / Residuals)

### Tukey's (honestly significant difference test)
lmTukey <- aov(movies$score ~ as.factor(movies$rating))
TukeyHSD(lmTukey)



### MULTIPLE Variable REGRESSION
download.file("http://apps.who.int/gho/athena/data/GHO/WHOSIS_000008.csv? profile=text&filter=COUNTRY:;SEX:", destfile="./data/hunger.csv")
hungerOrig <- read.csv("./data/hunger.csv") ; dim(hungerOrig)
hunger <- hungerOrig[hungerOrig$SEX != "BTSX",] ; dim(hunger)
levels(hungerOrig$SEX)
head(hunger) ; dim(hunger)
names(hunger) <- casefold(names(hunger))
hunger$sex <- sub("FMLE", "Female", hunger$sex)
hunger$sex <- sub("MLE", "Male", hunger$sex)

lm1 <- lm(hunger$numeric ~ hunger$year)
plot(hunger$year,hunger$numeric,pch=19,col="blue")
lines(hunger$year,lm1$fitted, lwd=3, col="darkgrey")

### COLOR by Male/Female
lmM <- lm(hunger$numeric[hunger$sex=="Male"] ~ hunger$year[hunger$sex=="Male"])
lmF <- lm(hunger$numeric[hunger$sex=="Female"] ~ hunger$year[hunger$sex=="Female"])
plot(hunger$year,hunger$numeric,pch=19)
points(hunger$year,hunger$numeric,pch=19,col=((hunger$sex=="Male")*1+1))
lines(hunger$year[hunger$sex=="Male"],lmM$fitted,col="black",lwd=3)
lines(hunger$year[hunger$sex=="Female"],lmF$fitted,col="red",lwd=3)

lmBoth <- lm(hunger$numeric ~ hunger$year + hunger$sex)
summary(lmBoth)

lmBoth2 <- lm(hunger$numeric ~ hunger$year + hunger$sex + hunger$sex*hunger$year)
summary(lmBoth2)


###################### QUIZ 4 ##########################
?warpbreaks
dim(warpbreaks)
head(warpbreaks)
levels(warpbreaks$tension)
summary(lm(warpbreaks$breaks ~ warpbreaks$tension))
confint(lm(warpbreaks$breaks ~ warpbreaks$tension), level=.95)

lmWarp <- lm(warpbreaks$breaks ~ relevel(warpbreaks$tension, ref="M"))
lmWarp <- lm(warpbreaks$breaks ~ relevel(warpbreaks$tension, ref="H"))
summary(lmWarp)
confint(lmWarp, level=.95)

lmAOV <- aov(warpbreaks$breaks ~ warpbreaks$tension)
summary(lmAOV)


download.file("https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt", destfile="./data/movies2.txt", method="curl")
movies2 <- read.table("./data/movies2.txt",sep="\t",header=T,quote="") ; dim(movies2) ; dim(movies)
head(movies2) ; head(movies)
summary(lm(movies2$score ~ movies2$box.office))
confint(lm(movies2$score ~ movies2$box.office), level=.90)
lm1 <- lm(movies2$score ~ movies2$box.office)

summary(lm(movies2$score ~ movies2$box.office + movies2$running.time))
lm2 <- lm(movies2$score ~ movies2$box.office + movies2$running.time)
summary(lm2)

plot(movies2$box.office, movies2$score,pch=19,col="blue")
abline(lm1, lwd=3, col="red")
abline(lm2, lwd=3, col="yellow")

plot(movies2$box.office, movies2$running.time, pch=19,col="blue")
abline(lm(movies2$running.time ~ movies2$box.office))
summary(lm(movies2$running.time ~ movies2$box.office))

plot(movies2$running.time, movies2$score, pch=19,col="blue")
movies2$running.time > 200
movies2 <- movies2[(movies2$running.time > 200) == F,]
lm3 <- lm(movies2$score ~ movies2$box.office + movies2$running.time) ; summary(lm3)
abline(lm3, lwd=3, col="red")
lines(movies2$running.time, lm3$fitted, lwd=3, col="darkgrey")

levels(movies2$rating)
lm4 <- lm(movies2$score ~ movies2$rating + movies2$running.time + movies2$rating*movies2$running.time)
summary(lm4)
lm5 <- lm(movies2$score ~ relevel(movies2$rating, ref="PG") + movies2$running.time + relevel(movies2$rating, ref="PG") * movies2$running.time)
summary(lm5)

