## Exploratory GRAPHS
setwd("./DataAnalysis")
load("./data/loansDataMore.rda")
names(dat)
d = head(dat)

### BOXPLOT - col, varwidth, names, horizontal
boxplot(dat$intRate, col="blue")
boxplot(dat$intRate ~ as.factor(dat$fico), col="blue")
boxplot(dat$intRate ~ as.factor(dat$fico), col="blue", varwidth=T)

### BARPLOT - 
barplot(table(dat$fico), col="blue")
barplot(table(dat$intRate), col="blue")

### HISTOGRAM - breaks,freq,col,xlab,ylab, xlim, _ylim ,main
hist(dat$intRate, col="blue")
hist(dat$intRate,col="blue",breaks=50,main="Interest Rates")

### DENSITY PLOT
dens <- density(dat$intRate)
plot(dens, lwd=4, col="blue")

densFico <- density(dat$intRate[which(dat$fico>690)])
lines(densFico, lwd=3, col="orange")

### SCATTERPLOT - x,y,type,xlab,ylab,xlim,ylim,cex,col,bg
?par
plot(dat$fico, dat$intRate,pch=19,col="blue")
plot(dat$fico, dat$intRate,pch=19,col="blue", cex=0.5) # size matters!
plot(dat$fico, dat$intRate,pch=19,col=dat$inq6Mos, cex=0.7) # add color as var

### Overlaying Lines
plot(dat$fico, dat$intRate,pch=19,col="blue", cex=0.5)

### Edit colnames
head(dat[c(2,4)])
n <- c("amtReq", "amtFund", "monthInc", "openCred", "credBal", "inq6Mos", "amtUnfun", "fico", "intRate")
colnames(dat) <- n
head(dat)

### SCATTERPLOT - numeric var as factors
library(Hmisc)
palette(gray(seq(0,.9,len = 6))) ## change color palette
ratio <- dat$amtFund / dat$monthInc
thirdVar <- cut2(ratio, g=6)
plot(dat$fico, dat$intRate, pch=19,col=thirdVar,cex=0.7)

datSamp <- sample(1:2500,size=500,replace=FALSE)
plot(dat$fico[datSamp], dat$intRate[datSamp], pch=19,col=thirdVar[datSamp],cex=0.7)

smoothScatter(dat$fico, dat$intRate)
qqplot(dat$fico, dat$intRate)


### LOTS of points
x <- rnorm(1e5) ; y <- rnorm(1e5) ; plot(x,y,pch=19)
sampVal <- sample(1:1e5,size=1000,replace=FALSE) ## sample only SOME values
plot(x[sampVal],y[sampVal],pch=19)
smoothScatter(x,y) ## SMOOTH density plot

### QQ Plots - quantiles
x <- rnorm(20); y <- rnorm(20)
qqplot(x,y)
abline(c(0,1))

### Matplot & spaghetti - paramters: x, y, lty,lwd,pch,col
X <- matrix(rnorm(20*5),nrow=20)
matplot(X,type="b")

### HEATMAPS - paramters: x,y,z,col
image(1:10, 1:5 , as.matrix(dat[1:10,1:5]))
## Heatmaps - transpose to match intuition
newMatrix <- as.matrix(dat[1:10,1:5])
newMatrix <- t(newMatrix)[,nrow(newMatrix):1]
image(1:5, 1:10, newMatrix)

### MAPS - basics
library(maps)
map("world")
lat <- runif(40,-180,180) ; lon <- runif(40,-90,90)
points(lat,lon,col="blue",pch=19)

### MISSING values & plots
x <- rnorm(100) ; y <- rnorm(100)
x[y<0] <- NA
boxplot(y ~ is.na(x)) ## is.na(x) = TRUE when y < 0


############## Weekly Quiz 3 ###
### question 3, hierarchical clustering
head(iris)
irisSubset <- iris[,1:4] ; head(irisSubset)
distSubset <- dist(irisSubset) ; head(distSubset)
hClustering <- hclust(distSubset)
plot(hClustering) ## create DENDOGRAM

### question 4, K-means clustering
fileURL <- "https://spark-public.s3.amazonaws.com/dataanalysis/quiz3question4.rda"
download.file(fileURL, destfile="./data/quiz3question4.rda", method="curl")
load("./data/quiz3question4.rda")
ls()
plot(dataSet$x,dataSet$y,pch=19)
x <- dataSet$x ; y <- dataSet$y
dataFrame <- data.frame(x,y)

kmeansObj <- kmeans(dataFrame, centers=2) ## start with 3 centers
names(kmeansObj)
## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
## [6] "betweenss"    "size" 
kmeansObj$cluster

par(mar=rep(0.2,4))
plot(x,y, col=kmeansObj$cluster, pch=19, cex=2) ## plot color = CLUSTERS
points(kmeansObj$centers, col=1:3, pch=3, cex=3, lwd=3) ## plot CENTERS

### question 5
library(ElemStatLearn)
data(zip.train)
# Create an image matrix for the 3rd row, which is a 4
im = zip2image(zip.train,3) ; image(im)
zip.train[3,1] ## 3rd row = "4"

im8 = zip2image(zip.train,8) ; image(im8) ; zip.train[8,1]
im18 = zip2image(zip.train,18) ; image(im18) ; zip.train[18,1]

## calc svd of image matrix of 8th & 18th row
svd8 <- svd(im8)
plot(svd8$d,xlab="Column",ylab="Singluar value",pch=19)
plot(svd8$d^2 / sum(svd8$d^2), xlab="Column", ylab="% variance explained",pch=19)

svd18 <- svd(im18)
plot(svd18$d,xlab="Column",ylab="Singluar value",pch=19)
plot(svd18$d^2 / sum(svd18$d^2), xlab="Column", ylab="% variance explained",pch=19)

svd1 <- svd(zip.train)
names(svd1)
par(mfrow=c(1,3))
plot(svd1$d,xlab="Column",ylab="Singluar value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)



