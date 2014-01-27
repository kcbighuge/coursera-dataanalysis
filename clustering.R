### CLUSTERING
// DISTANCE or SIMILARITY:
    // Euclidean Distance # Continuous
    // Correlation Similarity # Continuous
    // Manhattan Distance # Binary

### Hierarchical Clustering - example
set.seed(1234); par(mar=c(0,0,0,0))
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))

?dist ## parameters: x,method
dataFrame <- data.frame(x=x,y=y)
dist(dataFrame) ## create new points b/w 2 closest points

?hclust
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering) ## create DENDOGRAM

### Pretty DENDOGRAMS - use function myplclust()
myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
## modifiction of plclust for plotting hclust objects *in colour*!
## Copyright Eva KF Chan 2009
## Arguments:
## hclust: hclust object
## lab: a character vector of labels of the leaves of the tree
## lab.col: colour for the labels; NA=default device foreground colour
## hang: as in hclust & plclust
## Side effect:
## A display of hierarchical cluster with coloured leaf labels.
y <- rep(hclust$height,2); x <- as.numeric(hclust$merge)
y <- y[which(x<0)]; x <- x[which(x<0)]; x <- abs(x)
y <- y[order(x)]; x <- x[order(x)]
plot( hclust, labels=FALSE, hang=hang, ... )
text( x=x, y=y[hclust$order]-(max(hclust$height)*hang),
    labels=lab[hclust$order], col=lab.col[hclust$order], 
    srt=90, adj=c(1,0.5), xpd=NA, ... )
}

myplclust(hClustering,lab=rep(1:3,each=4),lab.col=rep(1:3,each=4))

### heatmap()
?heatmap
dataFrame <- data.frame(x=x,y=y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)


### K-Means Clustering - example
set.seed(1234); par(mar=c(0,0,0,0))
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))

?kmeans ## parameters: x,centers,iter.max,nstart
dataFrame <- data.frame(x,y)
kmeansObj <- kmeans(dataFrame, centers=3) ## start with 3 centers
names(kmeansObj)
kmeansObj$cluster

par(mar=rep(0.2,4))
plot(x,y,col=kmeansObj$cluster,pch=19,cex=2)
points(kmeansObj$centers,col=1:3,pch=3,cex=3,lwd=3)


## Heatmaps
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
kmeansObj2 <- kmeans(dataMatrix,centers=3)
par(mfrow=c(1,2),mar=rep(0.2,4))
image(t(dataMatrix)[,nrow(dataMatrix):1],yaxt="n")
image(t(dataMatrix)[,order(kmeansObj$cluster)],yaxt="n")


### DIMENSION REDUCTION - Matrix Data
set.seed(12345); par(mar=rep(0.2,4))
dataMatrix <- matrix(rnorm(400),nrow=40)
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])

## Cluster the data
par(mar=rep(0.2,4))
heatmap(dataMatrix) ## 40 rows of observations, 10 cols of variables

## ADD a PATTERN
set.seed(678910)
for(i in 1:40){
    # flip a coin
    coinFlip <- rbinom(1,size=1,prob=0.5)
    # if coin is heads add a common pattern to that row
    if(coinFlip){
        dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3),each=5)
    }
}
par(mar=rep(0.2,4))
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
par(mar=rep(0.2,4))
heatmap(dataMatrix) ## see added pattern

### Pattern in rows & columns
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered),40:1,,xlab="Row",ylab="Row Mean",pch=19)
plot(colMeans(dataMatrixOrdered),xlab="Column",ylab="Column Mean",pch=19)

### Dimension Reduction
// Related solutions - PCA/SVD (Singular Value Decomposition)

## Components of the SVD - u and v
?svd
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1],40:1,,xlab="Row",ylab="First left singular vector",pch=19)
plot(svd1$v[,1],xlab="Column",ylab="First right singular vector",pch=19)

## Components of the SVD - d and variance
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,2))
plot(svd1$d,xlab="Column",ylab="Singluar value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)

## Relationship to principal components
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered,scale=TRUE)
plot(pca1$rotation[,1],svd1$v[,1],pch=19,xlab="Principal Component 1",ylab="Right Singular Vector 1")
abline(c(0,1))

## Components of the SVD - variance explained
constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){
    constantMatrix[i,] <- rep(c(0,1),each=5)
}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singluar value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)


### MISSING Values - error: infinite or missing values
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=F)] <- NA
svd1 <- svd(scale(dataMatrix2))
######### Imputing {impute} - replace missing values
library(impute)
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=F)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered))
svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2))
plot(svd1$v[,1],pch=19)
plot(svd2$v[,1],pch=19)
####################### Removed from CRAN ###########


### CLUSTERING Example ###  SAMSUNG smartphone data
setwd("./DataAnalysis")






