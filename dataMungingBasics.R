### Data Munging Basics
cameraData <- read.csv("./data/cameras.csv")

names(cameraData)
tolower(names(cameraData))

### Fixing CHARACTER vector
splitNames = strsplit(names(cameraData), "\\.") # use \\ in front offset period
splitNames[[6]]

### apply first element of character vector for each column/variable
firstElement <- function(x){x[1]}
sapply(splitNames,firstElement)

### Data for submission & review of SAT questions
reviews <- read.csv("./data/reviews.csv")
solutions <- read.csv("./data/solutions.csv")
head(reviews,2)

names(reviews)
sub("_","", names(reviews),)

testName <- "this_is_a_test"
sub("_","",testName)
gsub("_","",testName) # gsub = global sub

### convert quant var's to qualitative var's (factor) in RANGES - cut()
reviews$time_left[1:10]
timeRanges <- cut(reviews$time_left,seq(0,3600,by=600))
timeRanges[1:10]
class(timeRanges)
table(timeRanges, useNA="ifany")

### Quant var's in RANGES: cut2() in {Hmisc}
library(Hmisc)
timeRanges<- cut2(reviews$time_left, g=6)
table(timeRanges, useNA="ifany")

### ADDING extra variables
reviews$timeRanges <- timeRanges
head(reviews,2)

### MERGE data
names(reviews)
names(solutions)
dim(reviews) ; dim(solutions)

mergedData <- merge(reviews,solutions, all=T) # merged incorrectly on 'id'
head(mergedData, 3)
dim(mergedData)

mergedData2 <- merge(reviews,solutions, by.x="solution_id",by.y="id", all=T)
head(mergedData2,3)
dim(mergedData2)

reviews[1,1:6]
solutions[solutions$id==3,]
reviews[reviews$solution_id==3,]

### SORTING values
mergedData2$reviewer_id[1:10]
sort(mergedData2$reviewer_id)
as.factor(unique(mergedData2$reviewer_id))

### ORDERING values, from min to max
order(mergedData2$reviewer_id)[1:10] # shows which element belongs in order
mergedData2$reviewer_id[order(mergedData2$reviewer_id)] # includes NA values

### RE-ORDER a data frame
head(mergedData2[,1:6],4)
sortedData <- mergedData2[order(mergedData2$reviewer_id),]
head(sortedData[,1:6],4)

sortedData <- mergedData2[order(mergedData2$reviewer_id,mergedData2$id),]
head(sortedData[,1:6],9) # re-order by multiple var's

### RESHAPING data
misShaped <- as.data.frame(matrix(c(NA,5,1,4,2,3), byrow=T, nrow=3))
names(misShaped) <- c("treatmentA","treatmentB")
misShaped$people <- c("John","Jane","Mary")
misShaped

## RESHAPING data - melt()
library(reshape2)
melt(misShaped,id.vars="people",variable.name="treatment",value.name="value")
reShaped = melt(misShaped,id.vars="people",variable.name="treatment",value.name="value")
reShaped[order(reShaped$people),]




