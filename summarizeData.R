## SUMMARIZE Data: Earthquake data from data.gov

fileUrl <- "http://earthquake.usgs.gov/earthquakes/catalogs/eqs7day-M1.txt"
download.file(fileUrl,destfile="./data/earthquakeData.csv",method="curl")
eData1 <- read.csv("./data/earthquakeData.csv")
earthquakeDateDownload <- date()
tempData <- eData1[-1,]

write.table(tempData, file= "./data/earthquakeDataModified.csv", sep= ",")
eData <- read.csv("./data/earthquakeDataModified.csv")

save(earthquakeDateDownload,eData, file="./data/housingData.rda")

### LOOKING at data: dim(),names(),nrow(),ncol()
dim(eData)
names(eData)
nrow(eData)
ncol(eData)

quantile(eData$Lat)
summary(eData)
class(eData)
sapply(eData[1,], class) # see class of each col var

unique(eData$Src) # see uniques
length(unique(eData$Src))
table(eData$Src)
table(eData$Src,eData$Version)

eData$Lat[1:10]
eData$Lat[1:10] > 40 # returns LOGICAL vector
any(eData$Lat[1:10] > 40) # returns LOGICAL output if ANY = TRUE
all(eData$Lat[1:10] > 40) # returns LOGICAL output if ALL = TRUE

### LOOKING at Subsets
eData[eData$Lat > 0 & eData$Lon > -65, c("Lat","Lon")] # subset AND
eData[eData$Lat > 65 | eData$Lon > 00, c("Lat","Lon")] # subset OR

### Peer Review Experiment Data: PLOS One
fileUrl1 <- "https://dl.dropbox.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropbox.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile="./data/reviews.csv", method="curl")
download.file(fileUrl2, destfile="./data/solutions.csv", method="curl")
reviews <- read.csv("./data/reviews.csv")
solutions <- read.csv("./data/solutions.csv")
head(reviews,2) ; dim(reviews)
head(solutions,2)

reviews$time_left[1:10]
is.na(reviews$time_left[1:10])
sum(is.na(reviews$time_left)) # count number of times na = True
table(is.na(reviews$time_left)) # counts for logical vector

### table() - NA issue
table(c(0,1,2,3,NA,3,3,2,2,3)) # create table using values, form levels
table(c(0,1,2,3,NA,3,3,2,2,3),useNA="ifany") # create table with na's

colSums(reviews)
rowSums(reviews)
colMeans(reviews, na.rm= T) # remove na values
rowMeans(reviews, na.rm= T) # remove na values





