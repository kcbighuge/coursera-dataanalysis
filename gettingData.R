## GETTING DATA
getwd()
setwd("./DataAnalysis")
list.files()
dir.create("./data")

## Get from internet
?download.file
    // url, destfile, method

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/cameras.csv",method="curl")
    # use method="curl" if https
list.files("./data")

dateDownloaded <- date() ; dateDownloaded

## Loading data
?read.table
    // file, header (line), sep, row.names, nrows
?read.csv
?read.csv2

cameraData <- read.table("./data/cameras.csv")
cameraData <- read.table("./data/cameras.csv",sep=",",header=TRUE)
head(cameraData)

cameraData <- read.csv("./data/cameras.csv")
head(cameraData)

library(xlsx) # slow, read as .xlsx & write as .csv
?read.xlsx
    // file, sheetIndex, rowIndex, colIndex, header

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl, destfile= "./data/camera.xlsx", method= "curl")
cameraData <- read.xlsx2("./data/camera.xlsx", sheetIndex= 1)
head(cameraData)

?file.choose # less re-producible, use to search for data files

## REMEMBER: to close connections
?connections
    // file (text), url, gzfile, bzfile (.bz2)

?readLines # read lines of text from connection
    // params: con, n, encoding

### READ from LOCAL file
con <- file("./data/cameras.csv","r")
cameraData <- read.csv(con) # ?read.csv
close(con)
head(cameraData)

### READ from WEB
con <- url("http://simplystatistics.org","r") # "r" = read in text mode
simplyStats <- readLines(con, 10) # read only 10 lines, ?readLines
close(con)
simplyStats

### READ from JSON {RJSONIO}
library(RJSONIO)
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.json?accessType=DOWNLOAD"
download.file(fileUrl, destfile= "./data/camera.json",method="curl")
con = file("./data/camera.json")
jsonCamera <- fromJSON(con) # ?fromJSON function
close(con)
head(jsonCamera$meta$view)
jsonCamera$meta$view$id
jsonCamera$meta$view$name
jsonCamera$meta$view$attribution

### WRITING Data - write.table()
?write.table
    // parameters: x, file, quote, sep, row.names, col.names
cameraData <- read.csv("./data/cameras.csv")
tmpData <- cameraData[,-1] # remove FIRST col
write.table(tmpData, file= "./data/camerasModified.csv", sep= ",")
cameraData2 <- read.csv("./data/camerasModified.csv")
head(cameraData2) # same as cameraData w/out FIRST col

?save # save R objects into binary .rda file
    // list of objects, file
?save.image
    // save EVERYTHING in working dir
cameraData <- read.csv("./data/cameras.csv")
tmpData <- cameraData[,-1]
save(tmpData,cameraData,file="./data/cameras.rda")

### READING Saved Data - load()
?load
    // imoprtant parameter: file
rm(list=ls())               ; ls()
load("./data/cameras.rda")  ; ls()

### PASTING char strings together - paste(), paste0()
?paste
    // Important parameters: list of text strings, sep
    // paste0 is the same as paste except sep="" instead of sep=" "
    // Great for looping over files
    // See also file.path()
for(i in 1:5){ ## var i ends up = 5
    fileName = paste0("./data",i,".csv")
    print(fileName) ## var fileName ends up = 5
}

### GETTING data off web
library(XML)
con = url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode = readLines(con)
close(con)
htmlCode
summary(htmlCode)

html3 <- htmlTreeParse("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en", useInternalNodes = T)
xpathSApply(html3, "//title", xmlValue) # parameter //title finds <title>
xpathSApply(html3, "//td[@id='col-citedby']", xmlValue)

### FURTHER RESOURCES
    // packages: httr, RMySQL, bigmemory, RHadoop, foreign

######################
### QUIZ questions ###
######################
con = url('http://simplystatistics.tumblr.com/')
simplyStats <- readLines(con, n= 150)
close(con)
nchar(simplyStats[2])
nchar(simplyStats[45])
nchar(simplyStats[122])

fileUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/ss06hid.csv"
download.file(fileUrl, destfile= "./data/housing.csv",method="curl")
housing <- read.csv("./data/housing.csv")
head(housing)
housingDateDownload <- date()
save(housingDateDownload,housing, file="./data/housingData.rda")

popUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/ss06pid.csv"
download.file(popUrl, destfile= "./data/pop.csv",method="curl")
popDateDownload <- date()
population <- read.csv("./data/pop.csv")
save(popDateDownload,population, file="./data/populationData.rda")

nrow(subset(housing, VAL==24))

head(housing$FES)

nrow(subset(housing, BDS==3 & RMS==4))
nrow(subset(housing, BDS==2 & RMS==5))
nrow(subset(housing, BDS==2 & RMS==7))

agricultureLogical <- housing$ACR == 3 & housing$AGS == 6
which(agricultureLogical)

indexes =  which(agricultureLogical)
subsethousing = housing[indexes,]
sum(is.na(subsethousing$MRGX))

strsplit(names(housing), split= 'wgtp')

summary(housing$YBL)
quantile(housing$YBL, na.rm= T)

#### MERGE data frames housing, population by SERIALNO
load("./data/populationData.rda")
load("./data/housingData.rda")
housing.pop <- merge(housing,population, by.x="SERIALNO",by.y="SERIALNO", all=T)
dim(housing.pop)
dim(housing) ; dim(population)




