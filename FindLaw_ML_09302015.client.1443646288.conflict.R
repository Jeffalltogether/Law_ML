##################### FindLaw.com Predictive Analysis #########################
### The purpose of this assessment is to determine if words used in criminal 
### case summaries can be used to predict elements of a court-case that would
###     1. Increase the efficiency of a lawyer to extract key information
###     2. Predict the liklihood of success for a case to win
###        an appeals proceeding.
library(tm)
library(dplyr)
setwd("C:/Users/jeffthatcher/Cloud Drive/RRepos/Law_DataSet/FindLaw/9thAppeals")

textDir <- "C:/Users/jeffthatcher/Cloud Drive/RRepos/Law_DataSet/FindLaw/9thAppeals"

categoryDocs <- paste(textDir, "/Category/", sep = "")
shortDocs <- paste(textDir, "/Short/", sep = "")
opinionDocs <- paste(textDir, "/Opinion/", sep = "")

###     Starting with some descriptive statistics.
###     how many categories of criminal appeals cases are in the data?
###     Load case category files:

corpRaw <- VCorpus(DirSource(directory = textDir), readerControl = list(reader=readPlain))



caseFiles <- dir(textDir, full.names = TRUE, recursive = FALSE)
caseTitles <- dir(textDir, full.names = FALSE, recursive = FALSE)

categoryFiles <- grep("Category",caseFiles)
caseTitles <- gsub(" Category.txt", "", caseTitles[categoryFiles])

caseClassData <- sapply(caseFiles[categoryFiles], read.table, sep=",", USE.NAMES=FALSE)



caseClass <- read.table(caseFiles[categoryFiles])

categoryInfo <- rep(1, length(categoryFiles))

for (i in 1:length(categoryFiles)){
        categoryInfo[i] <- read.table(caseList[categoryFiles[i]], sep=",")
}

### Load Category Files
corpRaw <- VCorpus(DirSource(directory = textDir), readerControl = list(reader=readPlain))
corpRaw <- cleanCorpus(corpRaw)

corpDTM <- TermDocumentMatrix(corpRaw)

words <- colnames(corpDTM)

df <- as.data.frame(as.matrix(corpDTM))
