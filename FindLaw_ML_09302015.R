##################### FindLaw.com Predictive Analysis #########################
### The purpose of this assessment is to determine if words used in criminal 
### case summaries can be used to predict elements of a court-case that would
###     1. Increase the efficiency of a lawyer to extract key information
###     2. Predict the liklihood of success for a case to win
###        an appeals proceeding.
library(tm)
library(reshape2)
library(ggplot2)
library(plyr)
library(class)
library(caret)
#library(MASS)

setwd("C:/Users/jeffthatcher/Cloud Drive/RRepos/Law_DataSet/FindLaw/9thAppeals")

textDir <- "C:/Users/jeffthatcher/Cloud Drive/RRepos/Law_DataSet/FindLaw/9thAppeals"

categoryDocs <- paste(textDir, "/Category/", sep = "")
shortDocs <- paste(textDir, "/Short/", sep = "")
opinionDocs <- paste(textDir, "/Opinion/", sep = "")

###     Starting with some descriptive statistics.
###     how many categories of criminal appeals cases are in the data?
###     Load case category files:
categoryFiles <- dir(categoryDocs, full.names = TRUE, recursive = FALSE)
categoryTitles <- dir(categoryDocs, full.names = FALSE, recursive = FALSE)

caseClassData <- sapply(categoryFiles, read.table, sep=",", USE.NAMES=FALSE, strip.white=TRUE)

categoryDF = melt(caseClassData)
categoryDF <- categoryDF[,1:6]
rownames(categoryDF) <- categoryTitles
#categoryDF <- gsub(" ", "", categoryDF[1,]) #fixed by strip.white in read.table

categories <-c(levels(categoryDF$V1), levels(categoryDF$V2), levels(categoryDF$V3),
               levels(categoryDF$V4), levels(categoryDF$V5), levels(categoryDF$V6))
categories <- unique(categories)
categories
### Answer: There are 29 total categories

### How many cases are represented in each category?
wordTotals <- rep(1, length(categories))

for (i in 1: length(categories)){
        wordTotals[i] = sum(as.integer(categoryDF==categories[i]), na.rm=TRUE)       
}

wordCount <- as.data.frame(wordTotals, categories)
wordCount$category = rownames(wordCount)

Plot1 <- ggplot(wordCount, aes(x = category, y = wordTotals)) +
        geom_bar(colour = "black", stat= "identity", position = "dodge") + 
        scale_fill_brewer(palette="Pastel1") + 
        theme(axis.text.x = element_text(angle=60, hjust=1)) + 
        ggtitle("Categories of criminal cases ruled on by the 9th district court of appeals in 2014\nExtracted from FindLaw.com search \nTotal number of cases is 196") + 
        ylab("Number of cases")
Plot1
rm(wordCount, wordTotals)
### Can we predict the classificaiton of a case based on the Opinion Data from the case?
### First we will determine how many classes are present in the data set?
#   There appears to be a heirarchy of case categories

#   We will remove all cases from the list that do not have "Criminal Law & Procedure"
#   as the highest level of their class heirarchy.
casesToCategorize <- which(categoryDF$V1 == "Criminal Law & Procedure")
categoryDF <- categoryDF[casesToCategorize,]
categoryDF$caseTitle <- rownames(categoryDF)

### With this, we can define the class of each remaining case
class <- paste(as.character(categoryDF$V1), as.character(categoryDF$V2), sep=" ")

### The total number of unique classes in our dataset are 15 out of 172
### This results in about 11.5 cases per class.  Potentially too few for accurate
### classification, we can deal with this by collecting more data from the
### website if needed.

class.abbreviations <- abbreviate(class)
class.abbreviations <- gsub("&", "",class.abbreviations)
class.unique <- unique(class.abbreviations)
classKey <- cbind(unique(class), class.unique)

### How many cases to we have for each Class?
categoryDF$class <- class.abbreviations

class.caseNum <- rep(1, length(class.unique))

for (i in 1: length(class.unique)){
        class.caseNum[i] = sum(as.integer(categoryDF$class==class.unique[i]), na.rm=TRUE)       
}

class.count <- as.data.frame(class.caseNum, class.unique)
class.count$class = rownames(class.count)

Plot1 <- ggplot(class.count, aes(x = class, y = class.caseNum)) +
        geom_bar(colour = "black", stat= "identity", position = "dodge") + 
        scale_fill_brewer(palette="Pastel1") + 
        theme(axis.text.x = element_text(angle=60, hjust=1)) + 
        ggtitle("Classes of criminal cases for ML Prediction\nExtracted from FindLaw.com search \nTotal number of cases is __") + 
        ylab("Number of Cases")
Plot1

rm(class.count, class.caseNum)
### Turns out hat only 5 out of the 15 are represented by more than 2 cases.
### Again, this may pose a problem, but we will press on.

### The goal of the next step is to determine if we can predict the appropriate
### case Classification by the Opinion Data alone

#   Lets load the Opinion Data of the cases selected in the previous step:
#   This time we will use the tm-package to handle these large text files
cleanCorpus <- function(corpus) {
        corpus.tmp <- tm_map(corpus, removePunctuation)
        corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
        corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
        corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
        corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("english"))
        return(corpus.tmp)
}

corpRaw <- VCorpus(DirSource(directory = opinionDocs), readerControl = list(reader=readPlain))
corpRaw <- cleanCorpus(corpRaw)

### Some non-ASCII cahracters got stuck in the column names.  Lets remove them here
### by eliminating every character that's not an alphanumeric or a 
### whitespace character.
for(j in seq(corpRaw)){   
        corpRaw[[j]] <- gsub("[^\\w\\s]", "", corpRaw[[j]], perl = TRUE)
}

### Build a term document matrix
corpRaw <- tm_map(corpRaw, PlainTextDocument)  
corpDTM <- DocumentTermMatrix(corpRaw)

rm(corpRaw)

### remove the documents we have excluded from the classification problem

opinionDF <- as.data.frame(as.matrix(corpDTM))

### Now we can pull only the cases that begin with the criminal law category
#opinionDF <- opinionDF[, %in% categoryDF$caseTitle]
rownames(opinionDF) <- list.files(opinionDocs)
opinionDF <- opinionDF[which(rownames(opinionDF) %in% rownames(categoryDF)),]

rm(corpDTM)

### Now let's apply some prediction
### First, we will add the class label to each case and remove the case types 
### that are not represented enought to train a classifier
opinionDF$cla55 <- categoryDF$class[which(rownames(categoryDF) %in% rownames(opinionDF))]

#   Now a quick check to see that the correct class was assigned to each case
ifelse(((rownames(categoryDF[which(rownames(categoryDF) %in% rownames(opinionDF)),])) == rownames(opinionDF)) != TRUE,
        "case and class do not match", "case and class match")

#   Here we can remove those under-represented classes
class.toKeep <- c("CLPHC", "CLPS", "CrmnlLwPrcdrEv", "CrmnlLwPrcdrImL")
opinionDF <- opinionDF[which(opinionDF$cla55 %in% class.toKeep), ]


### Next, we will use the holdout method to generate our train and test set
###############https://www.youtube.com/watch?v=j1V2McKbkLo###################
case.classes <- opinionDF[,"cla55"]
case.data.noClass <- opinionDF[,-which(colnames(opinionDF)=="cla55")]

accuracy <- rep(1, 10)
for (i in 1:100){
        train.index <- sample(1:nrow(opinionDF), 0.6*nrow(opinionDF))
        test.index <- (1:nrow(opinionDF))[-train.index]
        
        knn.pred <- knn(case.data.noClass[train.index, ], case.data.noClass[test.index, ], case.classes[train.index])
        
        ### Accuracy
        #   Confusion Matrix
        
        conf.mat <- table("Predictions" = knn.pred, Actual = case.classes[test.index])
        print(conf.mat)
        accuracy[i] <- sum(diag(conf.mat)) / length(test.index) *100
}
tenFoldCV <- mean(accuracy)
tenFoldCV

#  Next we can try after removig the Near Zero Variance Terms
x <- nearZeroVar(opinionDF, saveMetrics = TRUE)
x <- as.data.frame(x)
nonZeroVarWords <- rownames(x[x[,"nzv"] <=0, ])
OpinionDF.nonZeroVar <- opinionDF[,which(colnames(opinionDF) %in% nonZeroVarWords)]

case.classes <- OpinionDF.nonZeroVar[,"cla55"]
case.data.noClass <- OpinionDF.nonZeroVar[,-which(colnames(OpinionDF.nonZeroVar)=="cla55")]

accuracy <- rep(1, 10)
for (i in 1:100){
        train.index <- sample(1:nrow(OpinionDF.nonZeroVar), 0.6*nrow(OpinionDF.nonZeroVar))
        test.index <- (1:nrow(OpinionDF.nonZeroVar))[-train.index]
        
        knn.pred <- knn(case.data.noClass[train.index, ], case.data.noClass[test.index, ], case.classes[train.index])
        
        ### Accuracy
        #   Confusion Matrix
        
        conf.mat <- table("Predictions" = knn.pred, Actual = case.classes[test.index])
        print(conf.mat)
        accuracy[i] <- sum(diag(conf.mat)) / length(test.index) *100
}
tenFoldCV <- mean(accuracy)
tenFoldCV

### Lets try some PCA on the data...
opinion.pca <- prcomp(case.data.noClass,
                                center = TRUE,
                                scale. = TRUE) 
#summary(opinion.pca) # two PCs for cumulative proportion of >80% 
opinion.PCAtransformed<-as.data.frame(opinion.pca$x[,1:3])

case.data.noClass <- opinion.PCAtransformed

accuracy <- rep(1, 10)
for (i in 1:100){
        train.index <- sample(1:nrow(opinion.PCAtransformed), 0.6*nrow(opinion.PCAtransformed))
        test.index <- (1:nrow(opinion.PCAtransformed))[-train.index]
        
        knn.pred <- knn(case.data.noClass[train.index, ], case.data.noClass[test.index, ], case.classes[train.index])
        
        ### Accuracy
        #   Confusion Matrix
        
        conf.mat <- table("Predictions" = knn.pred, Actual = case.classes[test.index])
        print(conf.mat)
        accuracy[i] <- sum(diag(conf.mat)) / length(test.index) *100
}
tenFoldCV <- mean(accuracy)
tenFoldCV

# So none of these simple-to-apply techniques made andy imporvements. This is a point
# where we might look at other aspects of the machine learning paradime, startinf with
# features. We created a list of words that are intuitively more important in assisting
# the categorization of these documents.

supervisedWords <- read.csv("C:/Users/jeffthatcher/Cloud Drive/RRepos/Law_ML/supervisedWords.csv", header=FALSE)

opinionDF.supervisedWords <- opinionDF[,which(colnames(opinionDF) %in% as.matrix(supervisedWords))]

case.classes <- opinionDF.supervisedWords[,"cla55"]
case.data.noClass <- opinionDF.supervisedWords[,-which(colnames(opinionDF.supervisedWords)=="cla55")]

accuracy <- rep(1, 10)
for (i in 1:100){
        train.index <- sample(1:nrow(opinionDF.supervisedWords), 0.6*nrow(opinionDF.supervisedWords))
        test.index <- (1:nrow(opinionDF.supervisedWords))[-train.index]
        
        knn.pred <- knn(case.data.noClass[train.index, ], case.data.noClass[test.index, ], case.classes[train.index])
        
        ### Accuracy
        #   Confusion Matrix
        
        conf.mat <- table("Predictions" = knn.pred, Actual = case.classes[test.index])
        print(conf.mat)
        accuracy[i] <- sum(diag(conf.mat)) / length(test.index) *100
}
tenFoldCV <- mean(accuracy)
tenFoldCV