########################### Webscraper for FindLaw.com  ########################
### To do this for all search results we will use the information give from the
### search querey found on this website.  After querreying all criminal cases
### in the 9th district of appeals from Jan 1, 2014 to Dec 31, 2014, We find the 
### following information:
#       * The information shows that the URL for the search is specific, and
#       * The number of pages of search results are indicated on the page.
### We will take this information manually from the page in order to loop
### through all of the search results and collect all of the data as text.

setwd("C:/Users/jeffthatcher/Cloud Drive/RRepos/Law_DataSet/FindLaw/9thAppeals")
library(XML)

###set directories for writing the three types text files
if(!file.exists("Category")) {
        dir.create("Category")
}
if(!file.exists("Short")) {
        dir.create("Short")
}
if(!file.exists("Opinion")) {
        dir.create("Opinion")
}

writeTextLoc <- "C:/Users/jeffthatcher/Cloud Drive/RRepos/Law_DataSet/FindLaw/9thAppeals"

writeCategoryLoc <- paste(writeTextLoc, "/Category/", sep = "")
writeShortLoc <- paste(writeTextLoc, "/Short/", sep = "")
writeOpinionLoc <- paste(writeTextLoc, "/Opinion/", sep = "")

### Set the file URL after your initial search perameters are entered on the website.
#       !!! be sure to use the URL that ends in "pgnum=1" !!!  
#       This can be accomplished by manually going to page #2 and then back to page #1
QuerreyURL <- "http://caselaw.findlaw.com/summary/search/?query=filters&court=us-9th-circuit&dateFormat=yyyyMMdd&startDate=20110101&endDate=20141231&topic=cs_19&pgnum=1"

### MANUALLY Identify the Numbeer of PAGES of search results
pages <- 94

### Use the page number information to generate a list of all the search result URLs
QuerreyURL <- gsub("pgnum=1", "pgnum=", QuerreyURL)
crawlThroughURLs <- rep(1, length(pages))

for (i in 1:pages){
        crawlThroughURLs[i] <- paste(QuerreyURL, as.character(i), sep="")
}

### Loop through all pages of the search rsults to collect all data in the order
### that follows inside the loop

for (i in 1:length(crawlThroughURLs)){

        ### Parse the first page of the search results
        doc1 <- htmlTreeParse(crawlThroughURLs[i], useInternal=TRUE)

        ### Capture the Category of all cases on the page
        caseCategory <- xpathSApply(doc1, "//i", xmlValue, trim = TRUE)

        ### Capture the case titles of all cases on the page
        caseTitles <- xpathSApply(doc1, "//tbody//tr//b", xmlValue, trim = TRUE)
        saveTitles <- gsub("[[:punct:]]", "", caseTitles)
        
        ### Write the category to a file in the working directory
        x <- paste(writeCategoryLoc, saveTitles, sep = "")
        x <- paste(x, ".txt", sep="")
        
        for (i in 1:length(caseCategory)){
                write(caseCategory[i], file = x[i])   
        }
        
        ### Capture the URL of all the cases on the page
        caseURL <- rep(1, length(caseTitles))
        findURLXpath <- "//a[contains(text(), 'CASE')]/@href"
        
        for (i in 1:length(caseTitles)){
                specificCase <- gsub("CASE", caseTitles[[i]], findURLXpath)
                caseURL[i] <- xpathSApply(doc1, specificCase)
        }
        
        ### Go to page on the caseURLs and extract the data.
        caseSynopsis <- rep(1, length(caseURL))
        caseOpinion <- rep(1, length(caseURL))
        for (i in 1:length(caseURL)){
                ### Read the html on a specific case page
                doc2 <- htmlTreeParse(caseURL[[i]], useInternal=TRUE)
        
                ### Get the brief case description
                caseSynopsisList <- xpathSApply(doc2, "//p", xmlValue, trim = TRUE)
                caseSynopsis[i] <- toString(caseSynopsisList)
                
                ### Get the URL for the full case summary
                specificCase <- gsub("CASE", caseTitles[[i]], findURLXpath)
                caseOpinionURL <- xpathSApply(doc2, specificCase)
        
                ### Go to case summary page and extract the data into a list variable.
                doc3 <- htmlTreeParse(caseOpinionURL, useInternal=TRUE)
        
                caseOpinionList <- xpathSApply(doc3, "//p", xmlValue, trim = TRUE)
                caseOpinion[i] <- toString(caseOpinionList)
        }
        
        ### Write the brief case description to a file
        x <- paste(writeShortLoc, saveTitles, sep = "")
        x <- paste(x, ".txt", sep="")
        
        for (i in 1:length(caseSynopsis)){
                write(caseSynopsis[i], file = x[i])   
        }
        
        ### Write the fulltext opinion to a file
        x <- paste(writeOpinionLoc, saveTitles, sep = "")
        x <- paste(x, ".txt", sep="")
        
        for (i in 1:length(caseOpinion)){
                write(caseOpinion[i], file = x[i])   
        }
}
