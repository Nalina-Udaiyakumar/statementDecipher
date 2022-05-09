## Deciphering Credit card statements
#### Part 3: Matching transactions in statements to store categories from directory

# Clearing work environment variables and checking r version used in session
# rm()
# sessionInfo()

#### Library calls -----------
# install.packages("pdftools")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("plyr")
# install.packages("xlsx")


library(pdftools)
library(tidyverse)
library(lubridate)
library(stringr)
library(plyr)
library(xlsx)


#### ---------- Reading the credit card statements

# Setting directory to the credit card statements folder
setwd('//Your directory here//')

# Read required files from the directory
completeListOfTransactions <- 
  read.xlsx( "Credit Card Statements.xlsx", sheetName = "Sheet1")


#### Manipulate storenames in storedirectory to match with the description in transactions----

## Create extra rows in directory for stores that have Canada or Canadian in their name, 
##  replacing with Can and Cdn respectively - to match transactions with them
##  Ex: PetroCanada appears in transactions as "PetroCan" and "Canadian tire" as "CDN tire"
# Checking for stores that have Canada or Canadian in their names
grep(pattern = "Canada", x = storeDirectory$StoreName)
grep(pattern = "Canadian", x = storeDirectory$StoreName)
# Creating a subset of the directory with the chosen rows, to perform changes 
subStoreDirectory <- storeDirectory[grep(pattern = "Canada", x = storeDirectory$StoreName),]
subStoreDirectory <- rbind.data.frame(subStoreDirectory,
                                      storeDirectory[grep(pattern = "Canadian", x = storeDirectory$StoreName),])
subStoreDirectory$StoreName <- str_replace_all(string = subStoreDirectory$StoreName,
                                               pattern="Canada",
                                               replacement = "Can")
subStoreDirectory$StoreName <- str_replace_all(string = subStoreDirectory$StoreName,
                                               pattern="Canadian",
                                               replacement = "Cdn")
## Append subset back to storedirectory
storeDirectory <- rbind.data.frame(storeDirectory,subStoreDirectory)

## Create extra rows in directory for stores that have a - in their name, 
# Checking for stores that have a - in their names
grep("\\-",storeDirectory$StoreName)
# Creating a subset of the directory with the chosen rows, to perform changes 
subStoreDirectory <- storeDirectory[grep(pattern = "\\-", x = storeDirectory$StoreName),]
subStoreDirectory$StoreName <- str_replace_all(string = subStoreDirectory$StoreName,
                                               pattern="\\-",
                                               replacement = "")
## Append subset back to storedirectory
storeDirectory <- rbind.data.frame(storeDirectory,subStoreDirectory)

## Create extra rows in directory for stores that have a ' in their name, 
# Checking for stores that have a - in their names
grep("'",storeDirectory$StoreName)
# Creating a subset of the directory with the chosen rows, to perform changes 
subStoreDirectory <- storeDirectory[grep(pattern = "'", x = storeDirectory$StoreName),]
subStoreDirectory$StoreName <- str_replace_all(string = subStoreDirectory$StoreName,
                                               pattern="'",
                                               replacement = "")
## Append subset back to storedirectory
storeDirectory <- rbind.data.frame(storeDirectory,subStoreDirectory)


## Create extra rows in directory for stores that have a " in their name, 
# Checking for stores that have a - in their names
grep('"',storeDirectory$StoreName)
# Creating a subset of the directory with the chosen rows, to perform changes 
subStoreDirectory <- storeDirectory[grep(pattern = '"', x = storeDirectory$StoreName),]
subStoreDirectory$StoreName <- str_replace_all(string = subStoreDirectory$StoreName,
                                               pattern='"',
                                               replacement = "")
## Append subset back to storedirectory
storeDirectory <- rbind.data.frame(storeDirectory,subStoreDirectory)

# Removing "Energy" from the names of gas stations to match with transaction description
storeDirectory$StoreName <- str_replace_all(string = storeDirectory$StoreName,
                                            pattern = "Energy",
                                            replacement = "")
storeDirectory$StoreName <- str_trim(storeDirectory$StoreName)



### Checks -------------------
storeDirectory$Rank <- NA
storeDirectory <- storeDirectory[order(storeDirectory$StoreName),]
storeDirectory$Rank <- rank(storeDirectory$StoreName)
a <- table(storeDirectory$Rank)
## Check: If unique(a)=1 then the store directory has no duplicate store names
print(unique(a))

## Matching storenames using grep
storeDirectory <- storeDirectory[order(storeDirectory$Category),]
storeDirectory$Rank <- NULL


completeListOfTransactions1 <- completeListOfTransactions
completeListOfTransactions1$Category <- NA

# Add categories to the list of transactions
##1. Matching for storenames with spaces replaced by regex for one or more whitespaces
# Matching transaction description to storename in directory using a vectorized wrapper of grep
storeDirectory$positionmatch <- VectorizedGrep(
  pattern =  regex(str_replace_all(storeDirectory$StoreName,pattern = " ",replacement = "[:space:]?")),
  x = completeListOfTransactions$Description,
  ignore.case = T)

for(i in 1:nrow(storeDirectory))
{
  if(length(unlist(storeDirectory$positionmatch[i]))>0)
  {
    completeListOfTransactions1$Category[unlist(storeDirectory$positionmatch[i])] <- 
      storeDirectory$Category[i]
  }
  if(length(unlist(storeDirectory$positionmatch[i]))==0)
    next
}

##2. Matching for storenames as is
# Matching transaction description to storename in directory using a vectorized wrapper of grep
storeDirectory$positionmatch <- VectorizedGrep(
  pattern =  storeDirectory$StoreName,
  x = completeListOfTransactions$Description,
  ignore.case = T)

completeListOfTransactions1$Category2 <- NA
for(i in 1:nrow(storeDirectory))
{
  if(length(unlist(storeDirectory$positionmatch[i]))>0)
  {
    completeListOfTransactions1$Category2[unlist(storeDirectory$positionmatch[i])] <- 
      storeDirectory$Category[i]
  }
  if(length(unlist(storeDirectory$positionmatch[i]))==0)
    next
}

## Assigning category to transactions based ono results from the 2 cases above
completeListOfTransactions1$Category <- ifelse(test = is.na(completeListOfTransactions1$Category),
                                  yes = completeListOfTransactions1$Category2,
                                  no = completeListOfTransactions1$Category)
                                  
completeListOfTransactions1$Category2 <- NULL
completeListOfTransactions <- completeListOfTransactions1
rm(completeListOfTransactions1)

## Adding extra columns for ease of processing:
# Month of transaction
completeListOfTransactions$Month <- format(as.Date(completeListOfTransactions$`Transaction Date`,"%b %d"),"%b")
# Year of transaction
completeListOfTransactions$Year <- as.integer(year(today()))

if(sum(as.integer(unique(completeListOfTransactions$Month) %in% c("Dec","Jan")))==2)
{
completeListOfTransactions$Year <- ifelse(completeListOfTransactions$Month=="Dec",
                                          completeListOfTransactions$Year-1,
                                          completeListOfTransactions$Year)
}


## Write the categorized transactions into a csv file
write.csv(completeListOfTransactions, 
          "Credit Card Statements_Categorized.csv")

## Not writing it in xlsx format coz it writes some erroneous cells,
##  in the category and amount fields

