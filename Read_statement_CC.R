## Deciphering Credit card statements
#### Part 1: Reading the credit card statements

# Clearing work environment variables and checking r version used in session
# rm()
# sessionInfo()

#### Library calls -----------
## installations: uncomment if needed
# install.packages("pdftools")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("plyr")


library(pdftools)
library(tidyverse)
library(lubridate)
library(stringr)
library(plyr)


#### ---------- Reading the credit card statements

# Setting directory to the credit card statements folder
setwd('your directory path here')
## !!!! Don't forget to put your directory path here before you execute!!!

# Listing files in the directory 
filesInDir <- file.info(list.files(full.names = T))
# Getting the file name with the most recent modified time
recentFileName <- rownames(filesInDir)[which.max(filesInDir$mtime)]

# Reading in the most recent credit card statement pdf file 
creditCardStatement <- pdf_text(recentFileName)

# Creating a list of month names in order to 
#   filter rows of PDF text with transactions
monthNamesList <- c(1:12)
monthNamesList <- month(monthNamesList,label=T)
monthNamesList <- as.character(monthNamesList)
monthNamesList <- str_to_upper(monthNamesList)

# Create a table of transactions to add transaction df from each page of 
#  credit card statement to it
creditCardStatementDf <- data.frame()

# Performing operations on each page of credit card statement
for(i in 1:(length(creditCardStatement)))
{
  currentCCStatementPage <- creditCardStatement[i] %>%
    readr::read_lines()                      ##---- replace 1 with i
  
  ## Fixing issue with a particular transaction line
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "PAYMENT - THANK YOU",
                replacement = "    PAYMENT - THANK YOU") %>%
    strsplit(split="    ") 
  
  # Flagging rows of the credit card statement page that have transaction data
  rowsToKeep <- apply(Vectorize(grepl, "pattern")(monthNamesList, str_sub(sapply(currentCCStatementPage, "[[", 1),start=1,end=3)),
                      1,
                      function(x) which(x)[1])
  rowsToKeep <- !is.na(rowsToKeep)
  
  # Filtering rows of the credit card statement page that have transaction details
  transactionTable <- currentCCStatementPage[rowsToKeep]
  
  ### Breaking out of the loop if the page has no transactions
  if(length(transactionTable)==0)
    break
  
  # Flagging all elements with zero length
  elementsToKeep <- sapply(transactionTable,function(el) as.integer(as.logical(str_length(el))))
  
  ## Check
  rowsumElements <- sapply(elementsToKeep,sum)
  print(rowsumElements)
  
  
  # Removing empty elements from transactionTable
  newTransactionTable <- list()
  for(x in 1:length(transactionTable))
  {
    vectorToKeep <- c()
    rowsumElements <- sum(elementsToKeep[[x]])
    for(y in 1:length(transactionTable[[x]]))
    {
      ## Keeping an element of the list if flag is 1
      if(elementsToKeep[[x]][y])
      {
        ## Appending the element to new list if flag is 1, after grep correcting 
        vectorToKeep <- append(vectorToKeep,transactionTable[[x]][y])
      }
    }
    newTransactionTable <- append(newTransactionTable,list(vectorToKeep))
  }   
  
  # Converting list of transactions to a dataframe
  transactiondf <- plyr::ldply(newTransactionTable)
  
  # Adding appropriate column names to the dataframe
  colnames(transactiondf) <- c("Transaction Date", "Posting Date", "Description", "Amount")
  
  # Cleaning up values in the dataframe
  ## Trim each string
  transactiondf$'Transaction Date' <- str_trim(transactiondf$`Transaction Date`)
  transactiondf$'Posting Date' <- str_trim(transactiondf$`Posting Date`)
  transactiondf$Description <- str_trim(transactiondf$Description)
  transactiondf$Amount <- str_trim(transactiondf$Amount)
  
  # Converting dates to date format from string
  transactiondf$'Transaction Date' <- as.Date(transactiondf$'Transaction Date', "%B %d")
  transactiondf$`Posting Date` <- as.Date(transactiondf$`Posting Date`, "%B %d")
  
  # Converting the amount column to numeric format ($ is an escape character, hence the \\ operator)
  transactiondf$Amount <- str_replace(transactiondf$Amount,pattern = "\\$", replacement = "")
  
  # Appending transactiondf to creditCardStatementDf
  creditCardStatementDf <- rbind.data.frame(creditCardStatementDf,transactiondf)

}
