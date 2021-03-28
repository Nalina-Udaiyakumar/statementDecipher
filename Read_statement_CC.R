## Deciphering Credit card statements
#### Part 1: Reading the credit card statements

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

# Listing files in the directory 
filesInDir <- file.info(list.files(full.names = T))
# Getting the file name with the most recent modified time
# recentFileName <- rownames(filesInDir)[which.max(filesInDir$mtime)]
recentFileName <- rownames(filesInDir)[1]
print(recentFileName)

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
  
  ## Fixing issue with a couple of particular transaction line cases
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "PAYMENT - THANK YOU",
                replacement = "    PAYMENT - THANK YOU")
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "PAYBACK WITH POINTS",
                replacement = "    PAYBACK WITH POINTS")
  ## Introduce deliberate line breaks to separate transaction lines
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "NEW BALANCE",
                replacement = "\n\nNEW BALANCE")
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "CONTACT US",
                replacement = "\n\nCONTACT US")
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "Collect Outside North America",
                replacement = "\n\nCollect Outside North America")
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "Minimum payment",
                replacement = "\n\nMinimum payment")
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "Credit limit",
                replacement = "\n\nCredit limit")
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "Annual interest rates",
                replacement = "\n\nAnnual interest rates")
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "Cash advances",
                replacement = "\n\nCash advances")
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "CALCULATING YOUR BALANCE",
                replacement = "\n\nCALCULATING YOUR BALANCE")
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "Payments & credits",
                replacement = "\n\nPayments & credits")
  currentCCStatementPage <- currentCCStatementPage %>%
    str_replace(pattern = "Fees",
                replacement = "\n\nFees")
  
    
  # Splitting by "\n\n" to insert line breaks at the required positions
  currentCCStatementPage <- currentCCStatementPage %>%
    strsplit(split="\n\n")
  
  # Keep only the first element in each element of the list 
  currentCCStatementPage <- 
    lapply(currentCCStatementPage,
           function(x) x[[1]])
  
  ## ! Update: splitting after deliberately inserting spaces after transaction and posting dates
  # currentCCStatementPage <- currentCCStatementPage %>%
  #   strsplit(split="    ") 
   
  # Flagging rows of the credit card statement page that have transaction data
  rowsToKeep <- apply(Vectorize(grepl, "pattern")
                      (monthNamesList,str_sub(sapply(currentCCStatementPage, "[[", 1),start=1,end=3)),
                      1,
                      function(x) which(x)[1])
  rowsToKeep <- !is.na(rowsToKeep)
  
  # Filtering rows of the credit card statement page that have transaction details
  transactionTable <- currentCCStatementPage[rowsToKeep]
  
  ### Breaking out of the loop if the page has no transactions
  if(length(transactionTable)==0)
    break
  
  ## Insert spaces "    " wherever a particular column ends: 
  ##  after each date in 'MMM dd ' format- i.e transaction date and posting date, before $/-$ sign, 
  ##  and before the word "PAYMENT", since there is no space in this particular case
  transactionTable <- transactionTable %>%
    str_replace(pattern = "PAYMENT - THANK YOU",
                replacement = "    PAYMENT - THANK YOU")
  
  
  ## Inserting spaces in strings after transaction date: 
  # Creating regex to indentify the position of transaction date in a string
  transactionDateFormat <- paste(monthNamesList,collapse=" [0-9]{2} |")
  transactionDateFormat <- paste0(transactionDateFormat," [0-9]{2} ")
  transactionDateFormat <- regex(transactionDateFormat)
  
  # Locate the position of the posting date
  # Convert to data frame for ease of operation
  transactionTable1 <- data.frame(transactionTable)
  # colnames(transactionTable1)
  
  # Extracting the positions of the posted date as columns in the dataframe
  transactionTable1$transactionDatePosition2start <- 
    lapply(str_locate_all(transactionTable1$transactionTable,transactionDateFormat),
           function(x) as.integer(x[2,1]))
  transactionTable1$transactionDatePosition2end <- 
    lapply(str_locate_all(transactionTable1$transactionTable,transactionDateFormat),
           function(x) as.integer(x[2,2]))
  
  transactionTable1$transactionDatePosition2beforestart <- as.integer(transactionTable1$transactionDatePosition2start)-1
  transactionTable1$transactionDatePosition2afterend <- as.integer(transactionTable1$transactionDatePosition2end)+1
    
  ## Removing rows that do not have any posting date
  transactionTable1 <- transactionTable1[!is.na(transactionTable1$transactionDatePosition2end),]
  
  # Insert "    " after posting date
  transactionTable1$correctedData <- paste(str_sub(transactionTable1$transactionTable,
                                                   start = 1,
                                                   end = transactionTable1$transactionDatePosition2beforestart),
                                          str_sub(transactionTable1$transactionTable,
                                                   start = transactionTable1$transactionDatePosition2start,
                                                   end = transactionTable1$transactionDatePosition2end),
                                           "    ",
                                           str_sub(transactionTable1$transactionTable,
                                                   start = transactionTable1$transactionDatePosition2afterend,
                                                   end = str_length(transactionTable1$transactionTable))
                                           )
  
  ## The starting and ending position of the transaction date are always 1 and 7
  # Insert "    " after transaction date
  transactionTable1$correctedData <- paste(str_sub(transactionTable1$correctedData,
                                                                   start = 1,
                                                                   end = 7),
                                           "    ",
                                          str_sub(transactionTable1$correctedData,
                                                                   start = 8,
                                                                   end = str_length(transactionTable1$correctedData))
                                                 )
  
  
  ## Inserting spaces in strings before the $/-$ sign: 
  # Creating regex to indentify the position of transaction date in a string
  transactionAmountFormat <- "\\$[0-9]{1}"
  transactionAmountFormat <- regex(transactionAmountFormat)
  
  # Locate the position of the $ sign
  ## Getting only the first element of position vector for each element in the list
  transactionTable1$transactionAmountPositionstart <- 
    lapply(str_locate_all(transactionTable1$correctedData,transactionAmountFormat),
           function(x) as.integer(x[1,1]))
  
  transactionTable1$transactionAmountPositionbeforestart <- as.integer(transactionTable1$transactionAmountPositionstart)-3
  transactionTable1$transactionAmountPositionafterstart <- as.integer(transactionTable1$transactionAmountPositionstart)+1
  
 
  # Insert "    " before the $ or -$ sign
  transactionTable1$correctedData <- paste(str_sub(transactionTable1$correctedData,
                                                   start = 1,
                                                   end = transactionTable1$transactionAmountPositionbeforestart),
                                           "    ",
                                           str_sub(transactionTable1$correctedData,
                                                   start = transactionTable1$transactionAmountPositionbeforestart,
                                                   end = str_length(transactionTable1$correctedData))
                                           )
  
  # Converting transactionTable back to a list
  transactionTable <- as.list.data.frame(transactionTable1$correctedData)
  
  # Removing the unwanted data frame transactionTable1
  rm(transactionTable1)
  
  # Splitting by "    " to get the 4 columns of data in the transaction table
  transactionTable <- transactionTable %>%
    strsplit(split="    ") 
  
  # Flagging all elements with zero length
  elementsToKeep <- sapply(transactionTable,function(el) str_length(el))
  elementsToKeep <- sapply(transactionTable,function(el) as.integer(as.logical(str_length(str_trim(el)))))
  
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
  
  # Converting dates to date format from string - not converting. I like the %b-%d format. Don't want year in the format.
  # transactiondf$'Transaction Date' <- as.Date(transactiondf$'Transaction Date', "%B %d")
  # transactiondf$`Posting Date` <- as.Date(transactiondf$`Posting Date`, "%B %d")
  
  # Converting the amount column to numeric format ($ is an escape character, hence the \\ operator)
  transactiondf$Amount <- str_replace(transactiondf$Amount,pattern = "\\$", replacement = "")
  
  # Appending transactiondf to creditCardStatementDf
  creditCardStatementDf <- rbind.data.frame(creditCardStatementDf,transactiondf)
}
# completeListOfTransactions <- creditCardStatementDf ## only when reading the first file
completeListOfTransactions <- rbind.data.frame(completeListOfTransactions,creditCardStatementDf)


# Writing the complete list of transactions to an excel file
write.xlsx(completeListOfTransactions, "Credit Card Statements.xlsx", sheetName = "Sheet1",
           col.names = TRUE, row.names = FALSE, append = FALSE)

# Writing as csv file:
write.csv(completeListOfTransactions, "CC Statements CSV.csv")
