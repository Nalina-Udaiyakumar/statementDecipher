## Deciphering Credit card and chequing account statements - part 2
#### ---------- Getting list of supermarkets, big box stores, restaurants and gas stations in canada to match

#### Library calls ---------- 
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("plyr")
# install.packages("rvest")
# install.packages("xml2")

library(tidyverse)
library(lubridate)
library(stringr)
library(plyr)
library(xml2)
library(rvest)


## Getting list of supermarkets, big box stores, restaurants and gas stations in Canada to match

# Supermarkets - https://en.wikipedia.org/wiki/List_of_supermarket_chains_in_Canada
# Superstores - https://en.wikipedia.org/wiki/List_of_superstores#Canada
# Gas stations - https://en.wikipedia.org/wiki/Category:Gas_stations_in_Canada
# Restaurants - https://en.wikipedia.org/wiki/List_of_Canadian_restaurant_chains
# Clothing stores - https://en.wikipedia.org/wiki/List_of_Canadian_clothing_store_chains
# Stores by category - https://en.wikipedia.org/wiki/List_of_Canadian_stores

## Scraping supermarket names -----------------------------------
## Explicit call to library XML to call the specific functions in XML, not rvest and xml2
library(XML)
url <- "https://en.wikipedia.org/wiki/List_of_supermarket_chains_in_Canada"
# Reading the html file and parsing it
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
# Extracting the list of supermarket names
listOfSupermarkets <- xpathSApply(parsed_doc, path = '//*[@id="mw-content-text"]/div/ul/li', xmlValue)
listOfSupermarkets <- str_replace_all(listOfSupermarkets,"\\\n","/")
listOfSupermarkets <- str_split(listOfSupermarkets,"/")
listOfSupermarkets <- unlist(listOfSupermarkets)
listOfSupermarkets <- str_trim(listOfSupermarkets)

## Removing rows that are erroneously scraped into the list of supermarket names
VectorizedGrep <- Vectorize(FUN = grep, vectorize.args = "pattern")
elementsToRemove <- VectorizedGrep(
  pattern=c('operates', 'List of', 'operatives'), 
  listOfSupermarkets)
elementsToRemove <- unlist(elementsToRemove)
listOfSupermarkets <- listOfSupermarkets[-elementsToRemove]

## Scraping big-box store names -----------------------------------
## Explicit call to library XML to call the specific functions in XML, not rvest and xml2
library(XML)
url <- "https://en.wikipedia.org/wiki/List_of_superstores"
#  Reading the html file and parsing it
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
# Extracting the list of supermarket names
listOfSuperstores <- xpathSApply(parsed_doc, path = '/html/body/div[3]/div[3]/div[5]/div[1]/ul[4]', xmlValue)
listOfSuperstores <- str_replace_all(listOfSuperstores,"\\\n","/")
listOfSuperstores <- str_split(listOfSuperstores,"/")
listOfSuperstores <- unlist(listOfSuperstores)
listOfSuperstores <- str_trim(listOfSuperstores)

## Removing rows that are erroneously scraped into the list of superstore names
elementsToRemove <- VectorizedGrep(
  pattern=c('including', 'defunct'), 
  listOfSuperstores)
elementsToRemove <- unlist(elementsToRemove)
listOfSuperstores <- listOfSuperstores[-elementsToRemove]


## Scraping gas station names -----------------
webpage <- read_html("https://en.wikipedia.org/wiki/Category:Gas_stations_in_Canada")
listOfGasStations <- webpage %>%
  html_nodes("li") %>%
  html_text()
listOfGasStations <- listOfGasStations[3:26]
# Convert list of gas stations to a dataframe and append it to store directory
listOfGasStations <- data.frame(listOfGasStations)
colnames(listOfGasStations) <- "StoreName"
# Adding the category "Gas" for the above store names
listOfGasStations$Category <- "Fuel"


## Scraping clothing stores -----------------
## Explicit call to library XML to call the specific functions in XML, not rvest and xml2
library(XML)
url <- "https://en.wikipedia.org/wiki/List_of_Canadian_clothing_store_chains"
#  Reading the html file and parsing it
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
# Extracting the list of clothing store  names
listOfClothingStores <- xpathSApply(parsed_doc, path = '/html/body/div[3]/div[3]/div[5]/div/ul/li/a', xmlValue)
listOfClothingStores <- str_trim(listOfClothingStores)
# Convert list of clothing stores to a dataframe and append it to store directory
listOfClothingStores <- data.frame(listOfClothingStores)
colnames(listOfClothingStores) <- "StoreName"
# Adding the category for the clothing stores
listOfClothingStores$Category <- "Clothing" 


## Scraping all category store names from wiki page of Canadian chains -----------------
## Explicit call to library XML to call the specific functions in XML, not rvest and xml2
library(XML)
url <- "https://en.wikipedia.org/wiki/List_of_Canadian_stores"
## ------------- Marked for future upgrade ;) -------------------
listOfCustomStores <- read.csv("Custom list of stores.csv", stringsAsFactors = FALSE,
                               header = TRUE)


## Remove repetitions and creating store directory ---------------------------
# Remove repetitions of store names in supermarket and superstore categories 
#  and add both lists to store directory

# Removing repetitions in list of supermarkets and superstores
listOfSupermarkets <- unique(listOfSupermarkets)
listOfSuperstores <- unique(listOfSuperstores)
elementsToRemove <- VectorizedGrep(listOfSuperstores,listOfSupermarkets)
elementsToRemove <- unlist(elementsToRemove)
listOfSupermarkets <- listOfSupermarkets[-elementsToRemove]

# Removing repetitions in list of clothing stores and superstores
listOfClothingStores <- unique(listOfClothingStores)
listOfSuperstores <- unique(listOfSuperstores)
elementsToRemove <- VectorizedGrep(listOfSuperstores,listOfClothingStores)
elementsToRemove <- unlist(elementsToRemove)
listOfClothingStores <- listOfClothingStores[-elementsToRemove]

# Converting the list of supermarkets into a dataframe
listOfSupermarkets <- data.frame(listOfSupermarkets)
colnames(listOfSupermarkets) <- "StoreName"
# Adding the category "Groceries" for the supermarkets
listOfSupermarkets$Category <- "Groceries"


## Marking drug stores in the list of superstores as "Healthcare - pharmacy"
# Creating a list of drug stores from the list of superstores
elementsToKeep <- grep("Drug", listOfSuperstores)
listOfDrugStores <- listOfSuperstores[elementsToKeep]
# Removing the drug stores from the list of superstores
listOfSuperstores <- listOfSuperstores[-elementsToKeep]

## Converting the list of supermarkets into a dataframe
listOfSuperstores <- data.frame(listOfSuperstores)
colnames(listOfSuperstores) <- "StoreName"
# Adding the category "Shopping" for the superstores
listOfSuperstores$Category <- "Shopping"

## Converting the list of drug stores into a dataframe
listOfDrugStores <- data.frame(listOfDrugStores)
colnames(listOfDrugStores) <- "StoreName"
# Adding the category "Healthcare - Pharmacy" for the drug stores
listOfDrugStores$Category <- "Healthcare - Pharmacy"


## Create Store Directory------------
# Creating a store directory dataframe with all store names and categories, 
#  to be used as lookup for categorizing each transaction, add supermarkets to it
storeDirectory <- listOfSupermarkets

# Appending the list of superstores to the store directory
storeDirectory <- rbind.data.frame(storeDirectory,listOfSuperstores)
# Appending the list of drug stores to the store directory
storeDirectory <- rbind.data.frame(storeDirectory,listOfDrugStores)
# Appending list of gas stations to store directory
storeDirectory <- rbind.data.frame(storeDirectory,listOfGasStations)
# Appending list of clothing stores to store directory
storeDirectory <- rbind.data.frame(storeDirectory,listOfClothingStores)

# Eliminate duplicates with the custom list of store additions and add them to directory
VectorizedGrep(listOfCustomStores,storeDirectory$StoreName)
storeDirectory <- rbind.data.frame(storeDirectory,customListOfStores)
storeDirectory <- unique(storeDirectory)
