#### Part 4: Analysing spending and creating charts and reports

# Clearing work environment variables and checking R version used in session
# rm()
# sessionInfo()

#### Library calls -----------
# install.packages("stringr")


library(stringr)
library(tidyverse)
library(lubridate)


#### ---------- Reading the credit card statements

# Setting directory to the credit card statements folder
setwd('!!!! Your directory path here !!!!!!!')

# Read required files from the directory
completeListOfTransactions <- 
  read.csv("Credit Card Statements_Categorized.csv", header = TRUE, stringsAsFactors = F)

# Remove the serial numbers column if present
if(!is.null(completeListOfTransactions$X))
  completeListOfTransactions$X <- NULL

## Reassigning the column names of the dataframe to ensure it is unchanged 
##  after reading and writing into csv 
colnames(completeListOfTransactions) <- c("Transaction Date", "Posting Date", 
                                          "Description", "Amount", "Category",
                                          "Month", "Year")


# Checking for NAs and NULLs in date and amount columns
any(is.na(completeListOfTransactions$`Transaction Date`))
any(is.na(completeListOfTransactions$`Posting Date`))
any(is.na(completeListOfTransactions$Amount))
any(is.na(completeListOfTransactions$Year))


# Knowing the statements in brief
unique(completeListOfTransactions$Month)
unique(completeListOfTransactions$Year)
dim(completeListOfTransactions)
table(completeListOfTransactions$Month,completeListOfTransactions$Year)

## Plot: 
## 1. Spending and payments every month - horizontal bar chart 
## 2. spending by category each month - pie chart
## 3. Top 5 spends by amount and category - heatmap or list 
## 4. Groceries trend each month and week - bar chart
## 5. utilities spend each month - line chart
##

## Creating date columns
# Creating a list of month names in order to 
#   filter rows of PDF text with transactions
completeListOfTransactions$Tdate <- str_replace(string=completeListOfTransactions$`Transaction Date`,
                                                pattern = completeListOfTransactions$Month,
                                                replacement = "")
completeListOfTransactions$Tdate <- str_replace(string=completeListOfTransactions$Tdate,
                                                pattern = "-",
                                                replacement = "")
completeListOfTransactions$Date <- with(completeListOfTransactions,str_c(Tdate,Month,Year,sep="-"))
completeListOfTransactions$Date <- as.Date(completeListOfTransactions$Date,format = "%d-%b-%Y")
# Creating a column for concatenated month and year - calling it Period - for graphs
completeListOfTransactions$Period <- format(completeListOfTransactions$Date,"%b-%y")
completeListOfTransactions$Day <- format(completeListOfTransactions$Date,"%a")
completeListOfTransactions$Week <- format(completeListOfTransactions$Date,"%V")
## Correcting week number of first week in Jan as 1 and not 53
completeListOfTransactions$Week <- ifelse(completeListOfTransactions$Week>50 & completeListOfTransactions$Month=="Jan",
                                          "01",completeListOfTransactions$Week)
class(completeListOfTransactions$Week)
completeListOfTransactions$Week <- as.integer(completeListOfTransactions$Week)

# Setting right the type of required columns
class(completeListOfTransactions$Period)
class(completeListOfTransactions$Description)
class(completeListOfTransactions$Category)
class(completeListOfTransactions$Amount)
## Convert the amount column to numeric/number
completeListOfTransactions$Amount <- str_trim(completeListOfTransactions$Amount)
completeListOfTransactions$Amount <- str_replace_all(string = completeListOfTransactions$Amount,
                                                     pattern = ",", replacement = "")
completeListOfTransactions$Amount <- as.numeric(completeListOfTransactions$Amount)

# order the data by transaction date
completeListOfTransactions1 <- completeListOfTransactions
completeListOfTransactions <- completeListOfTransactions[order(completeListOfTransactions$Year,
                                                               as.Date(completeListOfTransactions$`Transaction Date`,format="%d-%b")),]
completeListOfTransactions$SN <- 1:nrow(completeListOfTransactions)
# correcting NAs in the Category
completeListOfTransactions$Category[which(is.na(completeListOfTransactions$Category))] = "Undefined"
# Add cashflow column to biforcate money in and out each month
completeListOfTransactions$Cashflow <- ifelse(completeListOfTransactions$Category=="Credit card payment" | 
                                                completeListOfTransactions$Category=="Banking fee","Payments",
                                              "Spending")

## Plot 1:  Spending and payments every month - horizontal stacked bar chart
plotdata <- completeListOfTransactions %>%
  group_by(Period) %>%
  filter(Category=="Credit card payment") %>%
  summarize(Payment=sum(Amount), SN=max(SN)) %>%
  arrange(by=SN)

plotdata2 <- completeListOfTransactions %>%
  group_by(Period) %>%
  filter(Category!="Credit card payment") %>%
  summarize(Spending=sum(Amount), SN=max(SN)) %>%
  arrange(by=SN)

ggplot(completeListOfTransactions, aes(y=Amount, x=Period,fill=Cashflow )) +
  geom_bar(position="stack", stat="identity")+
  geom_text(data=plotdata,aes(x=Period,y=Payment,label=Payment,fill=NULL),
            nudge_y = 10)+
  geom_text(data=plotdata2,aes(x=Period,y=Spending,label=Spending,fill=NULL),
            nudge_y = 10)+
  coord_flip()

## Plot 2: Spending by category for the recent month - pie chart
# Subset the spending data from the complete list of transactions
plotdata <- completeListOfTransactions[which(completeListOfTransactions$Cashflow=="Spending"),]
# Filter spending for the recent month
plotdata <- plotdata[order(plotdata$Year,
                      as.Date(plotdata$`Transaction Date`,format="%d-%b")),]
plotdata$SN <- 1:nrow(plotdata)
maxSN <- max(plotdata$SN)
recentPeriod <- plotdata$Period[which(plotdata$SN==maxSN)]
plotdata <- plotdata[which(plotdata$Period==recentPeriod),]

ggplot(plotdata, aes(x = "", y=Amount, fill = factor(Category))) +
   geom_bar(width = 1, stat = "identity") +
   theme(axis.line = element_blank(),plot.title = element_text(hjust=0.5), legend.position="bottom") +
   labs(fill="class", x=NULL,y=NULL,title="Category spending in the recent month")+
   coord_polar(theta = "y", start=0)

## Plot 3: Top 5 spends by amount and category - multi facet bar chart
# Filter spending data and create a pivot table for each category in each period
plotdata <- completeListOfTransactions %>%
  filter(Cashflow=="Spending") %>%
  group_by(Period,Category) %>%
  summarize(Spending=sum(Amount)) %>%
  arrange(Period,desc(Spending)) 
# Create a subrank variable that creates numbers top spending each month
plotdata <- plotdata %>%
  group_by(Period) %>%
  mutate(Ranks=order(Spending, decreasing=TRUE))
# Keep only top 5 spending in each period
plotdata <- plotdata[which(plotdata$Ranks<=5),]

# Bar chart
ggplot(plotdata, aes(y=Spending, x=Category,fill=Category )) +
  geom_bar(position="stack", stat="identity")+
  labs(x=NULL,y=NULL,title="Top 5 spending categories each month")+
  facet_wrap(~ Period)+
  guides(nrow=3)

## Plot 4: Grocery shopping trend each month and week - dot plot
# Filer grocery transactions and extract transaction date to separate column
plotdata <- completeListOfTransactions[which(completeListOfTransactions$Category=="Groceries"),]
plotdata$Tdate <- str_sub(plotdata$`Transaction Date`,start=1,end=2)
plotdata$Tdate <- str_replace_all(plotdata$Tdate,"-","")
plotdata$Tdate <- as.integer(plotdata$Tdate)
unique(plotdata$Tdate)

ggplot(data=plotdata, aes(x=Tdate, y=Amount, colour=Description))+
  geom_point(shape=2, size=2, stroke=2)+
  labs(x="Transaction Date",y="Amount",title="Grocery spending each month")+
  theme(legend.position="bottom")+
  facet_wrap(~Period)

#Grocery spending by month, week number and day
## By month
plotdata <- completeListOfTransactions %>%
  filter(Category %in% c("Groceries","Shopping-Walmart")) %>%
  group_by(Month,Year) %>%
  mutate(Spending=sum(Amount))

plotdata <- plotdata %>%
  select(Period, Month, Spending, Year)

plotdata <- unique(plotdata)

ggplot(data=plotdata, aes(x=Month, y=Spending))+
  geom_point(shape=10, size=2, stroke=2, colour="Red")+
  labs(x="Month",y="Grocery spending",title="Grocery spending each month")+
  facet_wrap(~Year)

## By week number
plotdata <- completeListOfTransactions %>%
  filter(Category %in% c("Groceries","Shopping-Walmart")) %>%
  group_by(Week,Year) %>%
  mutate(Spending=sum(Amount))
plotdata <- plotdata %>%
  select(Period, Week, Spending, Year)

plotdata <- unique(plotdata)
class(plotdata$Week)
plotdata$Week <- as.integer(plotdata$Week)
plotdata$Season <- ifelse(plotdata$Week<=9,"Winter",
                          ifelse(plotdata$Week>=10 & plotdata$Week<=23,"Spring",
                                 ifelse(plotdata$Week>=24 & plotdata$Week<=36,"Summer",
                                        ifelse(plotdata$Week>=37 & plotdata$Week<=49,"Fall",
                                               "Winter"))))

ggplot(data=plotdata, aes(x=Week, y=Spending, colour=Season))+
  geom_point(shape=13, size=2, stroke=2)+
  labs(x="Week",y="Grocery spending",title="Grocery spending each week")+
  theme(legend.position="bottom")+
  facet_wrap(~Year)

## By Day of the week
plotdata <- completeListOfTransactions %>%
  filter(Category %in% c("Groceries","Shopping-Walmart")) %>%
  group_by(Day, Year) %>%
  mutate(Spending=sum(Amount))
plotdata <- plotdata %>%
  select(Day, Spending, Year)
plotdata <- unique(plotdata)

ggplot(data=plotdata, aes(x=Day, y=Spending))+
  geom_point(shape=17, size=2, stroke=2, colour="Red")+
  labs(x="Day of the week",y="Grocery spending",title="Most likely to go grocery shopping on")+
  facet_wrap(~Year)
