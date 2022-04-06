# statementDecipher

Kernel: R

Markdown: Absent

Logs: Absent

License: APACHE 2.0

Purpose: Reading credit card statements from a particular bank, that are in PDF format and analysing transactions to present spending pattern.

R libraries used:
pdftools
stringr
xlsx
xml2
rvest
XML
tidyverse
lubridate

Code files: 
Read_statement_CC.R : Reading the PDF credit card statment file and creating a dataframe of transactions in the recent statement.
Get_Store_Names.R : Scraping retail and major store names on the web; add custom list of stores and curating them to make a business directory to categorize each credit card transaction.
Match_Store_Name.R : Cleaning the transaction data and categorizing each transaction by matching business names to the description.
Charts_and_reports.R: Creating additional columns in the categorized credit card transaction data to aid summarising the data and creating plots to present spending pattern.

Library files:
Custom list of stores.csv : User-defined list of store names with their categories.

Misc files:
Sample CC.png : Sample image showing the structure of the credit card statement the codes are woking with.  
