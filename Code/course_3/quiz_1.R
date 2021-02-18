#install "here" package to work with my working directory independent of operating system
if (!require(here)) install.packages("here"); library(here)  
if (!require(janitor)) install.packages("janitor"); library(janitor)  
if (!require(xlsx)) install.packages("xlsx"); library(xlsx)  
if (!require(XML)) install.packages("XML"); library(XML)  
if (!require(data.table)) install.packages("data.table"); library(data.table)  


#question 1 - How many properties are worth $1,000,000 or more?

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv",
              destfile = here::here("Data", "course_3", "housing_microdata.csv"))

df_1 <- read.csv(here::here("Data", "course_3", "housing_microdata.csv"))

#check out the variables
str(df_1)

tabyl(df_1$VAL)

# ---------- answer
nrow(df_1[which(df_1$VAL == 24), ])

# question 2 - Use the data you loaded from Question 1. 
# Consider the variable FES in the code book. Which of the "tidy data" principles does this variable violate? 

# ------------- answer = Tidy data has one variable per column. 

# question 3 - Download the Excel spreadsheet on Natural Gas Aquisition Program here: 



dat <- read.xlsx(here::here("Data", "course_3", "data_NGAP.xlsx"), "NGAP Sample Data",
                  rowIndex = 18:23,
                  colIndex = 7:15)
# --------- answers

sum(dat$Zip*dat$Ext,na.rm=T)

# question 4 - Read the XML data on Baltimore restaurants from here:

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml",
              destfile = here::here("Data", "course_3", "baltimore_restaurants.xml"))

# load in xml

doc <- xmlTreeParse(here::here("Data", "course_3", "baltimore_restaurants.xml"))

#check xml

rootnode <- xmlRoot(doc)

#check names
# ---------- no answer

# question 5 - The American Community Survey distributes downloadable data about United States communities

# read in with fread
DT <- fread(here::here("Data", "course_3", "housing_microdata.csv"))

# -------------- answer

start <- Sys.time()

tapply(DT$pwgtp15,DT$SEX,mean)

End <- Sys.time()

End - start
