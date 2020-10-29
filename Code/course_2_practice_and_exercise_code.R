
data <- read.csv("C:/R work/Data-Science-Coursera-Course/Data/R Programming/hw1_data.csv") 

#check head of data
head(data)
#check the tail of the data
tail(data)

#check 47th row
data[47,1]

#check number of missings in ozone (1st) column
sum(is.na(data$Ozone))

#check the mean
mean(data$Ozone, na.rm = T)

#extract subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. 
which(data$Ozone > 31 & data$Temp >90)
 sub_data <- data[which(data$Ozone > 31 & data$Temp >90),]
#calculate the mean of solar.r
mean(sub_data$Solar.R, na.rm = T)

#What is the mean of "Temp" when "Month" is equal to 6? 
mean(data[which(data$Month == 6),4])

#What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?
 
max(data[which(data$Month == 5),1], na.rm = T) 

#swirl installation
install.packages("swirl")
  install.packages("bitops")
  install.packages("testthat")
library(swirl)

  ls()
  #start swirl
swirl()
