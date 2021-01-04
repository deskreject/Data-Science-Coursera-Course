#install "here" package to work with my working directory independent of operating system
if (!require(here)) install.packages("here"); library(here)  

#define the function

complete <- function(directory, id) {
  
  #create a list of dataframes by iterating over the predefined ids
  
  list <- lapply(seq_along(id), function(i){
    
    # sum code to make sure the 0s are added to the file name where necessary
    
    if(id[i] < 10){
      df <- read.csv(here::here("Data", "R Programming",directory, paste(0,0,id[i],".csv", sep = "")))
    }else if(id[i] < 100){
      df <- read.csv(here::here("Data", "R Programming",directory, paste(0,id[i],".csv", sep = "")))
    } else{
      df <- read.csv(here::here("Data", "R Programming",directory, paste(id[i],".csv", sep = "")))
    }
    
    #count the number of observations for which there are no NAs in a row
    
    nobs <- sum(is.na(df[,2] & df[,3]) == F )
    
    return(cbind(id[i],nobs))
  })
  
  # create a data frame from the list objects above
  
  df_complete <- as.data.frame(matrix(unlist(list), ncol = 2, byrow = T))

  #rename the columns according to exercise
  
names(df_complete) = c("id", "nobs")

#return the data frame

return(df_complete)


}

#test the function
complete(directory = "specdata", id = 5:12)  

#exercises

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs) 

RNGversion("3.5.1")
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
