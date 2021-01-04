#install "here" package to work with my working directory independent of operating system
if (!require(here)) install.packages("here"); library(here)  

#define the polutantmean function

polutantmean <- function(directory, pollutant, id) {
  
  #iterate over the ids to read in the correct dataframes
  
 means <- lapply(seq_along(id), function(i){
   
   #some if statements to make sure that 0s are added where necessary to the id
   
  if(id[i] < 10){
   df <- read.csv(here::here("Data", "R Programming",directory, paste(0,0,id[i],".csv", sep = "")))
  }else if(id[i] < 100){
    df <- read.csv(here::here("Data", "R Programming",directory, paste(0,id[i],".csv", sep = "")))
  } else{
    df <- read.csv(here::here("Data", "R Programming",directory, paste(id[i],".csv", sep = "")))
  }
  
   #calculate the mean value of the polutants, ignoring NAs
   
  return(mean(df[,c(pollutant)], na.rm = T))
})
 
 #get a vector of means
 
 mean(unlist(means))
}



polutantmean <- function(directory, pollutant, id) {
  
  #iterate over the ids to read in the correct dataframes
  
  list <- lapply(seq_along(id), function(i){
    
    #some if statements to make sure that 0s are added where necessary to the id
    
    if(id[i] < 10){
      df <- read.csv(here::here("Data", "R Programming",directory, paste(0,0,id[i],".csv", sep = "")))
    }else if(id[i] < 100){
      df <- read.csv(here::here("Data", "R Programming",directory, paste(0,id[i],".csv", sep = "")))
    } else{
      df <- read.csv(here::here("Data", "R Programming",directory, paste(id[i],".csv", sep = "")))
    }
    
    #calculate the mean value of the polutants, ignoring NAs
    
    return(df)
  })
  
  #get a vector of means
  
  #bound_df <- as.data.frame(matrix(unlist(list), ncol = 4, byrow = F))
  
  return(list)
}

# testing function with a vector of id

polutantmean(directory = "specdata", pollutant = "nitrate", id = 110:112)

#testing function with a value for id

polutantmean(directory = "specdata", pollutant = "nitrate", id = 10)


#exercise

polutantmean("specdata", "sulfate", 1:10)

head(polutantmean("specdata", "nitrate", 70:72))

polutantmean("specdata", "sulfate", 34)

polutantmean("specdata", "nitrate", 1:270)
