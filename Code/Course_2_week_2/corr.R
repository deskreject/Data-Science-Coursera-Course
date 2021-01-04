corr <- function(directory, threshold = 0) {
  
  #read in all the files in the directory that end with .csv
  
  temp = list.files(path = here::here("Data", "R Programming", directory), pattern="*.csv")
  myfiles = lapply(here::here("Data", "R Programming", "specdata",temp), read.csv)
    
  #create a list of correlations by iterating over all files read in from the directory
  
  correlation_vec <- lapply(seq_along(myfiles), function(i){
    
    #check which monitors match the minimum threshold of complete observations
    
    if(sum(is.na(myfiles[[i]][,2] & myfiles[[i]][,3]) == F ) >= threshold){
      
      # calculate the correlations between the polutants, ignoring NAs
      
      cor(myfiles[[i]][,2],myfiles[[i]][,3], use = "na.or.complete")
      
      #if no match, return nothing
      
    }else{
      
      return(NULL)
      
    }
  })
    
  #unlist to get a vector of correlation
  
    return(unlist(correlation_vec))
  }
  

corr(directory = "specdata", threshold = 1)

#exercises

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
