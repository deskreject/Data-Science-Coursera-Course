##### ------------------- package loading and data reading ---------------- #####

Sys.setenv(LANG = "en")

library(tidyverse)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)  
if (!require(here)) install.packages("here"); library(here)  

# read in the data from the csv

outcomes_df <- read.csv(here::here("Data",
                                   "course_2_week_4",
                                   "outcome-of-care-measures.csv"))
hospital_df <- read.csv(here::here("Data",
                                   "course_2_week_4",
                                   "hospital-data.csv"))

##### --------------------- Ex 1: looking at the data ---------------------- #####

str(outcomes_df)

#change some of the relevant variables into numeric

outcomes_df[,11] <- as.numeric(outcomes_df[,11])

#plot histogram of outcome for heart attack

hist(outcomes_df[,11])

##### ---------------------- Ex 2: create function that spits out best hopspital ------ ####

#### Notes
## takes two arguments, outcome measure and state
## returns name of hospital for lowest outcome measure
## outcome can be any of "heat attack", "heart failure" or "pneumonia"
## should have a "stop" statement that quits function if state or outcome doesn't match



best <- function(state, outcome) {
  
  
  ## Read outcome data
  df <- read.csv(here::here("Data",
                            "course_2_week_4",
                            "outcome-of-care-measures.csv"))
  
  ## Check that state and outcome are valid
  
  potential_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!state %in% df$State){
    stop("state name not found")
  }
  
  if(!outcome %in% potential_outcomes){
    stop("outcome name not found")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  best_hospital <- if(outcome == "heart attack") {
    df %>% filter(state == State) %>%
      filter(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
                                                                              na.rm = T)) %>%
      select(Hospital.Name)
    
  } else if(outcome == "heart failure"){
    df %>% filter(state == State) %>%
      filter(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)  == min(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), 
                                                                                            na.rm = T)) %>%
      select(Hospital.Name)
  } else if(outcome == "pneumonia"){
    df %>% filter(state == State) %>%
      filter(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia )  == min(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ), 
                                                                                         na.rm = T)) %>%
      select(Hospital.Name)
  }
  
  ## rate
  return(sort(best_hospital$Hospital.Name))
}

# test the function
best("TX", "heart failure")

best("NY", "heart attack")

best("MD", "heart attack")

best("MD", "pneumonia")

best("BB", "heart attack")

best("NY", "hert attack")


##### ---------------------- Ex 3: create function that spits out hopspital according to ranking position ------ ####

rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  
  df <- read.csv(here::here("Data",
                            "course_2_week_4",
                            "outcome-of-care-measures.csv"))
  
  #filter down to relevant columns
  df_refined <- df[, c("Hospital.Name", 
                       "State",
                       "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  
  #rename the column names
  names(df_refined) <- c("Hospital.Name", 
                         "State",
                         "heart.attack",
                         "heart.failure",
                         "pneumonia")
  
  #reformat the column
  df_refined[,c(3:5)] <- apply(df_refined[,c(3:5)],2,as.numeric)
  
  ## Check that state, outcome and num are valid
  
  potential_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!state %in% df$State){
    stop("state name not found")
  }
  
  if(!outcome %in% potential_outcomes){
    stop("outcome name not found")
  }
  
  
  if(outcome == "heart attack"){
    num_hospitals <- df_refined %>% 
      filter(State == state) %>%
      dplyr::select(Hospital.Name, State, heart.attack) %>%
      filter(is.na(heart.attack) == F) %>%
      summarise(n = n())
  }else if(outcome == "heart failure"){
    num_hospitals <- df_refined %>% 
      filter(State == state) %>%
      dplyr::select(Hospital.Name, State, heart.failure) %>%
      filter(is.na(heart.failure) == F) %>%
      summarise(n = n())
  }else{
    num_hospitals <- df_refined %>% 
      filter(State == state) %>%
      dplyr::select(Hospital.Name, State, pneumonia) %>%
      filter(is.na(pneumonia) == F) %>%
      summarise(n = n())
  }
  
  #transform the num to a number
  
  if(num == "best"){
    num <- 1
  } else if(num == "worst"){
    num <- max(num_hospitals$n)
  }
  
  if(num > num_hospitals$n){
    stop("NA, try different num argument")
}
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  
    if(outcome == "heart attack"){
      ranking <- df_refined %>% 
        filter(State == state) %>%
        arrange(Hospital.Name) %>%
      arrange(heart.attack) %>%
        mutate("rank" = rank(heart.attack, ties.method = "first"))
    }else if(outcome == "heart failure"){
      ranking <- df_refined %>% 
        filter(State == state) %>%
        arrange(Hospital.Name) %>%
      arrange(heart.failure) %>%
        mutate("rank" = rank(heart.failure, ties.method = "first"))
    }else{
      ranking <- df_refined %>% 
        filter(State == state) %>%
        arrange(Hospital.Name) %>%
      arrange(pneumonia) %>%
        mutate("rank" = rank(pneumonia, ties.method = "first"))
    }
  
 
  
  return(ranking %>% filter(rank == num))
  
  
}

#test the function
rankhospital("TX", "pneumonia", 4)

rankhospital("MD", "heart attack", "worst")

rankhospital("MN", "heart attack", 5000)


##### ---------------------- Ex 4: create function that spits out hospitals by state with specified ranking ------ ####

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  df <- read.csv(here::here("Data",
                            "course_2_week_4",
                            "outcome-of-care-measures.csv"))
  
  #filter down to relevant columns
  df_refined <- df[, c("Hospital.Name", 
                       "State",
                       "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  
  #rename the column names
  names(df_refined) <- c("Hospital.Name", 
                         "State",
                         "heart.attack",
                         "heart.failure",
                         "pneumonia")
  
  #reformat the column
  df_refined[,c(3:5)] <- apply(df_refined[,c(3:5)],2,as.numeric)
  
  ## Check that state, outcome and num are valid
  
  potential_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  
  if(!outcome %in% potential_outcomes){
    stop("outcome name not found")
  }
  

  
  ## For each state, find the hospital of the given rank
  
  df_hospital_name <- lapply(df_split, function(x){
    
    if(outcome == "pneumonia"){
      df_split_ranked <- x %>% 
        arrange(Hospital.Name) %>%
        filter(is.na(pneumonia) == F) %>%
        arrange(pneumonia)  %>%
        mutate("rank" = rank(pneumonia, ties.method = "first"))
    } else if(outcome == "heart failure") {
      df_split_ranked <- x %>% 
        arrange(Hospital.Name) %>%
        filter(is.na(heart.failure) == F) %>%
        arrange(heart.failure)  %>%
        mutate("rank" = rank(heart.failure, ties.method = "first"))
    }else{
      df_split_ranked <- x %>% 
        arrange(Hospital.Name) %>%
        filter(is.na(heart.attack) == F) %>%
        arrange(heart.attack)  %>%
        mutate("rank" = rank(heart.attack, ties.method = "first"))
    }
    
    if(num == "worst"){
      df_split_ranked[length(df_split_ranked[,1]), 1]
    } else if(num == "best"){
      df_split_ranked[1, 1]
    } else if(num > df_split_ranked[,1]){
      return(NA)
    }else{
      df_split_ranked[num,1]
    }
  })
  
  
  unique(df_refined[,2])
  
  hosptial_names_comp <- as.data.frame(unlist(df_hospital_name))
  hosptial_names_comp$state <- sort(unique(df_refined[,2]))
  
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return(hosptial_names_comp)
}


#testing function

head(rankall("heart attack", 20), 10)

tail(rankall("pneumonia", "worst"), 3)

tail(rankall("heart failure"), 10)

# the exam

best("SC", "heart attack")

best("NY", "pneumonia")

best("AK", "pneumonia")

rankhospital("NC", "heart attack", "worst")

rankhospital("WA", "heart attack", 7)

rankhospital("TX", "pneumonia", 10)

rankhospital("NY", "heart attack", 7)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
