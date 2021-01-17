##### ------------------- package loading and data reading ---------------- #####

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
                            "hospital-data.csv"))
  
  ## Check that state and outcome are valid
  
  
  ## Return hospital name in that state with lowest 30-day death
  best_hospital <- if(outcome == "hear attack") {
  min(best_hospital[,11])
    
  }
  
  ## rate
  best_hospital
}
