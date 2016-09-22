#Ver. 0.3
#Script (C) 2016 K. Deane-Alder
#Licensed under CC-BY 3.0 NZ

#This script has the main functions and input code for the metabolomics project
#Secondary functions are imported as necessary

library(qvalue)
library(testthat)

#need to test that the library is loaded?

source("project_0-2.R")
source("TESTING.R")

WD <- getwd() #set working directory for the project as wherever the script is opened
# cat("Working directory is:", WD)

# For citation:
citeQ <- "John D. Storey with contributions from Andrew J. Bass, Alan Dabney and David Robinson (2015). qvalue: Q-value estimation for false
  discovery rate control. R package version 2.4.2. http://github.com/jdstorey/qvalue"
# cat("This script uses the Q-value package from the open-source project Bioconductor:", "\n", citeQ, "\n")

#BEGIN BASIC USER INPUT CODE

UserInput <- function() {
  nameinput <- readline(prompt = "Enter a compound name: ")
  nameinput <- ifelse(grepl("[^A-Za-z0-9]", nameinput),NA,nameinput) # if the input isn't a text string, including digits, mark as NA
  if (is.na(nameinput)) {
    cat("'", nameinput, "'", "is not a valid text string for matching! Please try again.")
  }
  else
    collookup <- grep(nameinput, CompoundNames, ignore.case = TRUE, value = FALSE) #looks up column compound names, returns column number
  # now need to sort out if grep returns more than one value (i.e. search "Alanine", get Phenylalanine AND Alanine)
  alphabeta <- grep(nameinput, CompoundNames, ignore.case = TRUE, value = TRUE) #get the corresponding match names from a repeat grep
  nMatches <- length(collookup) #store "length" (i.e. number) of lookups
  
  if (nMatches == 1) { #if length is equal to 1, there's only one option
    ShowAndTell(collookup)
    }
  if (nMatches > 1) {  #if more than one match was obtained...
    cat(nMatches, "matches found. Which option do you want to plot?") #print a descriptive message asking for input
    state <- 1 #just an iterator variable for the following loop
    for(i in collookup){ #for as many values as collookup has (i.e. no. of matches returned)
      cat("\n", " ", alphabeta[state], " ", "(",state,")") #print the match name, and number (for user input), each on a new line
      state <- state+1 #increase the iterator so we move through the values
    }
    readline("Enter a positive integer: ") -> SelectiveInput #this is where we ask for input in the form of a +ve integer, corresponding to above matches
    SelectiveInput <- ifelse(grepl("[^0-9]", SelectiveInput),NA,as.integer(SelectiveInput)) #if they don't give us a +ve integer, mark as NA
    while (is.na(SelectiveInput)) { #As long as the input is set as "NA"
      readline("Error. Please enter a positive integer: ") -> SelectiveInput #Throw up an error and ask them to try again
      SelectiveInput <- ifelse(grepl("[^0-9]", SelectiveInput),NA,as.integer(SelectiveInput)) #Please get it right this time, user
    }
  ShowAndTell(collookup[SelectiveInput])
  }
  if(nMatches == 0) { #if no matches found...
    cat("No matches found for string", "'", nameinput, "'") #throw up an error message
  }
}

UserInput()