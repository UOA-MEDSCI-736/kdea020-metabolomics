#Ver. 0.3
#Script (C) 2016 K. Deane-Alder
#Licensed under CC-BY 3.0 NZ

#This script has the main functions and input code for the metabolomics project
#Secondary functions are imported as necessary

library(qvalue) #import qvalue library
source ("project_0-2.R")
# source("TESTING.R")

Metab <- 0
InjOrder <- 0
myfile <- 0

CheckInstalled <- installed.packages()

if( sum (grepl ("qvalue", CheckInstalled) ) == 0 ) {
  NoQvalue <- c("Bioconductor qvalue package not installed! Please install it.", "\n", "For instructions to install Qvalue package, go to https://bioconductor.org/packages/release/bioc/html/qvalue.html")
  stop(NoQvalue)#we want to terminate the program...
}

# check and store file here


WD <- getwd() #set working directory for the project as wherever the script is opened
# cat("Working directory is:", WD)

# For citation:
citeQ <- "John D. Storey with contributions from Andrew J. Bass, Alan Dabney and David Robinson (2015). qvalue: Q-value estimation for false
  discovery rate control. R package version 2.4.2. http://github.com/jdstorey/qvalue"
# cat("This script uses the Q-value package from the open-source project Bioconductor:", "\n", citeQ, "\n")

#BEGIN BASIC USER INPUT CODE


library(testthat) #import testthat library

UserInput <- function() {
  nameinput <- readline(prompt = "Enter a compound name: ") #prompt user for input
  nameinput <- ifelse(grepl("[^A-Za-z0-9]", nameinput),NA,nameinput) # if the input isn't a text string (including digits), mark as NA
  if (is.na(nameinput)) { #if it doesn't find a match in the compound names...
    cat("'", nameinput, "'", "is not a valid text string for matching! Please try again.") #throw up an error for the user
  }
  else #if it DOES find a match
    collookup <- grep(nameinput, CompoundNames, ignore.case = TRUE, value = FALSE) #looks up column compound names, returns column number
  # now need to sort out if grep returns more than one value (i.e. search "Alanine", get Phenylalanine AND Alanine)
  alphabeta <- grep(nameinput, CompoundNames, ignore.case = TRUE, value = TRUE) #get the corresponding match names from a repeat grep
  nMatches <- length(collookup) #store "length" (i.e. number) of lookups
  
  if (nMatches == 1) { #if length is equal to 1, there's only one option
    ShowAndTell(collookup) #print the plots
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
  ShowAndTell(collookup[SelectiveInput]) #print the plots
  }
  if(nMatches == 0) { #if no matches found...
    cat("No matches found for string", "'", nameinput, "'") #throw up an error message
  }
}

InputFiles <- function() {
  default <- readline(prompt = "Use default dataset? Y / N: ")
  default <- ifelse(grepl("[^YNyn]", default),NA,default)
  if (is.na(default)) {
    cat("'", default, "'", "is not a Y or N! Please try again.")
  }
  else
    if(grepl("Y", default, ignore.case = TRUE) == TRUE) {
      myfile <<- as.character("Hair.csv")
      read.csv('Hair.csv')->>Metab
      read.csv('inj_order_SGA_hair.csv')->>InjOrder #load injection order data file with equipment status
      UserInput()
    }
  if(grepl("N", default, ignore.case = TRUE) == TRUE) {
    cat("Please choose a file containing the main dataset.", "(Press ENTER to continue)", "\n")
    wait <- readline()
    MetabChoose <- file.choose()
    cat("Please choose a file containing the injection order for the samples.", "(Press ENTER to continue)", "\n")
    wait <- readline()
    InjChoose <- file.choose()
    myfile <<- as.character(MetabChoose)
    cat(MetabChoose, InjChoose, myfile)
    Metab <<- read.csv(MetabChoose)
    InjOrder <<- read.csv(InjChoose)
    UserInput()
  }
}

InputFiles()

MetabInfo<-Metab #dump Metab into MetabInfo
CompoundNames<-MetabInfo[1]
CompoundNames<-t(CompoundNames)
Metab<-Metab[,-c(1)] #subtract compound names
Metab<-t(Metab) #Values from rows into columns, so sample names are on rows
Metab<-as.data.frame(Metab)
Missing<-is.na(Metab) #Allows to check for NA values (reports TRUE/FALSE for each value)
nColumns<-dim(Metab)[2] #no. of columns is set to dimensions of Metab columns
nRows<-dim(Metab)[1]
# - So now data is set up so that Metab[2,4] = [S04, Compound 4 value] etc.

# - Get C or S for each sample so can do just of one group
Type<-substr(rownames(Metab),1,1) #Grab first letter of rownames
Type[Type=='X']<-'Sample' #Define as either C or S for each sample
Type<-factor(Type) #Coerce vector Type to a factor with two levels (C and S)

#UserInput()