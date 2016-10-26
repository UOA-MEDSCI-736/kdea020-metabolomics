#NoRAND main script - RUN THIS!
#K. Deane-Alder, 2016
#Licensed under CC-BY 4.0

#This script has the main functions and input code for NoRAND
#Secondary functions are imported as necessary

Metab <- 0
InjOrder <- 0
myfile <- 0

CheckInstalled <- installed.packages() #in order to check whether the user has bioconductor installed, which is a dependency of this script
                                       #justification for this approach is in the readme.md on GIT

if( sum (grepl ("qvalue", CheckInstalled) ) == 0 ) { #if qvalue is not installed...
  NoQvalue <- c("Bioconductor qvalue package not installed! Please install it.", "\n", "For instructions to install Qvalue package, go to https://bioconductor.org/packages/release/bioc/html/qvalue.html")
  stop(NoQvalue) #Print the above message, and we want to terminate the program...
}

library(qvalue) #import qvalue library

cat("Novel R script for the Analysis of Non-randomised Data (NoRAND) is a tool for producing interpretive statistics and modeling experimental error in non-randomised metabolomics datasets.", "\n", "\n")


WD <- getwd() #set working directory for the project as wherever the script is opened
cat("Working directory is:", WD, "\n", "\n")

# For citation:
citeQ <- "John D. Storey with contributions from Andrew J. Bass, Alan Dabney and David Robinson (2015). qvalue: Q-value estimation for false
  discovery rate control. R package version 2.4.2. http://github.com/jdstorey/qvalue"
cat("This script uses the Q-value package from the open-source project Bioconductor:", "\n", citeQ, "\n")

#BEGIN BASIC USER INPUT CODE

library(testthat) #import testthat library

InputFiles <- function() { #Init a function to allow for the user to input their own data files
  default <- readline(prompt = "Use example dataset? Y / N: ") #ask the user if they want to use the default dataset file names, or to choose their own
  default <- ifelse(grepl("[^YNyn]", default),NA,default) #Check that the input is either Y or N (not case-sensitive) - set to NA if not
  if (is.na(default)) { #If not Y/N...
    cat("'", default, "'", "is not a Y or N! Please run NoRAND again.") #Throw up an error message
  }
  else #If it's a Y...
    if(grepl("Y", default, ignore.case = TRUE) == TRUE) {
      myfile <<- as.character("example data/Hair.csv") #Default main dataset name
      read.csv('example data/Hair.csv') ->> Metab #Assigning to a data frame as a global
      read.csv('example data/inj_order_SGA_hair.csv') ->> InjOrder #load injection order data file with equipment status
      source ("Modules/datahandlingfunctions.R")
      source ("Modules/plotfunctions.R")
      source ("Modules/statfunctions.R")
      UserInput() #Run the UserInput function
    }
  if(grepl("N", default, ignore.case = TRUE) == TRUE) { #But if it's an N (the use wants to specify their own dataset)...
    cat("Please choose a .CSV file containing the main dataset.", "(Press ENTER to continue)", "\n") #ask them what they want to input
    wait <- readline() #wait on their input
    MetabChoose <- file.choose() #open up R's inbuilt file selection GUI
    cat("Please choose a .CSV file containing the injection order for the samples.", "(Press ENTER to continue)", "\n")
    wait <- readline()
    InjChoose <- file.choose()
    myfile <<- as.character(MetabChoose) #assigning files as globals for input...
    cat(MetabChoose, InjChoose, myfile)
    Metab <<- read.csv(MetabChoose)
    InjOrder <<- read.csv(InjChoose)
    source ("Modules/datahandlingfunctions.R")
    source ("Modules/plotfunctions.R")
    source ("Modules/statfunctions.R")
    UserInput()
    GetAllR2()
  }
}

InputFiles()

source ("Modules/TESTING.R")