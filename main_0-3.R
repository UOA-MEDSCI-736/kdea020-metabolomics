#Ver. 0.3
#Script (C) 2016 K. Deane-Alder
#This script has the main functions and input code for the metabolomics project
#Secondary functions are imported as necessary

library(qvalue)
#need to test that the library is loaded

WD <- getwd() #set working directory for the project as wherever the script is opened
# cat("Working directory is:", WD)

# For citation:
citeQ <- "John D. Storey with contributions from Andrew J. Bass, Alan Dabney and David Robinson (2015). qvalue: Q-value estimation for false
  discovery rate control. R package version 2.4.2. http://github.com/jdstorey/qvalue"
# cat("This script uses the Q-value package from the open-source project Bioconductor:", "\n", citeQ, "\n")

#BEGIN BASIC USER INPUT CODE