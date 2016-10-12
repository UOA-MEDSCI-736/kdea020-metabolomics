#NoRAND functions for data import and processing
#K. Deane-Alder, 2016
#Licensed under CC-BY 3.0 NZ

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


orderA <- InjOrder$injection.order
maxnRows <- max(nRows)
finalorderSamples <- c(orderA[1:maxnRows])

# - Sort out batches and injection order stuff
MetabInjOrder <- as.numeric(InjOrder$injection.order)
InjOrderNames <- as.character(InjOrder$Name)
InjTime <- InjOrder$Acquired
Inj.df <- data.frame(Names = InjOrderNames, Order = MetabInjOrder, Time = InjOrder$Acquired, Case.control = InjOrder$Case.control)

InjFinal <- Inj.df[ !(is.na(InjOrder$Case.control)), ] #instead of removing stuff that isn't C/S, use an attrib of the data frame

LogMeasurements <- log10(Metab) #Perform log transformation on measurements
LogMeasurements <- as.matrix(LogMeasurements) #Coerce the vector into a matrix for later use

# - Doing logs and stuff
LogMeasurements <- log10(Metab)
LogMeasurements <- as.matrix(LogMeasurements)

InjFinal$Names <- gsub("^C(\\d)$", "C0\\1", InjFinal$Names) #need to turn C1 -> C01 etc. for parity between Metab and InjOrder
InjFinal$Names <- gsub("^S(\\d)$", "S0\\1", InjFinal$Names) #for S1 -> S01 etc.

MetabRowNamesDataFrame <- data.frame(Names = rownames(Metab))
InjectionOrderForMatch <- as.data.frame(InjFinal)

#Rowmatch will only return non-null values if the grep = true. therefore if Lorder has length=83 (Length==nColumns) and is filled with non-null values, test has passed.

RowMatch <- function(rr) {
  ForMatch <- as.character(MetabRowNamesDataFrame$Names[rr])
  if(grep(ForMatch, InjectionOrderForMatch$Names, value = TRUE) == MetabRowNamesDataFrame$Names[rr]) {
    zz <- grep(ForMatch, InjectionOrderForMatch$Names, value = FALSE)
    MatrixToOrder <- c(InjectionOrderForMatch$Names[rr], InjectionOrderForMatch$Order[zz])
    # cat(InjectionOrderForMatch$Names[rr], "\t", InjectionOrderForMatch$Order[zz], "\n")
  }
  sort(MatrixToOrder)
}

nrowsMetabNames <-dim(MetabRowNamesDataFrame)[1]
Lorder <- 0
for (i in 1:nrowsMetabNames) {
  Lorder[i] <- RowMatch(i)
}
Lorder <- as.numeric(Lorder)

ShowAndTell <- function(i) { #this function produces the full set of graphs for a given compound, where i is the rownumber of that compound in Metab[]
  mkSingleGraphLog(i)
  DoCorrection.Linear(i)
  DoCorrection.Break(i)
  cat(CompoundNames[,i], "\n", "Type covar. p-value:", pval.mod3[i], "\n", "Q-values for each model:", "\n", "Linear:", qvals.mod1[i], "\n", "Step:", qvals.mod2[i], "\n", "Linear covar. with Type:", qvals.mod3[i], "\n", "DOC1:", DOC.mod1[i], "\n", "DOC2:", DOC.mod2[i], "\n")
} #and gives descriptive output as required

UserInput <- function() {
  nameinput <- readline(prompt = "Enter a compound name, or press [RETURN] to list all compounds: ") #prompt user for input
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
    readline("Enter a positive integer from the list above to select from multiple matching compounds: ") -> SelectiveInput #this is where we ask for input in the form of a +ve integer, corresponding to above matches
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
