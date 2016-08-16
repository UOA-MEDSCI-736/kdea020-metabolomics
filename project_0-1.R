# - Basic instructions and data import
#Eventually need to set working directory for portability

# - If you just want to test the output, input ' mkSingleGraph(1) ' (where 1 may be replaced by the "number" of the compound you want to plot, 1-47)
# - Or, you can now use ' mkGraphName("a string of text") ' and the script will plot what you entered, allowing multiple matches (and selection thereof). Not case sensitive.

WD <- "~/Desktop/736/Project/" #Setting working directory - eventually will take user input for this
setwd(WD)
cat("Working directory is:", WD)

read.csv('Hair.csv')->Metab #load first data file with main readings
read.csv('inj_order_SGA_hair.csv')->InjOrder #load injection order data file with equipment status

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
Type<-factor(Type) #levels?


# - To get statistics for later use... etc.

meanCompound <- t(apply(Metab, 2, mean))
sdCompound <- t(apply(Metab, 2, sd))

orderA <- InjOrder$injection.order
maxnRows <- max(nRows)
finalorderSamples <- c(orderA[1:maxnRows])


# - Sort out batches and injection order stuff
nBatch=4 #just for now, see next line
#nBatch = SOMETHINGFROM InjOrder (i.e. gap in times >=2h?)
#MetabInjOrder <- #need to get injection order and stuff
#use str(InjOrder)???
MetabInjOrder <- as.numeric(InjOrder$injection.order)
InjOrderNames <- as.character(InjOrder$Name)
InjTime <- InjOrder$Acquired
Inj.df <- data.frame(Names = InjOrderNames, Order = MetabInjOrder, Time = InjOrder$Acquired, Case.control = InjOrder$Case.control)
#([], ) trailing comma = do row stuff

# InjFinal <- Inj.df[ !( grepl("T", Inj.df$Names) | (grepl("B", Inj.df$Names) ) ), ]
InjFinal <- Inj.df[ !(is.na(InjOrder$Case.control)), ] #instead of removing stuff that isn't C/S, use an attrib of the data frame

# - Doing logs and stuff
LogMeasurements<-log(Metab)
LogMeasurements<-as.matrix(LogMeasurements)
CD<-matrix(nColumns, nBatch)
#Batch<-SampInfo$Batch - Get from InjOrder, crossref w/ Sample Names
#Cbind
#polyorder.mat<-matrix(nColumns, nBatch) #do I actually need this?

# - Organising data
InjFinal2 <- InjFinal

InjFinal$Names <- gsub("^C(\\d)$", "C0\\1", InjFinal$Names) #need to turn C1 -> C01 etc. for parity between Metab and InjOrder
InjFinal$Names <- gsub("^S(\\d)$", "S0\\1", InjFinal$Names) #for S1 -> S01 etc.

MetabRowNamesDataFrame <- data.frame(Names = rownames(Metab))
InjectionOrderForMatch <- as.data.frame(InjFinal)

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

TimeMatch <- function(tt) {
  ForMatch <- as.character(MetabRowNamesDataFrame$Names[tt])
  if(grep(ForMatch, InjectionOrderForMatch$Names, value = TRUE) == MetabRowNamesDataFrame$Names[tt]) {
    zz <- grep(ForMatch, InjectionOrderForMatch$Names, value = FALSE)
    MatrixToOrder <- c(InjectionOrderForMatch$Names[tt], as.character(InjectionOrderForMatch$Time[zz]))
    cat(MatrixToOrder, "\n")
    cat(InjectionOrderForMatch$Names[tt], "\t", InjectionOrderForMatch$Time[zz], "\n")
  }
}


# - The below function makes a graph of non-log conc. vs samples
mkSingleGraph <- function(d) { #d = no. of compound you want to plot
  # - Plotting things out
  mod1 <- lm(Metab[,d]~Lorder, subset=Type=='C')
  titleRel <- c(CompoundNames[,d], "R^2 = ", signif(summary(mod1)$r.squared,3) )
  plot(Lorder, Metab[,d], main = titleRel, xlab = "Injection Order", ylab = "Relative Intensity", col=Type)
  # - Making a trendline
  abline(mod1)
  #Should probably include a legend for Type/Colouring
}

mkSingleGraphLog <- function(b) { #b = no. of compound you want to plot - log scale, and by injection order
  # - Plotting things out, inc. model
  mod1 <- lm(LogMeasurements[,b]~Lorder, subset=Type=='C')
  titleLog <- c(CompoundNames[,b], "R^2 = ", signif(summary(mod1)$r.squared,3) )
  plot(Lorder, LogMeasurements[,b], main = titleLog, xlab = "Injection Order", ylab = "Log Relative Intensity", col=Type)
  # - Making a trendline
      # linelog <- line(Lorder, LogMeasurements[,b])
      # abline(coef(linelog))
  abline(mod1)
}

#if you don't want to use numbers!
mkGraphName <- function() {
  nameinput <- readline(prompt = "Enter a compound name: ")
  nameinput <- ifelse(grepl("[^A-Za-z0-9]", nameinput),NA,nameinput) # if the input isn't a text string, including digits, mark as NA
  if (is.na(nameinput)) {
    cat("'", nameinput, "'", "is not a valid text string for matching! Please try again.")
  }
  else
  
  collookup <- grep(nameinput, CompoundNames, ignore.case = TRUE, value = FALSE) #looks up column compound names, returns column number
  # now need to sort out if grep returns more than one value (i.e. search "Alanine", get Phe and Ala)
  alphabeta <- grep(nameinput, CompoundNames, ignore.case = TRUE, value = TRUE) #get the corresponding match names from a repeat grep
  nMatches <- length(collookup) #store "length" (i.e. number) of lookups
  
  if (nMatches == 1) { #if length is equal to 1, there's only one option
    cat("Plot", alphabeta, "as log?", " ")
    readline(prompt = "Y / N: ") -> YorN
    if(grepl("Y", YorN, ignore.case = TRUE) == TRUE) {
    mkSingleGraphLog(collookup)
    }
    if(grepl("N", YorN, ignore.case = TRUE) == TRUE) {
      mkSingleGraph(collookup)
    }
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
    cat("Plot", alphabeta[SelectiveInput], "as log?", " ")
    readline(prompt = "Y / N: ") -> YorN
    if(grepl("Y", YorN, ignore.case = TRUE) == TRUE) {
      mkSingleGraphLog(collookup[SelectiveInput])
    }
    if(grepl("N", YorN, ignore.case = TRUE) == TRUE) {
    mkSingleGraph(collookup[SelectiveInput])
    }
  }
  if(nMatches == 0) { #if no matches found...
    cat("No matches found for string", "'", nameinput, "'") #throw up an error message
  }
}

# - Making linear models

# - Model 2: Before/After "break" (gap in days during experiment)
IsBreak <- 25 # this will be a single value: the row no. for the sample which is the first after the "break"
BreakSplit <- Lorder <= IsBreak #a logical value that will factor Lorder into two levels at the split

mkModel2 <- function(b) { 
  mod2 <- lm(Metab[,b] ~ BreakSplit, subset = Type == "C")
  cat(CompoundNames[,b], "R^2 = ", signif(summary(mod2)$r.squared,3))
}

mkModel2Log <- function(b) {
  mod2 <- lm(LogMeasurements[,b] ~ BreakSplit, subset = Type == "C")
  cat("LOG", CompoundNames[,b], "R^2 = ", signif(summary(mod2)$r.squared,3))
}

# - functions
modelBreak <- function() {
  nameinput <- readline(prompt = "Enter a compound name: ")
  nameinput <- ifelse(grepl("[^A-Za-z0-9]", nameinput),NA,nameinput) # if the input isn't a text string, including digits, mark as NA
  if (is.na(nameinput)) {
    cat("'", nameinput, "'", "is not a valid text string for matching! Please try again.")
  }
  else
    
  collookup <- grep(nameinput, CompoundNames, ignore.case = TRUE, value = FALSE) #looks up column compound names, returns column number
  # now need to sort out if grep returns more than one value (i.e. search "Alanine", get Phe and Ala)
  alphabeta <- grep(nameinput, CompoundNames, ignore.case = TRUE, value = TRUE) #get the corresponding match names from a repeat grep
  nMatches <- length(collookup) #store "length" (i.e. number) of lookups
  
  if (nMatches == 1) { #if length is equal to 1, there's only one option
      mkModel2(collookup)
      cat("\n")
      mkModel2Log(collookup)
  }
  if (nMatches > 1) {  #if more than one match was obtained...
    cat(nMatches, "matches found. Which option do you want to model?") #print a descriptive message asking for input
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
      mkModel2(collookup[SelectiveInput])
      cat("\n")
      mkModel2Log(collookup[SelectiveInput])
  }
  
  if(nMatches == 0) { #if no matches found...
    cat("No matches found for string", "'", nameinput, "'") #throw up an error message
  }
}

# Modeling all compounds and producing output:
#make a data frame
#cat, concisely, elements of dataframe (Compound Name, Linear R^2, Break R^2, new line) for print