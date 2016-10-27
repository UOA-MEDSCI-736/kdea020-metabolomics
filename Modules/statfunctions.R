#NoRAND statistics functions
#K. Deane-Alder, 2016
#Licensed under CC-BY 4.0

library(qvalue) #Imports qvalue

# - Making linear models

# - Model 2: Before/After "break" (gap in days during experiment)
IsBreak <- 36 #Please change this to reflect a break in measurements in your data, if not using the example data
#I.e. in the example data break was found at Sample C36 (Lorder[36] = 100), So everything <=36 in the list is before the break (= TRUE)
Lorder.m <- as.matrix(Lorder) #coerce to a matrix
RowLorder.m <- row(Lorder.m) #subset rows out of this matrix into a new matrix for later use
BreakSplit <- Lorder <= IsBreak #The split is defined as a logical vector  
BreakSplitC <- subset(BreakSplit, Type == "C")

#a logical value that will factor Lorder into two levels at the split

LogMeasurementsC <- subset(LogMeasurements, Type == "C") #making subsets of LogMeasurements containing only 
LogMeasurementsS <- subset(LogMeasurements, Type == "S") #cases or controls, for brevity in later functions

Stairstep.df=data.frame(BreakSplitC = rep(0, length(LogMeasurements) ) ) #this creates an "empty" dataframe of the same length of LogMeasurements
Stairstep.df=data.frame(BreakSplitC = BreakSplit) #so that the predict() function has appropriate input

#From here on out, just looking at log(relative intensity) as a scaling normalisation step

mod2 <- lm(LogMeasurementsC ~ BreakSplitC) #init the stairstep model as mod2, using before/after break controls as a divider
summ.mod2 <- summary(mod2) #assign the summary of the model statistics to a vector
predict.mod2 <- predict(mod2, Stairstep.df = Stairstep.df) #create a vector for predictions using the earlier dataframe

R2.Break <- 0 #init a new vector for storing R^2s

AllR2.Break <- function(i) { #where the input is the number of the compound name, and the break R^2
  Reg <- regexpr("r.squared = ", summ.mod2[i]) #the regular expresssions are necessary
  RegStart <- Reg+12                           #as summary()$r.squared seems to run into problems with the break model (mod2)
  RegEnd <- Reg+28
  R2.Break[i] <- substring(grep("[\\d+]", summ.mod2[i], ignore.case = TRUE, value = TRUE), RegStart,RegEnd) #put the digits corresponding to the R^2 into a string
  cat(signif(as.numeric(R2.Break[i]),3), "\n" ) #reduce to a sensible 3 significant figures, using cat for output
}

R2.Linear <- 0
#Doing the same as above but for linear model R^2s, which don't need such silly measures
AllR2.Linear <- function(i) {
  mod1 <- lm(LogMeasurements[,i]~Lorder, subset=Type=='C')
  cat(CompoundNames[,i], "\t", signif(summary(mod1)$r.squared,3), "\t" )
}

GetAllR2 <- function() { #A function to output the R^2s for both models comparatively in a .tdt (tab-delimited) compatible format
  newfile <- "exports/NoRAND_R2.tdt" #relative file path and name to export, you may change this to what you like
  file.create(newfile) #create the file as described in the newfile vector above
  sink(newfile) #this diverts output (i.e. from cat() statements) into the file, rather than the terminal
  cat("Compound", "\t", "LINEAR R^2", "\t", "BREAK R^2", "\n") #creates column headers for the .tdt table
  for (i in seq_along(CompoundNames)) { #for each compound...
    cat(AllR2.Linear(i)) #print the results for the linear R2s
    cat(AllR2.Break(i)) #print the results for break model R2s
  }
  sink() #terminate diverting the output, so it returns to printing to terminal rather than the file
}

pval.mod1 <- 0 #initing vectors for use in later function
pval.mod2 <- 0
pval.mod3 <- 0
DOC.mod1 <- 0
DOC.mod2 <- 0


for(i in 1:nColumns) { #this way the results for all compounds, for each model, will be stored in globally-accessible matrices for later use
  mod1 <- lm(LogMeasurements[,i] ~ Lorder, subset = Type == 'C') #recreating model 1 in this fashion (for all compounds)
  mod2 <- lm(LogMeasurements[,i] ~ BreakSplit, subset = Type == 'C') #same for model 2 (stairstep)
  mod3 <- lm(LogMeasurements[,i] ~ Type + Lorder) #and for model 3 "confounder" - a linear regresssion as in mod1, but with case/control split as primary variable
  pval.mod1[i] <- summary(mod1)$coef[2,4] #produce pvalues for each model
  pval.mod2[i] <- summary(mod2)$coef[2,4]
  pval.mod3[i] <- summary(mod3)$coef[2,4]
  DOC.mod1[i] <- summary(mod1)$coef[1,3] #and use one of the column summaries (t sign) to get the direction of change (between cases/controls) of the compound for each model
  DOC.mod2[i] <- summary(mod2)$coef[1,3]
}

qvals.mod1 <- signif(qvalue(pval.mod1, lambda=0.01)$qval, 3) #computes Benjamini-Hochburg qvalue for mod1, limits to 3 significant figures for readability
qvals.mod2 <- signif(qvalue(pval.mod2, lambda=0.01)$qval, 3) #"" "" for mod 2
qvals.mod3 <- signif(qvalue(pval.mod3, lambda=0.01)$qval, 3) #"" ""for mod 3