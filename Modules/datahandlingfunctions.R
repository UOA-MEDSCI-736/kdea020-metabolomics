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
