#TESTING
#K. Deane-Alder, 2016
#Licensed under CC-BY 4.0

library(testthat) #Testthat package by Hadley, W.

#Input tests
test_that("R has read the file correctly", { #testing for reading
  teststring <- readLines(myfile) #coerces the .CSV into a string
  splitted <- strsplit(teststring, ",") #splits the string by commas into a list
  expect_equal(as.numeric(splitted[[2]][2]), Metab[1,1]) #checks to see if the list member corresponding to the expected value present in the data frame matches that value
} )


test_that("The type stratification for case/control has the correct number of values", {
  howmanytypes <- 2 #there should be two levels to Type (Case/Control aka C/S)
  expect_equal(howmanytypes, length(levels(Type))) #therefore we would expect the length of levels(Type) to equal 2.
})


test_that("The lines that convert C1 -> C01 (etc.) for parity between the main data file and the injection order file work", {
  Check <- as.character("C01") #coerce C01 (Control sample 1) to character for checking
  expect_equal(Check, InjFinal$Names[1]) #We expect that the first value in InjFinal$Names should be C01
} )


test_that("The injection ordering function has correctly ordered the samples", {
  MatchCheck <- RowMatch(33)[2] #The second column of output from the RowMatch() function is the sample number as.character (i.e. "C01")
  expect_equal(MatchCheck, InjFinal$Names[33]) #testing that the output of RowMatch(), for a given row, matches the correct order of Samples from InjFinal
} )


test_that("The (double) vector Lorder is storing the properly ordered output values from RowMatch()", {
 MatchCheck <- as.numeric(RowMatch(33)[1]) #the first column of output from RowMatch() is the numerical order of the corresponding sample (in column 2) will be used by Lorder
 expect_equal(MatchCheck, Lorder[33]) #If correct, the row numbers for values from RowMatch(CompoundNo.)[1] and Lorder should match.
} )

test_that("R has correctly produced tabulated R^2 output", {
  FileLocation <- paste("../", as.character(newfile)) #the test will therefore still work even if the user has changed the filename and path for export
  print(FileLocation)
  Filecheck <- read.delim(FileLocation)
})