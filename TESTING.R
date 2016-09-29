#TESTING
#Write a battery of tests for functions and source this file at the end.
#then use test_file(file.r) in main script to import tests
#test that vectors include their expected values and attributes?
#I.e. is a matrix, is the right length, or has ONLY numeric variables, or has BOTH numerics and strings...

library(testthat)

#Input tests
test_that("R has read the file correctly", { #testing for reading
  teststring <- readLines(myfile) #coerces the .CSV into a string
  splitted <- strsplit(teststring, ",") #splits the string by commas into a list
  expect_equal(as.numeric(splitted[[2]][2]), Metab[1,1]) #checks to see if the list member corresponding to the expected value present in the data frame matches that value
} )

#

test_that("The injection ordering function has correctly ordered the samples", {
  MatchCheck <- RowMatch(1)[2]
  identical(MatchCheck, InjFinal$Names[1])
} )