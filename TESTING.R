#TESTING
#Write a battery of tests for functions and source this file at the end.
#then use test_file(file.r) in main script to import tests
#test that vectors include their expected values and attributes?
#I.e. is a matrix, is the right length, or has ONLY numeric variables, or has BOTH numerics and strings...

read.csv("NLDL Data/TEK0000.CSV", header = FALSE) -> myfile
library(testthat)

V4 <- myfile[,4]
V5 <- myfile[,5]

test1 <- function(i) {
  if (i[1] == myfile[1,4]) {
    print("Test passed.")
  }
  else
    print("Test failed!")
}

context("File imports")

test_that("R has imported the columns from the file correctly", {
  expect_equal(V4, myfile[,4])
})