library(otvPlots)
context("Run PlotWrapper function")
drugSASDate <- read.csv("../testthat/drugSASDate.csv")

test_that("At most one of sortVars and sortFn is passed in", {
  expect_error(PlotWrapper(dataFl = "../testthat/drugSASDate.csv",
		  					dateNm = "date", sortVars = c("age", "residencecity")))
})

test_that("varNms is a subset of sortVars", {
  expect_error(PlotWrapper(dataFl = drugSASDate, dateNm = "date", 
  							sortVars = c("age", "residencecity"), varNms = c("age")))
}) 

test_that("Names of the variables are transformed correctly", {
  out <- PrepData(dataFl = "../testthat/drugRDate.csv", dateNm = "date",
               	  dateGp = "months", dateGpBp = "quarters")
  expect_equal(names(out)[6], "residencecity")
})
  
test_that("Incorrect file input when prepData is False", {
  expect_error(PlotWrapper(dataFl = drugSASDate, dateNm = "date",
                          prepData = FALSE))
}) 
  
  
  
  
  
  
  
  
  
  
  

