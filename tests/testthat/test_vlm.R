library(otvPlots)
context("Run the main function: vlm")
drugSASDate <- read.csv("../testthat/drugSASDate.csv")

test_that("At most one of sortVars and sortFn is passed in", {
  expect_error(vlm(dataFl = "../testthat/drugSASDate.csv",
		  					dateNm = "date", sortVars = c("age", "residencecity")))
})

test_that("varNms is a subset of sortVars", {
  expect_error(vlm(dataFl = drugSASDate, dateNm = "date", 
  							sortVars = c("age", "residencecity"), varNms = c("age")))
}) 

test_that("Incorrect file input when prepData is False", {
  expect_error(vlm(dataFl = "../testthat/drugRDate.csv", dateNm = "date",
                           prepData = FALSE))
}) 

test_that("selectCols and dropCols together give an error", {   
	expect_error(vlm("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y",
 		   selectCols = c("age", "balance", "date", "weight"),
 		   dropCols = c("default"), varNms = c("age")))

	expect_error(vlm("../testthat/rawData.rda", dateNm = "date", weightNm = "weight",
 		   dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y",
 		   selectCols = c("age", "balance", "date", "weight"),
 		   dropCols = c("default")))
})
