library(otvPlots)
context("Prepare Data")
data(bankData);  setDT(bankData)
is.cntns <- function(x)  inherits(x, "nmrcl") #!#previous name: "cntns"
is.dscrt <- function(x)  inherits(x, "ctgrl") #!# previous name: "dscrt"
is.IDate <- function(x)  inherits(x, "IDate")
is.binary <- function(x) uniqueN(na.omit(x)) == 2

test_that("Names of the variables are transformed correctly", {
  out <- PrepData(dataFl = "../testthat/drugRDate.csv", dateNm = "date",
               	  dateGp = "months", dateGpBp = "quarters")
  expect_equal(names(out)[6], "Residence.City")
})

test_that("Parse SAS (eg. 07Apr2017) default date format correctly", {
  out <- PrepData(dataFl = "../testthat/drugSASDate.csv", dateNm = "date", 
                  dateGp = "months", dateGpBp = "quarters")
  expect_false(all(is.na(out[, "date"])), 'Fail to parse SAS date format')
  }
)

test_that("Parse R (eg. 2017-04-17) default date format correctly", {
  out <- PrepData(dataFl = "../testthat/drugRDate.csv", dateNm = "date", 
                dateGp   = "months", dateGpBp = "quarters")
  expect_false(all(is.na(out[, "date"])), 'Fail to parse R date format')
  }
)

test_that("Incorrect date format creates warnings with csv input file", {
  expect_warning(
  	PrepData("../testthat/rawData.csv", dateNm = "date", weightNm ="weight", 
  		dateGp = "weeks", dateGpBp = "weeks"), "Formatting date as ")
  }
)

test_that("Incorrect date format creates warnings with Rdata input file", {
  expect_warning(
  	PrepData("../testthat/rawData.rda", dateNm = "date", weightNm ="weight", 
  		dateGp = "weeks", dateGpBp = "weeks"), "Formatting date as ")
  }
)

out <- suppressMessages(PrepData("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
			    dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y"))

test_that("All columns have exactly 2 classes, except date and weight", {
 	cntnsVars = Filter(is.cntns, out)
 	dscrtVars = Filter(is.dscrt, out)
 	dateVars  = Filter(is.IDate, out)
	expect_equal(length(cntnsVars), 2)
	expect_equal(length(dscrtVars), 3)
	expect_equal(length(dateVars),  2)
	expect_equal(length(class(out[, weight])), 1)
	expect_equal(length(cntnsVars) + length(dscrtVars) + length(dateVars) + 1, ncol(out))
})

test_that("Variables are assigned to appropriate data type", {
 	cntnsVars = Filter(is.cntns, out)
 	
	# test that all cntns variables are numeric
	expect_equal(length(Filter(Negate(is.numeric), cntnsVars)), 0)
	
	# test that no cntns variables are binary
	expect_equal(length(Filter(is.binary, cntnsVars)), 0)
	
	# test that all discrete variables are binary, character, or factor
	dscrtVars = Filter(is.dscrt, out)
 	binVars = Filter(is.binary, dscrtVars)
 	charVars = Filter(Negate(is.binary), dscrtVars)
 	charClasses = unique(sapply(charVars, function(x) class(x)[1]))
 	expect_equal(length(setdiff(charClasses, c("character", "factor"))), 0)
 	
 	# test that all remaining variables are IDate, except weight
 	dateVars = Filter(is.IDate, out)

	expect_equal(length(names(dateVars)) + length(names(binVars)) + length(names(charVars)) 
		+ length(names(cntnsVars)) + 1, length(names(out)))	
})

test_that("varNms parameter works", {
	out <- PrepData("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y", varNms = c("age", "balance"))
	cntnsVars = Filter(is.cntns, out)
	dscrtVars = Filter(is.dscrt, out)
 	dateVars  = Filter(is.IDate, out)
	expect_equal(length(cntnsVars), 2)
	expect_equal(length(dscrtVars), 0)
	expect_equal(length(dateVars), 2)

	out <- PrepData("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y", varNms = c(1, 4))
	cntnsVars = Filter(is.cntns, out)
	dscrtVars = Filter(is.dscrt, out)
 	dateVars  = Filter(is.IDate, out)
	expect_equal(length(cntnsVars), 2)
	expect_equal(length(dscrtVars), 0)
	expect_equal(length(dateVars), 2)
})

test_that("selectCols and dropCols work as expected for csv file", {
	
	# Test that selectCols works alone
	out <- PrepData("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y",
 		   selectCols = c("age", "balance", "date", "weight"))
    cntnsVars = Filter(is.cntns, out)
	dscrtVars = Filter(is.dscrt, out)
 	dateVars  = Filter(is.IDate, out)
	expect_equal(length(cntnsVars), 2)
	expect_equal(length(dscrtVars), 0)
	expect_equal(length(dateVars), 2)
 	
 	out <- PrepData("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y",
 		   selectCols = c(1, 4, 7, 6))
    cntnsVars = Filter(is.cntns, out)
	dscrtVars = Filter(is.dscrt, out)
 	dateVars  = Filter(is.IDate, out)
	expect_equal(length(cntnsVars), 2)
	expect_equal(length(dscrtVars), 0)
	expect_equal(length(dateVars), 2)

 	# test that dropCols works alone
 	out <- PrepData("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y",
 		   dropCols = c("job", "marital", "default"))
    cntnsVars = Filter(is.cntns, out)
	dscrtVars = Filter(is.dscrt, out)
 	dateVars  = Filter(is.IDate, out)
	expect_equal(length(cntnsVars), 2)
	expect_equal(length(dscrtVars), 0)
	expect_equal(length(dateVars), 2)

 	out <- PrepData("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y",
 		   dropCols = c(2:3, 5))
    cntnsVars = Filter(is.cntns, out)
	dscrtVars = Filter(is.dscrt, out)
 	dateVars  = Filter(is.IDate, out)
	expect_equal(length(cntnsVars), 2)
	expect_equal(length(dscrtVars), 0)
	expect_equal(length(dateVars), 2)
})

test_that("selectCols and dropCols work as expected for RData file", {
	
	# Test that selectCols works alone
	out <- PrepData("../testthat/rawData.rda", dateNm = "date", weightNm = "weight",
			dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y",
 		   	selectCols = c("age", "balance", "date", "weight"))
    cntnsVars = Filter(is.cntns, out)
	dscrtVars = Filter(is.dscrt, out)
 	dateVars  = Filter(is.IDate, out)
	expect_equal(length(cntnsVars), 2)
	expect_equal(length(dscrtVars), 0)
	expect_equal(length(dateVars), 2)
 	
 	out <- PrepData("../testthat/rawData.rda", dateNm = "date", weightNm = "weight",
			dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y",
 		   	selectCols = c(1, 4, 7, 6))
    cntnsVars = Filter(is.cntns, out)
	dscrtVars = Filter(is.dscrt, out)
 	dateVars  = Filter(is.IDate, out)
	expect_equal(length(cntnsVars), 2)
	expect_equal(length(dscrtVars), 0)
	expect_equal(length(dateVars), 2)

 	# test that dropCols works alone
 	out <- PrepData("../testthat/rawData.rda", dateNm = "date", weightNm = "weight",
 		   dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y",
 		   dropCols = c("job", "marital", "default"))
    cntnsVars = Filter(is.cntns, out)
	dscrtVars = Filter(is.dscrt, out)
 	dateVars  = Filter(is.IDate, out)
	expect_equal(length(cntnsVars), 2)
	expect_equal(length(dscrtVars), 0)
	expect_equal(length(dateVars), 2)

	 	out <- PrepData("../testthat/rawData.rda", dateNm = "date", weightNm = "weight",
 		   dateGp = "weeks", dateGpBp = "weeks", dateFt = "%d-%m-%Y",
 		   dropCols = c(2:3, 5))
    cntnsVars = Filter(is.cntns, out)
	dscrtVars = Filter(is.dscrt, out)
 	dateVars  = Filter(is.IDate, out)
	expect_equal(length(cntnsVars), 2)
	expect_equal(length(dscrtVars), 0)
	expect_equal(length(dateVars), 2)
})

test_that("dropConstants works as expected", {
	
	# test that attempting to group at too coarse a level results in the grouping variable being dropped 
	out <- suppressWarnings(PrepData("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "quarters", dateFt = "%d-%m-%Y", dropConstants = TRUE))
    expect_warning(PrepData("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "quarters", dateFt = "%d-%m-%Y", dropConstants = TRUE),
 		   "The following variables have no variability")
	expect_null(out[["quarters"]])
	
	
	# test that when dropConstants is set to FALSE, the constant grouping variable is retained, with a warning
	out <- suppressWarnings(PrepData("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "quarters", dateFt = "%d-%m-%Y", dropConstants = FALSE))
    expect_warning(PrepData("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "quarters", dateFt = "%d-%m-%Y", dropConstants = FALSE),
 		   "variability in grouping")
    expect_equal(length(unique(out[["quarters"]])), 1)
    
})
	
test_that("integer64 data doesn't cause problems", {
	require(bit64)
	out <- suppressWarnings(PrepData("../testthat/rawData.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "months", dateFt = "%d-%m-%Y"))
	out[ , balance := as.integer64(balance)]
	PrepData(out, dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "months", dateFt = "%d-%m-%Y")
	expect_false(is.integer64(out[, balance]))
	
	out <- suppressWarnings(PrepData("../testthat/rawData_bigint.csv", dateNm = "date", weightNm = "weight", 
 		   dateGp = "weeks", dateGpBp = "months", dateFt = "%d-%m-%Y"))
	expect_false(is.integer64(out[,bigint]))
})

test_that("Incorrect data input file generates error", {
	expect_error(dataFl <- PrepD("../testthat/PlotHistogram.RDS"))
})