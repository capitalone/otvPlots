library(otvPlots)
context("Order by R-squared")
load("../testthat/testData.rda")
testData = setDT(testData)


testOrder <- function(out, testData){
	cntnsVars <- names(Filter(is.cntns, testData))
	dscrtVars <- names(Filter(is.dscrt, testData))
		
	# testing that number of variables in output is equal to number of classed variables in input
	expect_equal(length(out), length(cntnsVars) + length(dscrtVars))
	
	cntnsOrder <- match(cntnsVars, out)
	dscrtOrder <- match(dscrtVars, out)
	
	#testing that all numeric variables appear before discrete
	expect_lt(max(cntnsOrder), min(dscrtOrder))
	
	#testing that all discrete variables appear in order
	expect_equal(order(dscrtOrder), 1:length(dscrtOrder))
}



test_that("OrderByR2 gives expected variable order", {
	out <- OrderByR2(dataFl = testData, 
	dateNm = "date", buildTm = NULL, weightNm = "weight", kSample = NULL)
	
	#testing order of categorical, and order of numeric relative to discrete
	testOrder(out, testData)
	
	#testing that numeric variables appear in order
	rSq1 <- CalcR2(out[1] , dataFl = testData, dateNm = "date", weightNm = "weight", imputeValue = NULL)
	rSq2 <- CalcR2(out[2] , dataFl = testData, dateNm = "date", weightNm = "weight", imputeValue = NULL)
	expect_gt(rSq1, rSq2)
})


test_that("OrderByR2 works for buildTm in date range", {
	buildTm = range(testData[, date][30:70])
	out <- OrderByR2(dataFl = testData, 
	dateNm = "date", buildTm = buildTm, weightNm = "weight", kSample = NULL)
	
	#testing order of categorical, and order of numeric relative to discrete
	testOrder(out, testData)

	testData1 = testData[date>=buildTm[1]&date<=buildTm[2]]
	#testing that numeric variables appear in order
	rSq1 <- CalcR2(out[1] , dataFl = testData1, dateNm = "date", weightNm = "weight", imputeValue = NULL)
	rSq2 <- CalcR2(out[2] , dataFl = testData1, dateNm = "date", weightNm = "weight", imputeValue = NULL)
	expect_gt(rSq1, rSq2)
})


test_that("OrderByR2 works for buildTm outside date range", {
	buildTm = range(testData[, date][30:100] + 15)
	out <- OrderByR2(dataFl = testData, 
	dateNm = "date", buildTm = buildTm, weightNm = "weight", kSample = NULL)
	
	#testing order of categorical, and order of numeric relative to discrete
	testOrder(out, testData)
	
	#testing that numeric variables appear in order
	testData1 = testData[date>=buildTm[1]&date<=buildTm[2]]
	rSq1 <- CalcR2(out[1] , dataFl = testData1, dateNm = "date", weightNm = "weight", imputeValue = NULL)
	rSq2 <- CalcR2(out[2] , dataFl = testData1, dateNm = "date", weightNm = "weight", imputeValue = NULL)
	expect_gt(rSq1, rSq2)
})


test_that("OrderByR2 works for kSample < N, with R2 being calculated on reduced sample", {
	set.seed(5555) 
	out <- OrderByR2(dataFl = testData, 
	dateNm = "date", buildTm = NULL, weightNm = "weight", kSample = 50)
	
	#testing order of categorical, and order of numeric relative to discrete
	testOrder(out, testData)
	
	#testing that numeric variables appear in order
	set.seed(5555)
	rSq1 <- CalcR2(out[1] , dataFl = testData[sample(.N, min(.N, 50))], dateNm = "date", weightNm = "weight", imputeValue = NULL)
	set.seed(5555)
	rSq2 <- CalcR2(out[2] , dataFl = testData[sample(.N, min(.N, 50))], dateNm = "date", weightNm = "weight", imputeValue = NULL)
	expect_gt(rSq1, rSq2)
})


test_that("OrderByR2 works for kSample > N", {
	out <- OrderByR2(dataFl = testData, 
	dateNm = "date", buildTm = NULL, weightNm = "weight", kSample = 200)
	
	#testing order of categorical, and order of numeric relative to discrete
	testOrder(out, testData)

	#testing that numeric variables appear in order
	rSq1 <- CalcR2(out[1] , dataFl = testData, dateNm = "date", weightNm = "weight", imputeValue = NULL)
	rSq2 <- CalcR2(out[2] , dataFl = testData, dateNm = "date", weightNm = "weight", imputeValue = NULL)
	expect_gt(rSq1, rSq2)
})


test_that("OrderByR2 works when kSample is too small to calculate R2, with numeric variables returned in 
	 order as given", {
	out <- OrderByR2(dataFl = testData, dateNm = "date", buildTm = NULL, weightNm = "weight", kSample = 2)
	
	#testing order of categorical, and order of numeric relative to discrete
	testOrder(out, testData)

	#testing that all continous variables appear in data order
	cntnsVars <- names(Filter(is.cntns, testData))
	cntnsOrder <- match(cntnsVars, out)
	expect_equal(order(cntnsOrder), 1:length(cntnsOrder))
})

test_that("OrderByR2 works when weight is null", {
	out <- OrderByR2(dataFl = testData, dateNm = "date", buildTm = NULL, weightNm = NULL, kSample = NULL)
	
	#testing order of categorical, and order of numeric relative to discrete
	testOrder(out, testData)
	
	#testing that numeric variables appear in order
	rSq1 <- CalcR2(out[1] , dataFl = testData, dateNm = "date", weightNm = NULL, imputeValue = NULL)
	rSq2 <- CalcR2(out[2] , dataFl = testData, dateNm = "date", weightNm = NULL, imputeValue = NULL)
	expect_gt(rSq1, rSq2)
})


 test_that("OrderByR2 gives warning when weight/date contains missing", {
	idx1 = sample(1:100, 100)[1:10]
	idx2 = sample(1:100, 100)[1:10]
	testData[idx1, weight := NA]
	testData[idx2, date := NA]
	
	# testing for warning that weight column contains missings
	expect_warning(OrderByR2(dataFl = testData, dateNm = "date", buildTm = NULL, 
		weightNm = "weight", kSample = NULL), "Weights column")
	# testing for warning that date column contains missings
	expect_warning(OrderByR2(dataFl = testData, dateNm = "date", buildTm = NULL, 
		weightNm = "weight", kSample = NULL), "Date column")
})




