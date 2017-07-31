library(otvPlots)
context("Calculate R-squared")
load("../testthat/testData.rda")
testData <- setDT(testData)
testData <- testData[, .(age, weight, date)]

test_that("CalcR2 gives correct R2 with weight", {
	test.R2 <- CalcR2("age", testData, "date", weightNm = "weight", imputeValue = NULL)
	ans.R2 <- summary(lm(age~date, weight=weight, data=testData))$r.squared
	expect_equal(test.R2, ans.R2)
})


test_that("CalcR2 gives correct R2 without weight", {
	test.R2 <- CalcR2("age", testData, "date", weightNm = NULL, imputeValue = NULL)
	ans.R2 <- summary(lm(age~date, data=testData))$r.squared
	expect_equal(test.R2, ans.R2)
})

#testData1 has missings in Y
idx = sample.int(100, 10)
testData1 = testData[idx, age:=NA]

test_that("CalcR2 is correct with imputation in Y and weight", {
	test.R2 <- CalcR2("age", testData1, "date", weightNm = "weight", imputeValue = 0)
	ans.R2 <- summary(lm(age~date, data=testData1[is.na(age), age:=0], weight=weight))$r.squared
	expect_equal(test.R2, ans.R2)
})

test_that("CalcR2 is correct with imputation in Y", {
	test.R2 <- CalcR2("age", testData1, "date", weightNm = NULL, imputeValue = 0)
	ans.R2 <- summary(lm(age~date, data=testData1[is.na(age), age:=0]))$r.squared
	expect_equal(test.R2, ans.R2)
})

#testData2 has missings in weight and date, but not in Y
testData2 =  testData[sample.int(.N, 10), weight := NA]
testData2 = testData2[sample.int(.N, 10),   date := NA]
test_that("CalcR2 is correct with missing values in weight and date", {
	test.R2 <- CalcR2("age", testData2, "date", weightNm = "weight", imputeValue = NULL)
	ans.R2 <- summary(lm(age~date, data=testData2, weight=weight))$r.squared
	expect_equal(test.R2, ans.R2)
})

#testData3 has missing in weight, date and Y
testData3 = testData2[idx, age := NA]
test_that("CalcR2 is correct with missing values in weight and date and Y", {
	test.R2 <- CalcR2("age", testData3, "date", weightNm = "weight", imputeValue = NULL)
	ans.R2 <- summary(lm(age~date, data=testData3, weight=weight))$r.squared
	expect_equal(test.R2, ans.R2)
})


test_that("CalcR2 is correct with missing values in weight and date and Y and imputation", {
	test.R2 <- CalcR2("age", testData3, "date", weightNm = "weight", imputeValue = 0)
	ans.R2 <- summary(lm(age~date, data=testData3[is.na(age), age:=0], weight=weight))$r.squared
	expect_equal(test.R2, ans.R2)
})


test_that("CalcR2 is correct with no weight and missing values in date and Y", {
	test.R2 <- CalcR2("age", testData3, "date", weightNm = NULL, imputeValue = NULL)
	ans.R2 <- summary(lm(age~date, data=testData3))$r.squared
	expect_equal(test.R2, ans.R2)
})


test_that("CalcR2 is correct with no weight and missing values in date and Y imputed", {
	test.R2 <- CalcR2("age", testData3, "date", weightNm = NULL, imputeValue = 0)
	ans.R2 <- summary(lm(age~date, data=testData3[is.na(age), age:=0]))$r.squared
	expect_equal(test.R2, ans.R2)
})








