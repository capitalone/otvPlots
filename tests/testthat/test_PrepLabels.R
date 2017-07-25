library(otvPlots)
context("Prepare Labels")
data(bankData);  setDT(bankData)
data(bankLabels);  setDT(bankLabels)

test_that("Prepare Label function can add title to the label file", {
	testFl <- PrepLabels("../testthat/drugLabel.csv")
	expect_identical(names(testFl), c("varCol", "labelCol"),
		"\"varCol\" and \"labelCol\" are not set correctly.")
})

test_that("Prepare Label function can add title to the label file", {
	testFl <- PrepLabels("../testthat/drugLabel.rda")
	expect_identical(names(testFl), c("varCol", "labelCol"),
		"\"varCol\" and \"labelCol\" are not set correctly.")
})

test_that("PrepLabels function can delete rows with empty \"varCol\"", {
	labelFl <- fread("../testthat/drugLabel.csv",
		select = 1:2, stringsAsFactors = FALSE)
	testFl <- PrepLabels("../testthat/drugLabel.csv")
	nrow_before <- nrow(labelFl)
	nrow_deleted <- labelFl[as.character(labelFl[,1][[1]])=="", .N]
	nrow_after <- nrow(testFl)
	expect_identical(nrow_before - nrow_deleted, nrow_after, "")
})

test_that("PlotVar function works good with label file", {
	dataFl <- PrepData(bankData, dateNm = "date",
             		   dateGp = "months", dateGpBp = "quarters")
	bankLabels <- PrepLabels(bankLabels, idx = 1:2)
	p <- PlotVar(dataFl, "balance", weightNm = NULL, dateNm = "date",
             	dateGp = "months", dateGpBp = "quarters",
             	labelFl = bankLabels)
	expect_is(p, "gtable")
})

test_that("Incorrect label input file generates error", {
	expect_error(labelFl <- PrepLabels("../testthat/PlotHistogram.RDS"))
})