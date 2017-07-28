library(otvPlots)
library(proto)
context("Plot histogram")
load("../testthat/testData.rda")
setDT(testData)
suppressMessages(PrepData(testData, dateNm = "date", 
				 dateGp = "weeks", dateGpBp = "weeks", weightNm = "weight"))

test_that("expected plot elements are returned", {
  p <- PlotBarplot(dataFl = testData, myVar =  "job", weightNm = "weight")
	
  expect_is(p$layers[[1]], "ggproto")
  expect_is(p$layers[[1]]$geom, "GeomBar")
  expect_is(p$layers[[1]]$stat, "StatIdentity")
  expect_identical(p$labels$x, "job")
  expect_identical(p$labels$y, "count")
  expect_is(p$scales$scales[[1]], "ScaleDiscrete")
  expect_true( "group" %in% names(p$mapping)) 
  expect_true( "x" %in% names(p$mapping)) 
  expect_true( "y" %in% names(p$mapping)) 
  expect_length(setdiff(c("group", "x", "y"), names(p$mapping)), 0)
})

test_that("variable is put in expected order with and without weights", {
	p <- PlotBarplot(dataFl = testData, myVar =  "job", weightNm = "weight")
	o1 <- names(rev(sort(xtabs(weight~job, data=testData))))
	o2 <- as.character(p$data[order(-count)][["job"]])
	expect_equal(o1, o2)
	
	p <- PlotBarplot(dataFl = testData, myVar =  "job", weightNm = NULL)
	o1 <- names(rev(sort(testData[, table(job)])))
	o2 <- rev(as.character(p$data[order(count)][["job"]]))
	expect_equal(o1, o2)
})

test_that("global totals are calculated as expected", {
	p1 <- PlotBarplot(dataFl = testData, myVar =  "job", weightNm = "weight")
	expect_equal(as.numeric(p1$data[job=="retired"]$count), as.numeric(testData[job=="retired", sum(weight)]))
	p2 <- PlotBarplot(dataFl = testData, myVar =  "job", weightNm = NULL)
	expect_equal(as.numeric(p2$data[job=="entrepreneur"]$count), as.numeric(testData[job=="entrepreneur", .N]))
})





