library(otvPlots)
library(proto)
context("Plot Boxplots")
load("../testthat/testData.rda")
setDT(testData)
suppressMessages(PrepData(dataFl = testData, dateNm = "date", dateGp = "weeks", dateGpBp = "weeks"))

test_that("Plot layers match expectations",{
  p <- PlotDist(dataFl = testData, myVar = "balance", dateGpBp = "weeks", weightNm = "weight")  
  expect_is(p$layers[[1]], "ggproto")
  expect_is(p$layers[[1]]$geom, "GeomBoxplot")
  expect_is(p$layers[[1]]$stat, "StatBoxplot")
  expect_is(p$layers[[2]]$geom, "GeomRug")
  expect_is(p$layers[[2]]$stat, "StatIdentity")
})


test_that("Mapping layer contains expected elements", {
  p <- PlotDist(testData, myVar = "balance", dateGpBp = "weeks")  
  expect_true( "group" %in% names(p$mapping)) 
  expect_true( "x" %in% names(p$mapping)) 
  expect_true( "y" %in% names(p$mapping)) 
  expect_length(setdiff(c("group", "x", "y"), names(p$mapping)), 0)
  
  expect_true( "x" %in% names(p$layers[[2]]$mapping)) 
  expect_true( "y" %in% names(p$layers[[2]]$mapping)) 
  expect_length(setdiff(c("x", "y"), names(p$mapping)), 0)
 })


test_that("Y axis is labeled 'balance' and X axis is labeled 'weeks'",{
  p <- PlotDist(testData, "balance", "weeks")
  expect_identical(p$labels$x, "weeks")
  expect_identical(p$labels$y, "balance")
})

test_that("invalid log transform returns message and untransformed plot", {
	expect_message(PlotDist(dataFl = testData, myVar = "balance", dateGpBp = "weeks", skewOpt = 3), 
	"untransformed boxplot")
	p <- PlotDist(dataFl = testData, myVar = "balance", dateGpBp = "weeks", skewOpt = 3)
	expect_is(p$layers[[1]], "ggproto")
  	expect_is(p$layers[[1]]$geom, "GeomBoxplot")
  	expect_is(p$layers[[1]]$stat, "StatBoxplot")
  	expect_is(p$layers[[2]]$geom, "GeomRug")
  	expect_is(p$layers[[2]]$stat, "StatIdentity")
  	expect_equal(length(grep("log10", p$labels$y)), 0)
})

test_that("valid log transform returns transformed scale",{
	testData[, posBalance := ifelse(balance >= 0, balance, 0)]
	p <- PlotDist(dataFl = testData, myVar = "posBalance", dateGpBp = "weeks", skewOpt = 3)
	expect_message(PlotDist(dataFl = testData, myVar = "posBalance", dateGpBp = "weeks", skewOpt = 3), 
		"Scale for 'y' is already present")
	expect_is(p$layers[[1]], "ggproto")
  	expect_is(p$layers[[1]]$geom, "GeomBoxplot")
  	expect_is(p$layers[[1]]$stat, "StatBoxplot")
  	expect_is(p$layers[[2]]$geom, "GeomRug")
  	expect_is(p$layers[[2]]$stat, "StatIdentity")
  	expect_equal(grep("log10", p$labels$y), 1)
})
