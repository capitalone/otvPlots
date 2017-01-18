library(otvPlots)
library(proto)
context("Plot histogram")
load("../testthat/testData.rda")
setDT(testData)
suppressMessages(PrepData(testData, dateNm = "date", 
				 dateGp = "weeks", dateGpBp = "weeks", weightNm = "weight"))

test_that("expected plot elements are returned", {
	glbTotals <- testData[, .(count = .N), by = "job"]
	p <- PlotHistogram(glbTotals, "job")
	
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
  expect_equal_to_reference(p, "../testthat/PlotHistogram.RDS")
})
 
 
test_that("expected plot elements are returned if newLevels are pre-computed", {
	glbTotals <- testData[, .(count = .N), by = "job"]
	newLevels <- glbTotals[, job][order(glbTotals[, count])]
 	newLevels <- rev(unlist(newLevels))

	p <- PlotHistogram(glbTotals, "job", newLevels)
	
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
  expect_equal_to_reference(p, "../testthat/PlotHistogram.RDS")
}) 

 
test_that("expected plot elements are returned if glbTotals is a factor", {
	glbTotals <- testData[, .(count = .N), by = "job"]
	newLevels <- glbTotals[, job][order(glbTotals[, count])]
 	newLevels <- rev(unlist(newLevels))
	glbTotals[, ("job") := factor(job, levels = newLevels)]
	p <- PlotHistogram(glbTotals, "job", newLevels)
	
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
  expect_equal_to_reference(p, "../testthat/PlotHistogram.RDS")
}) 


test_that("expected plot elements are returned if glbTotals is 
		  a factor, newLevels not given", {
	glbTotals <- testData[, .(count = .N), by = "job"]
	newLevels <- glbTotals[, job][order(glbTotals[, count])]
 	newLevels <- rev(unlist(newLevels))
	glbTotals[, ("job") := factor(job, levels = newLevels)]
	p <- PlotHistogram(glbTotals, "job")
	
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
    expect_equal_to_reference(p, "../testthat/PlotHistogram.RDS")
}) 


