library(otvPlots)
library(proto)
context("Plot trace plots of categories' proportions over time")
load("../testthat/testData.rda")
setDT(testData)
require(ggplot2)
suppressMessages(PrepData(testData, dateNm = "date", 
				 dateGp = "weeks", dateGpBp = "weeks", weightNm = "weight"))
p <- PlotRatesOverTime(dataFl = testData, dateGp = "weeks", myVar = "job",
    weightNm = "weight", newLevels = NULL)$p
test_that("expected plot elements are returned", {	
  expect_is(p$layers[[1]], "ggproto")
  expect_is(p$layers[[1]]$geom, "GeomLine")
  expect_is(p$layers[[1]]$stat, "StatIdentity")
  expect_is(p$layers[[1]]$position, "PositionIdentity")
  expect_identical(p$labels$x, "weeks")
  expect_identical(p$labels$y, "")
  expect_is(p$scales$scales[[1]], "ScaleContinuousDate")
  expect_identical(as.character(p$facet$params$facets[[1]]), "job")
})

test_that("rates are calculated correctly normalized by time", {
  dat = p$data
  # check that all weeks sum to 1
  dat[, sum := sum(rate), by = "weeks"]
  dat[, table(sum)]
  expect_length(dat[, table(sum)], 1)

  # check that 2008-06-03 is correctly calculated
  tmpData = testData[weeks == "2008-06-03"]
  tmpData[, rate1 := sum(weight), by = "job"]
  tmpData[, rate0 := sum(weight)]
  tmpData[, rate  := rate1/rate0]

  tmpData = unique(tmpData[, .(job, weeks, rate)])
  dat = dat[weeks == "2008-06-03" & rate > 0, .(weeks, job, rate)]
  dat[, job := as.character(job)]
  setkey(dat, job)
  setkey(tmpData, job)
  expect_equal(dat[, rate], tmpData[, rate])
})

test_that("rates are calculated correctly normalized by var", {
  p <- PlotRatesOverTime(dataFl = testData, dateGp = "weeks", myVar = "job",
                        weightNm = "weight", newLevels = NULL, normBy = "var")$p
  dat = p$data
  dat[, sum := sum(rate), by = "job"]
  
  #check all var rates sum to one
  expect_length(dat[, table(sum)], 1)
  expect_equal(dat[1, sum], 1)
  
  # check that rates are correctly calculated for technician
  tmpData = testData[job == "technician"]
  tmpData[, rate1:=sum(weight), by = "weeks"]
  tmpData[, rate0:= sum(weight)]
  tmpData[, rate := rate1/rate0]
  tmpData = unique(tmpData[, .(job, weeks, rate)])
  expect_equal(tmpData[1:4, rate], dat[job=="technician"][2:5, rate])
})

