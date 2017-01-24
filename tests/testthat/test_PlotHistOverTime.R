library(otvPlots)
library(proto)
context("Plot histogram over time")
load("../testthat/testData.rda")
setDT(testData)
suppressMessages(PrepData(testData, dateNm = "date", 
				 dateGp = "weeks", dateGpBp = "weeks", weightNm = "weight"))
p <- PlotHistOverTime(dataFl = testData, dateNm = "date", dateGp = "weeks", 
  	weightNm = "weight", myVar = "job", newLevels = NULL)

test_that("expected plot elements are returned", {	
  expect_is(p$layers[[1]], "ggproto")
  expect_is(p$layers[[1]]$geom, "GeomBar")
  expect_is(p$layers[[1]]$stat, "StatBin")
  expect_identical(p$labels$x, "date")
  expect_identical(p$labels$y, "")
  expect_is(p$scales$scales[[1]], "ScaleDiscrete")
  expect_equal(p$scales$scales[[1]]$breaks, names(rev(sort(testData[, xtabs(weight~job)]))))
  
  expect_true( "group" %in% names(p$mapping))
  expect_equal(as.character(p$mapping[["group"]]), "job") 
  expect_true( "x" %in% names(p$mapping))
  expect_equal(as.character(p$mapping[["x"]]), "date")
  expect_true( "fill" %in% names(p$mapping)) 
  expect_equal(as.character(p$mapping[["fill"]]), "job")
  expect_true( "weight" %in% names(p$mapping)) 
  expect_equal(as.character(p$mapping[["weight"]]), "weight")
  expect_length(setdiff(c("group", "x", "fill", "weight"), names(p$mapping)), 0)
})


test_that("Colours are as expected",{
	colours = unique(layer_data(p)[, c("group", "fill")])
	setDT(colours)
	expect_equal(colours[, fill], scales::hue_pal()(9))
	# breaks = p$scales$scales[[1]]$breaks
	# groupname = sort(breaks)
	# colours[, groupname := groupname]
	# expect_equal(colours[match(breaks, groupname)][, fill],
				  # c("#DB72FB","#00BA38", "#D39200", "#F8766D", "#619CFF", 
				  # "#00C19F", "#93AA00", "#FF61C3", "#00B9E3"))
})

