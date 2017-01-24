library(otvPlots)
library(proto)
context("Plot Discrete Rates")
load("../testthat/testData.rda")
setDT(testData)
suppressMessages(PrepData(testData, dateNm = "date", 
				 dateGp = "weeks", dateGpBp = "weeks", weightNm = "weight"))
newLevels = names(rev(sort(testData[, table(job)])))
p = PlotCatRate(dataFl = testData, myVar = "job", dateGp = "weeks", 
 				kCategories = 3, newLevels = newLevels)

test_that("Plot layers match expectations", {
  expect_is(p$layers[[1]], "ggproto")
  expect_is(p$layers[[1]]$geom, "GeomLine")
  expect_is(p$layers[[1]]$stat, "StatIdentity")
})

test_that("Plot is labeled as expected",{
  expect_identical(p$labels$x, "weeks")
  expect_identical(p$labels$y, NULL)
  expect_identical(p$labels$colour, "job")
  expect_identical(p$labels$group, "job")
})

test_that("Scale is discrete and manual",{
   expect_is(p$scales$scales[[1]], "ScaleDiscrete")
   expect_equal(p$scales$scales[[1]]$scale_name, "manual")
})

test_that("Mapping layer contains expected elements",{
  expect_true( "group" %in% names(p$mapping)) 
  expect_equal(as.character(p$mapping[["group"]]), "job")
  expect_true( "colour" %in% names(p$mapping)) 
  expect_equal(as.character(p$mapping[["colour"]]), "job")
  expect_true( "x" %in% names(p$mapping)) 
  expect_equal(as.character(p$mapping[["x"]]), "weeks")
  expect_true( "y" %in% names(p$mapping))
  expect_equal(as.character(p$mapping[["y"]]), "rate") 
  expect_length(setdiff(c("group", "colour", "x", "y"), names(p$mapping)), 0)	
})

test_that("Colours are as expected",{
	tmp1 = p$data
	tmp2 = setDT(layer_data(p))
	setkey(tmp1, job,weeks)
	setkey(tmp2, group, x)
	tmp = cbind(tmp1, tmp2)

	expect_true(all(tmp[job=="management", colour]=="#00BA38"))
	expect_true(all(tmp[job=="technician", colour]=="#DB72FB"))
	expect_true(all(tmp[job=="blue-collar", colour]=="#D39200"))
	
	p2 = PlotCatRate(dataFl = testData, myVar = "job", dateGp = "weeks", 
 				kCategories = 9, newLevels = newLevels)
	
	expect_equal(unique(layer_data(p2)$colour), 
				c("#00BA38", "#DB72FB", "#D39200", "#F8766D", "#619CFF", 
				  "#00C19F", "#93AA00", "#00B9E3", "#FF61C3"))
})

### REWRITE
test_that("Rates are correctly calculated", {

  # with weights
  p = PlotCatRate(dataFl = testData, myVar = "job", dateGp = "weeks", 
 				kCategories = 3, newLevels = newLevels, weight = "weight")
  dat1 = p$data[, .(weeks, job, rate)]
  dat1[, job:=as.character(job)]
  setkey(dat1, weeks, job)
   
  dat2 = as.data.table(as.data.frame(testData[,  xtabs(weight~job+weeks)]))
  setkey(dat2, weeks, job)
  dat2[, Freq:=Freq/sum(Freq), by="weeks"]
  dat2 = dat2[job%in%c("management", "blue-collar", "technician")]
  dat2[, job:=as.character(job)]
  dat2[, weeks:=as.IDate(weeks)]
  setkey(dat2, weeks, job)
  dat = merge(dat1, dat2, by=c("weeks","job"))
  
  expect_equal(dat$rate, dat$Freq)
  
  # without weights
  
  p = PlotCatRate(dataFl = testData, myVar = "job", dateGp = "weeks", 
 				kCategories = 3, newLevels = newLevels, weight = NULL)
  dat1 = p$data[, .(weeks, job, rate)]
  dat1[, job:=as.character(job)]
  setkey(dat1, weeks, job)
   
  dat2 = as.data.table(as.data.frame(testData[,  xtabs(~job+weeks)]))
  setkey(dat2, weeks, job)
  dat2[, Freq:=as.numeric(Freq)]
  dat2[, Freq:=Freq/sum(Freq), by="weeks"]
  dat2 = dat2[job%in%c("management", "blue-collar", "technician")]
  dat2[, job:=as.character(job)]
  dat2[, weeks:=as.IDate(weeks)]
  setkey(dat2, weeks, job)
  dat = merge(dat1, dat2, by=c("weeks","job"))
  
  expect_equal(dat$rate, dat$Freq)
})






