# Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
# Version 2.0 (the "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the  License at
# http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
# or agreed to in writing, software distributed under the License is distributed
# on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied. See the License for the specific language governing
# permissions and limitations under the License.

#' Over time variable plots for predictive modeling (otvPlots)
#'
#' The otvPlots package uses data.table and ggplot2 packages to efficiently plot
#' time series aggregated from large datasets. Plots are optionally returned
#' ordered by correlation with date -- a natural starting point for anomaly
#' detection.  Plots are automatically labeled if a variable dictionary is
#' provided. Discrete and numeric variables are handled automatically. The
#' package can be used either interactively for adhoc creation of plots for
#' specifical variables, or via the included wrapper function for hands-off
#' monitoring reports.
#'
#' @seealso \code{\link{PlotWrapper}}
#' @seealso \code{\link{PrintPlots}}
#' @seealso \code{\link{PlotVar}}
#' @seealso \code{\link{PlotDiscreteVar}}
#' @seealso \code{\link{PlotContVar}}
#' @seealso \code{\link{PlotQuantiles}}
#' @seealso \code{\link{PlotMean}}
#' @seealso \code{\link{PlotRates}}
#' @seealso \code{\link{PlotDist}}
#' @seealso \code{\link{PrepData}}
#' @seealso \code{\link{PrepLabels}}
#' @seealso \code{\link{OrderByR2}}
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @docType package
#' @name otvPlots
#' @import data.table
#' @import ggplot2
#' @importFrom grid grid.draw grid.newpage unit unit.c textGrob gpar
#' @importFrom gridExtra arrangeGrob
#' @importFrom moments skewness
#' @importFrom Hmisc wtd.quantile wtd.mean wtd.var
#' @importFrom stringi stri_trans_general
#' @importFrom scales hue_pal
#' @importFrom grDevices cairo_pdf dev.off
#' @importFrom graphics par
#' @importFrom stats lm.fit lm.wfit quantile sd var
#' @importFrom utils tail
NULL

###########################################
#          Wrapper Function               #
###########################################

#' Automated monitoring reports
#' 
#' Prepares input dataset and labels, sorts variables according to either user 
#' input or correlation with time, and outputs the sorted plots to pdf
#' 
#' @inheritParams PrepData
#' @inheritParams PrepLabels
#' @inheritParams OrderByR2
#' @inheritParams PlotVar
#' @inheritParams PrintPlots
#' @inheritParams PlotDiscreteVar
#' @param sortVars A pre-determined vector of variable names giving the order in which
#' variables should be plotted, or NULL to keep original ordering except that
#' numeric variables will come ahead of categorical and binary. sortVars should
#' also be NULL when sortFn is used.
#' @param sortFn Name of a function which returns sortVars as output. 
#' The function may take the following variables as input: dataFl, dateNm, buildTm, 
#' weightNm, kSample. OrderByR2 is included as an example, which sorts numeric plots
#' in order of strength of linear association with date. 
#' @param prepData logical. Indicates if data should be run through PrepData 
#' function. If FALSE, dataFl must be a data.table containing variables 
#' \code{weightNm, dateNm, dateGp} and \code{dateGpBp} (allows the user to use
#' arbitrary groupings of data on the x-axis)
#' @return A VLM report saved as \code{outFl}
#' @export
#' @seealso \code{\link[otvPlots]{PrepData}}
#' @seealso \code{\link[otvPlots]{PrepLabels}}
#' @seealso \code{\link[otvPlots]{OrderByR2}}
#' @seealso \code{\link[otvPlots]{PrintPlots}}
#' @seealso \code{\link[base]{strptime}}
#' @seealso \code{\link[data.table]{IDate}}
#' @section License: Copyright 2016 Capital One Services, LLC Licensed under the
#' Apache License, Version 2.0 (the "License"); you may not use this file 
#' except in compliance with the License. You may obtain a copy of the License
#' at http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable
#' law or agreed to in writing, software distributed under the License is
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#' KIND, either express or implied. See the License for the specific language
#' governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' setDT(bankData)
#' data(bankLabels)
#' setDT(bankLabels)
#'\dontrun{ 
#' 
#' PrepData(bankData, dateNm = "date", dateGp = "months", dateGpB = "quarters")
#' PrepLabels(bankLabels)
#' 
#' # PrepData should only need to be run once on a dataset, after that PlotWrapper 
#' # can be run with PrepData = FALSE 
#' PlotWrapper(dataFl = bankData, dateNm = "date", labelFl = bankLabels,
#'             dateGp = "months", dateGpBp = "quarters", outFl = "bank.pdf", 
#'             prepData = TRUE, kSample = NULL, kCategories = 12)
#'} 
#' # Different values of kSample can affect the appearance of boxplots (and 
#' # possibly the order of variable output if sortVars = 'R2' is used), but does 
#' # not affect the time series plots, which always use all of the data 
#'\dontrun{
#'
#'PlotWrapper(dataFl = bankData, dateNm = "date", labelFl = bankLabels,
#'             dateGp = "months", dateGpBp = "quarters", outFl = "bank.pdf", 
#'             prepData = FALSE, kSample = 500)
#'}
#' 
#' #  If weights are provided they will be used in all statistical calculations
#'\dontrun{bankData[, weight := rnorm(.N, 1, .1)]
#' PlotWrapper(dataFl = bankData, dateNm = "date", labelFl = bankLabels,
#'             dateGp = "months", dateGpBp = "quarters", weightNm = "weight", 
#'             outFl = "bank.pdf", prepData = FALSE, kSample = NULL)
#'}
#' # PlotWrapper is designed for non-interactive use, and both dataFl and
#' # labelFl could be passed as strings giving the location of the datasets on 
#' # disk, as long as they are able to be parsed by fread. Since the example 
#' # datasets bankData and bankLabels are saved as rda files, for this example 
#' # we will need to read them into memory using utils::data then convert 
#' # to data.table
#' data(bankData)
#' setDT(bankData)
#' data(bankLabels) 
#' setDT(bankLabels)
#' \dontrun{ 
#' PlotWrapper(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
#'             dateGp = "months", dateGpBp="quarters", weightNm = NULL, 
#'             outFl = "bank.pdf", prepData = TRUE, kSample = NULL)
#'}
#' # We can pass a vector of variable names to customize plotting order using
#' # sortVars, but we must exclude the "date" column from sortVars or the 
#' # function will stop with a message warning us it cannot plot dates
#'\dontrun{ 
#' sortVars = sort(bankLabels[varCol!="date", varCol])
#' PlotWrapper(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
#'             dateGp = "months", dateGpBp = "quarters", weightNm = NULL, 
#'             outFl = "bank.pdf", prepData = FALSE, kSample = NULL, 
#'             sortVars = sortVars, kCategories = 9)
#'} 
#' # We can test that the function is working with a specific variable using 
#' # the varNms parameter
#' PlotWrapper(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
#'             dateGp = "months", dateGpBp = "quarters", weightNm = NULL, 
#'             outFl = "bank.pdf", prepData = TRUE, kSample = NULL, 
#'             varNms = "age", sortVars = NULL)
#' 
#' # See otvPlots::PlotVar for examples in interactive use, 
#' # including use of the fuzzyLabels parameter
#' 
PlotWrapper <- function(dataFl, dateNm, labelFl = NULL, selectCols = NULL,
                        dropCols = NULL, dateFt = "%d%h%Y",
                        dateGp = NULL, dateGpBp = NULL, weightNm = NULL,
                        buildTm = NULL, highlightNms = NULL, skewOpt = NULL,
                        kSample = 50000, outFl = "otvPlots.pdf", prepData = TRUE,
                        varNms = NULL, fuzzyLabelFn = NULL, 
                        dropConstants = TRUE, sortVars = NULL, sortFn = NULL, 
                        kCategories = 3, ...) {

  if (!is.null(sortVars) & !is.null(sortFn)) {
  	stop ("Please choose between sortVars (predetermined order of plotting) and
          sortFn (function to determine plotting order)")}
          
  if(!is.null(sortVars) & !is.null(varNms) &&
      !all(varNms %in% sortVars)) {
    stop ("Please make certain that varNms is a subset of sortVars")}
  
  if (prepData) {
    if (is.character(dataFl)) {
      dataFl <- PrepData(dataFl = dataFl, dateNm = dateNm, 
                         selectCols = selectCols, dropCols = dropCols, 
                         dateFt = dateFt, dateGp = dateGp, dateGpBp = dateGpBp,
                         weightNm = weightNm, varNms = varNms,
                         dropConstants = dropConstants, ...)
    } else {
      PrepData(dataFl = dataFl, dateNm = dateNm, selectCols = selectCols, 
               dropCols = dropCols, dateFt = dateFt, dateGp = dateGp,
               dateGpBp = dateGpBp, weightNm = weightNm, varNms = varNms,
               dropConstants = dropConstants,  ...)
    } 
    
       # ensure no integer64 types	
    dateNm <- tolower(gsub("\\.|/|\\-|\"|\\s", "", dateNm))
    if (!is.null(weightNm)) {
      weightNm <- tolower(gsub("\\.|/|\\-|\"|\\s", "", weightNm))
    }
  } else {

    stopifnot(is.data.table(dataFl) && 
              all(tolower(c(weightNm, dateNm, dateGp, dateGpBp)) %in% 
                  tolower(names(dataFl))))
    for (var in names(dataFl)) {
  	  if (inherits(dataFl[[var]], "integer64")) {
  		dataFl[, (var) := as.numeric(get(var))]
  	  }
    }

  }
  if (is.character(labelFl)) {
    labelFl <- PrepLabels(labelFl)
  } else {
    PrepLabels(labelFl)
  }
  
  if (!is.null(sortFn) && is.character(sortFn)) {
  	sortVars <- do.call(sortFn, list(dataFl = dataFl, dateNm = dateNm, buildTm = buildTm, 
                          weightNm = weightNm, kSample = kSample))                       
  } else {
    if (is.null(sortVars)) {
      num_vars <- names(dataFl)[sapply(dataFl, inherits, "cntns")]
      cat_vars <- names(dataFl)[sapply(dataFl, inherits, "dscrt")]
      sortVars <- c(num_vars, cat_vars)
    }
  }
  
  if (!is.null(varNms)) {
    PrintPlots(outFl = outFl,
               dataFl = dataFl[, c(varNms, dateNm,
                                    dateGp, dateGpBp, weightNm), with = FALSE],
               sortVars = sortVars[sortVars %in% varNms], dateNm = dateNm, 
               dateGp = dateGp, dateGpBp = dateGpBp, weightNm = weightNm,
               labelFl = labelFl, highlightNms = highlightNms,
               skewOpt = skewOpt, kSample = kSample,
               fuzzyLabelFn = fuzzyLabelFn, kCategories = kCategories)
  } else {
    PrintPlots(outFl = outFl, dataFl = dataFl, sortVars = sortVars,
               dateNm = dateNm, dateGp = dateGp, dateGpBp = dateGpBp, 
               weightNm = weightNm, labelFl = labelFl,
               highlightNms = highlightNms, skewOpt = skewOpt,
               kSample = kSample, fuzzyLabelFn = fuzzyLabelFn, 
               kCategories = kCategories)
  }
}
  

###########################################
#           Create output                 #
###########################################

#' Create pdf output with plots for each variable arranged on a single page
#'
#' Creates plots and outputs results to a pdf named as \code{outFl}. Creates
#' letter-sized output with each page corresponding to a single variable.
#'
#' @param outFl Name of output file (by default "otvPlots.pdf"). PDF will be
#' saved to your working directory unless path is included in \code{outFl}
#' (e.g. "../plots/otvPlots.pdf")
#' @param sortVars A character vector of variable names in the order they will
#' be plotted. 
#' @inheritParams PlotVar
#' @inheritParams OrderByR2
#' @inheritParams PrepData
#' @inheritParams PlotDiscreteVar
#' @return A pdf of plots saved to \code{outFl}
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @export
PrintPlots <- function(outFl, dataFl, sortVars, dateNm, dateGp,
                       dateGpBp, weightNm = NULL, 
                       labelFl = NULL, highlightNms = NULL,
                       skewOpt = NULL, kSample = 50000, fuzzyLabelFn, 
                       kCategories = 3) {
  
  
  plotList <-
    lapply(sortVars, PlotVar,
           dataFl = dataFl, weightNm = weightNm, dateNm = dateNm,
           dateGp = dateGp, dateGpBp = dateGpBp, labelFl = labelFl,
           highlightNms = highlightNms, skewOpt = skewOpt,
           fuzzyLabelFn = fuzzyLabelFn, kCategories = kCategories)
  
  cairo_pdf(filename = outFl,  width = 11, height = 8, pointsize = 12,
            onefile = TRUE)
  
  par(ask = FALSE)
  for (x in plotList)	 {
    grid::grid.newpage()
    grid::grid.draw(x)
  }
  dev.off()
}


###########################################
#          Main Plot Function             #
###########################################

#' Interactive creation of overtime variable plots
#'
#' @param dataFl A data.table containing at least the following column names:
#' \code{myVar}, \code{weightNm}, \code{dateGp}, \code{dateGpBp}. Output of
#' PrepData
#' @param myVar Name of the variable to be plotted
#' @inheritParams PrepData
#' @param labelFl A data.table containing variable labels, or NULL for no 
#' labels. Output of PrepLabels
#' @param highlightNms Either NULL or a character vector of variables to recieve
#' red label. Currently NULL means all
#' variables will get a black legend. Ignored if labelFl = NULL.
#' @param skewOpt Either numeric constant or NULL. If numeric, say 5, the box
#' plots of variables whose skew exceeds 5 will be on a log10 scale if
#' possible. If negative, 3 will be used as the cutoff. Default is NULL (no 
#' transformation)
#' @param fuzzyLabelFn Either NULL or a function of 2 parameters: A label file 
#' in the format output by PrepLabels and a string giving a variable name. The
#' function should return the label corresponding to the variable given by the
#' second parameter. This function should describe how fuzzy matching should be
#' performed to find labels (see example below). If NULL, only exact matches 
#' will be retuned.
#' @inheritParams OrderByR2
#' @inheritParams PlotDiscreteVar
#' @return A grid of ggplots. For discrete variables (including continuous 
#' variables with no more than 2 unique levels not including NA), 
#' frequency/rate graphs are used.  For discrete variables with <=9 levels, 
#' the grid includes a frequency bar chart, a
#' stacked frequency plot over time, and a stacked proportion plot over time.
#' For other discrete variables a single frequency bar chart is returned. For 
#' continuous variables, a grid of ggplot objects is returned including a 
#' boxplot grouped by \code{dateGpBp}, a time series of p1, p50 and p99 grouped
#' by \code{dateGp}, a time series of mean and +-1 SD control limits grouped by
#' \code{dateGp}, and a time series of missing and zerorates grouped by
#' \code{dateGp}
#' @export
#' @seealso \code{\link{PlotDiscreteVar}}
#' @seealso \code{\link{PlotContVar}}
#' @seealso \code{\link{PlotDist}}
#' @seealso \code{\link{PlotQuantiles}}
#' @seealso \code{\link{PlotMean}}
#' @seealso \code{\link{PlotRates}}
#' @section License: Copyright 2016 Capital One Services, LLC Licensed under the
#' Apache License, Version 2.0 (the "License"); you may not use this file
#' except in compliance with the License. You may obtain a copy of the  License
#' at http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable
#' law or agreed to in writing, software distributed under the License is
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#' KIND, either express or implied. See the License for the specific language
#' governing permissions and limitations under the License.
#' @examples
#' require(data.table)
#' data(bankData)
#' setDT(bankData)
#' data(bankLabels)
#' setDT(bankLabels)
#' PrepLabels(bankLabels)
#' PrepData(bankData, dateNm = "date", dateGp = "months", dateGpBp = "quarters", 
#'          weightNm = NULL)
#'
#' # PlotVar will treat numeric and categorical data differently automatically. 
#' # Binary data is always treated as nominal.
#' plot(PlotVar(bankData, myVar = "duration", weightNm = NULL, 
#'      dateNm = "date", dateGp = "months", dateGpBp =  "quarters"))
#' plot(PlotVar(bankData, myVar = "job", weightNm = NULL,  
#'      dateNm = "date", dateGp = "months", dateGpBp =  "quarters"))
#' plot(PlotVar(bankData, myVar = "loan", weightNm = NULL, 
#'      dateNm = "date", dateGp = "months", dateGpBp =  "quarters"))
#' plot(PlotVar(bankData, myVar = "y", weightNm = NULL, 
#'      dateNm = "date", dateGp = "months", dateGpBp =  "quarters"))
#'
#' # It's possible to plot using dateNm as the grouping variable, or another
#' # custom grouping variable
#'\dontrun{ 
#'plot(PlotVar(bankData, myVar =  "y", weightNm = NULL, dateNm = "date", 
#'      dateGp = "date", dateGpBp = "date"))
#'}
#' # If labels are provided, they will be added. If the variable being plotted 
#' # is in the "highlightNms", its label will be red.
#' plot(PlotVar(bankData, myVar = "balance", weightNm = NULL, dateNm = "date", 
#'              dateGp = "months", dateGpBp = "quarters", labelFl = bankLabels))
#' plot(PlotVar(bankData, myVar = "balance", weightNm = NULL, dateNm = "date", 
#'              dateGp = "months", dateGpBp = "quarters", 
#'              highlightNms = c("balance"), labelFl = bankLabels))
#'
#' # It is possible to define a function for fuzzy label matching. For example, 
#' # if variables look like VAR_nameofvar, and the attribute dictionary contains
#' # defintions only for nameofvar, then a fuzzy matching function can be 
#' # provided which would first attempt to match exactly, and then to attempt to 
#' # match on the longest piece after splitting on the underscore:
#' Fuzzy = function(LabelFl, myVar){
#'    ll = labelFl[varCol == myVar, labelCol] # exact match
#'    if (ll == ""){
#'        # split on "_", search for exact match of longest piece
#'        shortNm = names(which.max(sapply(strsplit(myVar, "_")[[1]], nchar)))
#'        ll = labelFl[varCol == shortNm, labelCol]
#'    }
#'    return(ll)
#'  }
#'
#'  # See otvPlots::PlotWrapper for additional examples of non-interactive use
#'
PlotVar <- function(dataFl, myVar, weightNm, dateNm, dateGp, dateGpBp = NULL,
                   labelFl = NULL, highlightNms = NULL, skewOpt = NULL,
                   kSample = 50000, fuzzyLabelFn = NULL, kCategories = 3) {
  varCol <- labelCol <- NULL
  message(paste("plotting ", myVar))
  if (any(is.element(unlist(dataFl[, class(get(myVar))]),
                     c("Date", "IDate")))) {
    stop("Cannot plot dates")
  }
  
  if (!(inherits(myVar, "dscrt") | inherits(myVar, "cntns"))) {
    if (dataFl[, class(get(myVar))] %in% c("character", "factor") ||
        dataFl[, length(unique(stats::na.omit(get(myVar))))] == 2) {
      setattr(dataFl[, get(myVar)], "class", "dscrt")
    } else {
      setattr(dataFl[, get(myVar)], "class", "cntns")
    }
  }
  
  if (inherits(dataFl[[myVar]], "dscrt")) {
    p <- PlotDiscreteVar(myVar, dataFl, weightNm, dateNm, dateGp, kCategories)
  } else {
    if (inherits(dataFl[[myVar]], "cntns")) {
      p <- PlotContVar(myVar, dataFl, weightNm, dateGp, dateGpBp, skewOpt, 
                       kSample)
    }
  }
  
  # if no fuzzy matching function provided, provide exact matches on the first
  # column, otherwise use logic defined in fuzzyLabelFn
  if (!is.null(labelFl)) {
    if (!all(c("varCol", "labelCol") %in% names(labelFl))) {
      message("Running PrepLabels on labelFl")
      PrepLabels(labelFl)
    }
    subHeight <- grid::unit(12, "points")
    if (is.null(fuzzyLabelFn)) {
      ll <- paste0(labelFl[varCol == myVar, labelCol], "\n")
    } else {
      ll <- fuzzyLabelFn(labelFl, myVar)
    }
    
    if (!is.null(highlightNms) && myVar %in% highlightNms) {
      # variables in highlightNms get red legend
      subCol <- "red"
    } else {
      # should add other ways to trigger red labels
      subCol <- "black"
    }
    
    subText <- grid::textGrob(ll, gp = grid::gpar(fontsize = 10, col = subCol))
    grobHeights <- grid::unit.c(grid::unit(1, "npc") - subHeight, subHeight)
    p <- gridExtra::arrangeGrob(p, subText, heights = grobHeights)
  }
  
  return(p)
}



###########################################
#      Discrete Plot Method               #
###########################################
#' Frequency and proportion plots for discrete variables
#'
#' @inheritParams PlotVar
#' @inheritParams PlotHistOverTime
#' @param kCategories If a categorical variable has more than kCategories, only 
#' a global histogram will be plotted, rate plots for all categories will also be
#' plotted.
#' @export
#' @return Histogram and rate charts (if less than kCategories) for categorical data. 
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @examples
#' require(data.table)
#' data(bankData)
#' setDT(bankData)
#' require(ggplot2)
#'
#' PrepData(bankData, dateNm = "date", dateGp = "months", dateGpBp = "quarters", 
#'          weightNm = NULL)
#' # Single histogram is plotted for job type since there are 12 categories
#' plot(PlotDiscreteVar(myVar = "job", dataFl = bankData, weightNm =  NULL, 
#'                      dateNm = "date", dateGp = "months"))
#'                      
#' plot(PlotDiscreteVar(myVar = "job", dataFl = bankData, 
#'                      weightNm = NULL, dateNm = "date", dateGp = "months",
#'                      kCategories = 12))
#'
#'
#' # binary data is treated as categorical, and only the less frequent category 
#' # is plotted over time
#' plot(PlotDiscreteVar(myVar = "default", dataFl = bankData, weightNm = NULL, 
#'                      dateNm = "date", dateGp = "months"))

PlotDiscreteVar <- function(myVar, dataFl, weightNm = NULL, dateNm, dateGp,
                            kCategories = 9, normBy = "time") {
  count <- NULL
  
  p <- PlotHistogram(dataFl = dataFl, myVar = myVar, weightNm = weightNm) 
  newLevels <- as.character(p$data[order(-count)][[myVar]])

  # If more than kCategories levels only plot a single histogram (p)
  # Otherwise also plot the category rates over time (p2)
  
  if (!is.null(kCategories) && length(newLevels) <= kCategories) {    
      
    p2 <- PlotHistOverTime(dataFl = dataFl, dateGp = dateGp, 
    					   weightNm = weightNm, myVar = myVar, newLevels = newLevels, 
    					   normBy = normBy)    
    
    p  <- gridExtra::arrangeGrob(ggplotGrob(p), p2, widths = c(1, 2))
  }
  
  return(p)
}



###########################################
#        Continuous Plot Method           #
###########################################

#' Boxplots and overtime plots of numeric variables
#'
#' @inheritParams PlotVar
#' @inheritParams OrderByR2
#' @export
#' @return A grid of ggplot objects including a boxplot grouped by 
#' \code{dateGpBp}, a time series of  p1, p50 and p99 grouped by \code{dateGp},
#' a time series of mean and +-1 SD control limits grouped by \code{dateGp},
#' and a time series of missing and zerorates grouped by \code{dateGp}
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' setDT(bankData)
#' PrepData(bankData, dateNm = "date", dateGp = "months", dateGpBp = "years")
#' plot(PlotContVar("balance", bankData, NULL, "months", "years", 
#'                  skewOpt = NULL, kSample = NULL))
#'
PlotContVar <- function(myVar, dataFl, weightNm, dateGp, dateGpBp, 
                        skewOpt = NULL, kSample = 50000) {
  variable <- NULL
  if(inherits(myVar, "integer64")) stop("Cannot plot integer64 type--cast to numeric")
  meltdx <- SummaryStats(myVar = myVar, dataFl = dataFl, dateGp = dateGp,
  						 weightNm = weightNm)
  
  # option for log10 transform of box plot y axis if skewness is high enough. 
  # Invalid choices revert to 3.
  if (!is.null(skewOpt)) {
    stopifnot(is.numeric(skewOpt))
    if (skewOpt < 0) {
      skewOpt <- 3
    }
  }
  
  if (!is.null(kSample)) {
	# take a subsample of dataFl for boxplots
    p1 <- PlotDist(dataFl[sample(.N, min(.N, kSample))], myVar, dateGpBp, 
                  weightNm, skewOpt) 
  } else {
    p1 <- PlotDist(dataFl, myVar, dateGpBp, weightNm, skewOpt)
  }
  p2 <- PlotQuantiles(meltdx[variable %in% c("p99", "p50", "p1", "p99_g", 
                                             "p50_g", "p1_g")], myVar, dateGp)
  p3 <- PlotMean(meltdx[variable %in% c("Mean", "cl1", "cl2")], myVar, dateGp)
  p4 <- PlotRates(meltdx, myVar, dateGp)
  p5 <- rbind(ggplot2::ggplotGrob(p2), ggplot2::ggplotGrob(p3), 
              ggplot2::ggplotGrob(p4), size = "last")
  p  <- gridExtra::arrangeGrob(p1, p5,  
                               layout_matrix = cbind(c(1, 1, 1), c(5, 5, 5)), 
                               widths = 1:2)
  return(p)
}


###########################################
#     Continuous Plotting Functions       #
###########################################

#' Create data.table of summary statistics for continuous plotting functions
#' @inheritParams PrepData
#' @inheritParams PlotVar
#' @export
#' @return A data.table formatted for use by the continuous plot funtions 
#' \code{PlotMean}, \code{PlotQuantiles} and \code{PlotRates}.
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' setDT(bankData)
#' PrepData(bankData, dateNm = "date", dateGp = "months", dateGpBp = "years")
#' mdx = SummaryStats(myVar = "age", dataFl = bankData, dateGp = "months")
#' plot(PlotQuantiles(mdx[variable %in% c("p99", "p50", "p1", "p99_g", 
#'                                             "p50_g", "p1_g")], "age", "months"))
#' plot(PlotMean(mdx[variable %in% c("Mean", "cl1", "cl2")], "age", "months"))
#' plot(PlotRates(mdx, "age", "months"))
SummaryStats <- function(myVar, dataFl, dateGp, weightNm = NULL) {
  variable <- NULL
  if (!is.null(weightNm)) {
    dx <- dataFl[, {
      tmp1 = wtd.quantile_NA(get(myVar), get(weightNm), c(.01, .5, .99));
      list(
        "p1"   = tmp1[1],
        "p50"  = tmp1[2],
        "p99"  = tmp1[3],
        "Mean" = as.double(Hmisc::wtd.mean(get(myVar), get(weightNm),
                                           normwt = TRUE, na.rm = TRUE)),
        "zerorate"    = Hmisc::wtd.mean(get(myVar) == 0, get(weightNm),
                                        na.rm = TRUE, normwt = TRUE),
        "missingrate" = Hmisc::wtd.mean(is.na(get(myVar)), 
                                        get(weightNm), normwt = TRUE)
      )
    }, by = c(dateGp)]
    qq <- dataFl[, wtd.quantile_NA(get(myVar), get(weightNm), 
                                  probs = c(.99, .5, .01))]
    cl <- dataFl[, c(Hmisc::wtd.mean(get(myVar), get(weightNm), 
                                    na.rm = TRUE, normwt = TRUE), 
                    sqrt(Hmisc::wtd.var(get(myVar), 
                                        get(weightNm), na.rm = TRUE, normwt = TRUE)))]
  } else {
    dx <- dataFl[, {
      tmp1 = quantile(get(myVar), probs = c(.01, .5, .99), na.rm = TRUE);
      list(
        "p1"  = tmp1[1],
        "p50" = tmp1[2],
        "p99" = tmp1[3],
        "Mean"        = as.double(mean(get(myVar), na.rm = TRUE)),
        "zerorate"    = mean(get(myVar) == 0, na.rm = TRUE),
        "missingrate" = mean(is.na(get(myVar)))
      )
    }, by = c(dateGp)]
    qq <- dataFl[, quantile(get(myVar), probs = c(.99, .5, .01), na.rm = TRUE)]
    cl <- dataFl[, c(mean(get(myVar), na.rm = TRUE), sd(get(myVar), 
                                                        na.rm = TRUE))]
  }
  meltdx <- data.table::melt(dx, 
                            id.vars = c(dateGp), 
                            measure.vars = c("p99", "p50", "p1", "Mean", 
                                             "zerorate", "missingrate")
  )
  cl <- cl %*% matrix(c(1, 1, 1, -1), byrow = TRUE, nrow = 2) # mean +- 1 SD
  qq <- c(qq, cl)
  
  # 5 copies of dateGp for 5 global summary variables
  globaldx <- data.table(dateGp = rep(meltdx[variable == "p99", dateGp, 
                                             with = FALSE][[1]], 5))
  globaldx[, c("variable", "value") := list(rep(c("p99_g", "p50_g", 
                                                  "p1_g", "cl1", "cl2"), 
                                                each = .N / 5), 
                                            rep(qq, each = .N / 5))]
  meltdx <- rbindlist(list(meltdx, globaldx))
  return(meltdx)              	
}



#' Plots  01, 50, and 99 percentile together
#'
#' @param meltdx A data.table with p1, p50, and p99 in long format, produced by
#' \code{\link{PlotVar}}
#' @inheritParams PrepData
#' @inheritParams PlotVar
#' @return A ggplot object with \code{dateGp} on the x axis, \code{value} on the 
#' y axis, and variables \code{p01}, \code{p50} and \code{p99} plotted on the 
#' same graph, with grouped and global percentiles differentiated by linetype
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @export
#' @examples
#' data(bankData)
#' setDT(bankData)
#' bankData[, months := round(date, "months")]
#' bankDT = bankData[, {
#'   tmp1 = quantile(balance, p = c(.01, .5, .99));
#'   list("p1"  = tmp1[1] ,
#'        "p50" = tmp1[2] ,
#'        "p99" = tmp1[3]
#'   )}, by = "months"]
#'
#' bankMT = melt(bankDT, id.vars = "months", 
#'               measure.vars = c("p99", "p50","p1"))
#' globalPct = bankData[ , quantile(balance, p = c(.01, .5, .99) ) ]
#' globalDT = data.table("months" = rep(bankMT[variable == "p99", "months", 
#'                       with = FALSE][[1]], 3))
#' globalDT[, c("variable", "value") := list(rep(c("p1_g", "p50_g", "p99_g"), 
#'                                               each = .N/3),
#'                                           rep(globalPct, each = .N/3))]
#' bankMT = rbindlist(list( bankMT, globalDT))
#' # bankMT  # See long format used for plotting
#' PlotQuantiles(bankMT, "balance", "months")
PlotQuantiles <- function(meltdx, myVar, dateGp) {
  variable <- gp <- group <- NULL
  meltdx[, "group" := as.factor(ifelse(variable %in% c("p99", "p50", "p1"), 
                                       "by month", "global"))]
  meltdx[, variable := droplevels(variable)]
  levels(meltdx$variable) <- list(p99 = c("p99", "p99_g"), 
                                 p50 = c("p50", "p50_g"), 
                                 p1 = c("p1", "p1_g"))
  meltdx[, gp := paste(variable, group)]
  ggplot2::ggplot(meltdx, ggplot2::aes_string(x = dateGp, y = "value", 
                                      colour = "variable", lty = "group", 
                                      group = "gp")) + 
    ggplot2::geom_line() + ggplot2::ylab(NULL)
}


#' Plot mean with {Mean +- 1SD} control limits
#' 
#' @param meltdx A data.table with Mean and 1SD control limits in long format, 
#' produced by \code{\link{PlotVar}}
#' @inheritParams PrepData
#' @inheritParams PlotVar
#' @return A ggplot object with \code{dateGp} on the x axis, \code{value} on the
#' y axis, and variables \code{Mean}, \code{cl1} and \code{cl2} plotted on the
#' same graph, Mean and control limits differentiated by linetype
#' @export
#' @section License: 
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' setDT(bankData)
#' bankData[, months := round(date, "months")]
#' bankDT = bankData[, .( Mean = mean(balance)), by = "months"]
#' cl = bankData[, c(mean(balance), sd(balance))]
#' cl = cl %*% matrix(c(1, 1, 1, -1), byrow = TRUE, nrow = 2) # mean +- 1 SD
#' bankDT[, c("cl1", "cl2") := list(cl[1], cl[2])  ]
#' bankMT = melt(bankDT, id.vars = "months", 
#'          measure.vars = c("Mean", "cl1", "cl2"))
#' # bankMT # Long format for plotting
#' PlotMean(bankMT, "balance", "months")
PlotMean <- function(meltdx, myVar, dateGp){
  variable <- NULL
  setnames(meltdx, "variable", "var")
  meltdx[, variable := as.factor(ifelse(var != "Mean", "1SD CL", "mean"))]
  ggplot2::ggplot(meltdx, 
                  ggplot2::aes_string(x = dateGp, y = "value", group = "var", 
                                      linetype = "variable")) +
    ggplot2::geom_line(colour = "black") +
    ggplot2::scale_linetype_manual(values = c(2, 1), 
                                   breaks = c("mean", "1SD CL")) +
    ggplot2::ylab(NULL)
}


#' Plot zero and missing rates
#'
#' @param meltdx A data.table with missingrate and zerorate in long format, 
#' produced by \code{\link{PlotVar}}
#' @inheritParams PrepData
#' @inheritParams PlotVar
#' @export
#' @return A ggplot object with a \code{missingrate} and \code{zerorate} grouped 
#' by \code{dateGp}
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' setDT(bankData)
#' bankData[, months := round(date, "months")]
#' bankDT = bankData[, {list("zerorate" = mean(balance == 0),
#'                           "missingrate" = mean(is.na(balance)))}, 
#'                  by = "months"]
#' bankMT = melt(bankDT, id.vars = "months", 
#'               measure.vars = c("zerorate", "missingrate"))
#' # bankMT # Long format for plotting
#' PlotRates(bankMT, "balance", "months")
PlotRates <- function(meltdx, myVar, dateGp) {
  variable <- NULL
  ggplot2::ggplot(meltdx[variable %in% c("zerorate", "missingrate")], 
                  ggplot2::aes_string(x = dateGp, 
                                      y = "value", 
                                      colour = "variable", 
                                      group = "variable")) +
    ggplot2::geom_line() + ggplot2::ylab(NULL)
}

#' Simple grouped box plot
#'
#' @inheritParams PrepData
#' @inheritParams PlotVar
#' @return A ggplot object with a box plot of \code{myVar} grouped by 
#' \code{dateGpBp}
#' @export
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' setDT(bankData)
#' PrepData(dataFl = bankData, dateNm = "date", dateGp = "months", 
#'          dateGpBp = "quarters")
#' PlotDist(dataFl = bankData, myVar = "balance", dateGpBp = "quarters")
#' # The following attempt to log transform will fail due to negative values,
#' # and the untransformed version will be returned
#' PlotDist(dataFl = bankData, myVar = "balance", dateGpBp = "quarters", 
#'          skewOpt = 3)
#' # This attempt should succeed, as the skew exceeds 3 and there are no 
#' # negative values
#' PlotDist(dataFl = bankData, myVar = "duration", dateGpBp = "quarters",
#'          skewOpt = 3)
PlotDist <- function(dataFl, myVar, dateGpBp, weightNm = NULL, skewOpt = NULL){
  setkeyv(dataFl, dateGpBp)
  if (is.null(weightNm)) {
    p <- ggplot2::ggplot(dataFl, ggplot2::aes_string(x = dateGpBp, 
                                                     y = myVar, 
                                                     group = dateGpBp))
  } else {
    p <- ggplot2::ggplot(dataFl, ggplot2::aes_string(
      x = dateGpBp, y = myVar, group = dateGpBp, weight = weightNm))
  }
  p <- p + ggplot2::geom_boxplot() + ggplot2::ylab(myVar) + 
    ggplot2::scale_y_continuous() + 
    ggplot2::geom_rug(data = dataFl, 
                      mapping = ggplot2::aes_string(x = dateGpBp, 
                                                    y = myVar), 
                      sides = "l", position = "jitter", inherit.aes = FALSE, 
                      colour = "#F8766D", alpha = .4)
  
  # log10 transform of highly skewed variables, only if variable is non-negative
  # and has large (>50) # unique variables
  if (!is.null(skewOpt)) {
    M <- min(dataFl[, myVar, with = FALSE], na.rm = TRUE)
    if (M < 0) {
      message("range of ", myVar, " includes negative values, returning 
              untransformed boxplot")
    } else {
      if (dim(unique(dataFl[, myVar, with = FALSE]))[1] > 50){
        if (moments::skewness(dataFl[, myVar, with = FALSE], 
                              na.rm = TRUE) > skewOpt){
          p2 <- try(p + ggplot2::scale_y_log10() + 
                        ggplot2::ylab(paste(myVar, " (log10)")))
          if (inherits(p2, "try-error")) {
            message(paste("log transform failed, returning untransformed boxplot
                          of ", myVar))
          } else {
            p <- p2
          }
        }
      }
    }
  }
  return(p)
}



###########################################
#       Discrete Plotting Functions       #
###########################################


#' Plot Histogram of Discrete Variable
#'
#' @inheritParams PrepData
#' @inheritParams PlotVar
#' @export
#' @return A ggproto object with a histogram of \code{myVar} ordered by category frequency
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' setDT(bankData)
#' PrepData(bankData, dateNm = "date", dateGp = "months", dateGpBp = "quarters", 
#'          weightNm = NULL)
#' PlotHistogram(bankData, "job")
#' 
#' # NA will be included as a category if any NA are present
#' bankData[sample.int(.N)[1:1000], education := NA]
#' PlotHistogram(bankData, "education")
PlotHistogram <- function(dataFl, myVar, weightNm = NULL){
	count <- NULL
 	if (is.null(weightNm)) {
      glbTotals <- dataFl[, list(count = .N), by = myVar]
  	} else {
      glbTotals <- dataFl[, list(count = sum(get(weightNm))), by = myVar]
    }
  
    newLevels <- unlist(glbTotals[, myVar, with = FALSE][order(glbTotals[, -count])])
    glbTotals[, (myVar) := factor(get(myVar), levels = newLevels)]
  	
	p <- ggplot2::ggplot(glbTotals, ggplot2::aes_string(x = myVar, 
														y = "count",
                                                      	group = myVar)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_discrete(labels = abbreviate, breaks = newLevels) +
    ggplot2::theme(text = ggplot2::element_text(size = 10))
    return(p)
}


#' Plot Histogram of Discrete Variable Over Time
#'
#' @inheritParams PlotHistogram
#' @inheritParams PrepData
#' @inheritParams PlotVar
#' @param normBy "time" or "var"-- the normalization factor for rate plots. If "time" then 
#' line plots will be normalized by month, that is, all caltegories will sum to one within
#'  a particular month.  This allows you to compare the proportion of total coming from 
#' different categories at 
#' different points in time. For example, at month 1 10% volume came from category A and at 
#' month 6 75%. If total volume was constant or decreasing, this represents an increase 
#' in market share of category A. If "var", then each category is normalized independently. 
#' In the same example, if category A shows a decreasing trend using the "var" normalization,
#' this would imply that total volume is decreasing and category A market share is increasing. 
#' This example occurs for the "retired" job category in the bankData example data set. 
#' @param newLevels categories of myVar in order of global frequency
#' @export
#' @return A ggproto object with a histogram of \code{myVar} over time
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' setDT(bankData)
#' bankData[, weight := rpois(.N, 5)]
#' bankData[, weight := weight/sum(weight)]
#' PrepData(bankData, dateNm = "date", dateGp = "months", dateGpBp = "quarters", 
#'          weightNm = "weight")
#' PlotHistOverTime(dataFl = bankData, dateGp = "months", 
#' weightNm = "weight", myVar = "job", newLevels = NULL, normBy = "time")
#' PlotHistOverTime(dataFl = bankData, dateGp = "months", 
#' weightNm = "weight", myVar = "job", newLevels = NULL, normBy = "var")
#' 
PlotHistOverTime <- function(dataFl, dateGp, myVar, 
                             normBy = "time", weightNm = NULL, newLevels = NULL){
   N.x <- NULL 
   N.y <- NULL
   rate <- NULL
   N <- NULL
   count <- NULL
   
	 dataSub = dataFl[, c(dateGp, myVar, weightNm), with = FALSE]
	 dataSub[is.na(get(myVar)), (myVar) := "NA"]
	 
 	 if(is.null(newLevels)){
 	 	 if (is.null(weightNm)) {
     		glbTotals <- dataSub[, list(count = .N), by = myVar]
  		 } else {
   		    glbTotals <- dataSub[, list(count = sum(get(weightNm))), by = myVar]
          }

 		newLevels <- glbTotals[[myVar]][order(glbTotals[, -count])]
 	 }

	 hex = scales::hue_pal()(length(newLevels))[match(newLevels, c(dataFl[, sort(unique(get(myVar)))], "NA"))]
   
	 if (is.null(weightNm)) {
	   countData <- dataSub[, .N, by = c(myVar, dateGp)]
	   if(normBy == "time"){
	     countBy <- dataSub[, .N, by = c(dateGp)]
	   } else {
	     if(normBy == "var") {
	       countBy <- dataSub[, .N, by = c(myVar)]
	     }
	   }
	 } else {
	   countData <- dataSub[, list(N = sum(get(weightNm))), by = c(myVar, dateGp)]
	   if(normBy == "time"){
	     countBy <- dataSub[, list(N = sum(get(weightNm))), by = c(dateGp)]
	   } else {
	     if(normBy == "var") {
	       countBy <- dataSub[, list(N = sum(get(weightNm))), by = c(myVar)]
	     }
	   }
	 }
	
	 
	 crossLevels <- CJ(unique(countData[[dateGp]]), unique(countData[[myVar]]))
	 setnames(crossLevels, c("V1", "V2"), c(dateGp, myVar))
	 countData = merge(crossLevels, countData, all.x=TRUE, by = c(dateGp, myVar))
	 countData[is.na(N), N := 0]
	 countData[, (myVar) := factor(get(myVar), levels = newLevels)]
	 
	 if(normBy == "time"){
	   rateBy <- merge(countData, countBy, by = dateGp)
	 } else {
	   if(normBy == "var") {
	     rateBy  <- merge(countData, countBy, by = myVar)
	   }
	 }
	
	 rateBy[, rate := N.x / N.y]
	 rateBy[, (myVar) := factor(get(myVar), levels = newLevels)]
   
	 # Plot less frequent category only, helps when there is a large class imbalance
	 if (length(newLevels) == 2) {
	   rateBy <- rateBy[get(myVar) == newLevels[2]]
	 }
	 
   p <- ggplot2::ggplot(rateBy,
                        ggplot2::aes_string(x = dateGp, y = "rate")) +
     ggplot2::geom_line(stat = "identity")  + 
    facet_wrap(stats::as.formula(paste("~", myVar))) +
     ggplot2::ylab("") +
     ggplot2::scale_x_date() 
   
  
	return(p)
}




###########################################
#         Prepare Data                    #
###########################################

#' Clean an input dataset for plotting
#'
#' Cleans an input dataset for use by otvPlots. \code{dataFl} must contain, at a
#' minimum, a date column \code{dateNm} and a variable to be plotted.
#'
#' @param dataFl Either the name of an object that can be converted using
#' as.data.table (e.g. a ' data frame), or a character string containing the name
#' of dataset that can be loaded using fread (e.g. a csv file), or a file path of Rdata file. ' If dataset is
#' not in your working directory then \code{dataFl} must include (relative or 
#' absolute) path to file
#' @param selectCols Either NULL, or a vector contaning names or indices of
#' variables to read into memory -- must include \code{dateNm},
#' \code{weightNm} (if not null) and all variables to be plotted. If both
#' selectCols and dropCols are null, then all variables will be read in. Only
#' used when dataFl is a string constant. When dataFl is already a dataset in memory, use varNms
#' to indicate the column names to be plotted.
#' @param dropCols Either NULL or a vector of variables not to read into memory.
#' This parameter is passed directly to fread (see selectCols)
#' @param dateNm Name of column containing date variable
#' @param dateFt strptime format of date variable. Default is SAS format ("\%d\%h\%Y"). But 
#' input data with R date format (yyyy-mm-dd) will be detected and \code{dateFt} will 
#' be changed to "\%Y-\%m-\%d" automaticallly. See ?strptime
#' @param dateGp Name of the variable the time series plots should be grouped
#' by. Options are NULL, "weeks", "months", "quarters", "years". See
#' data.table::IDate. If NULL \code{dateNm} will be used.
#' @param dateGpBp Name of variable the boxplots should be grouped by. Same
#' options as \code{dateGp}. If NULL \code{dateGp} will be used.
#' @param weightNm Name of variable containing row weights or NULL for no
#' weights (all rows recieve weight 1)
#' @param varNms Either NULL or a vector of column names to be plotted. If null,
#' will default to all columns which are not \code{dateNm} or \code{weightNm}.
#' Can also be a vector of indices of the column names, after dropCols or
#' selectCols have been applied, if applicable, and not including dateGp,
#' dateGpBp (which will be added to the data.table)
#' @param dropConstants logical Indicates whether or not constant (all
#' duplicated or NA) variables should be dropped from dataFl prior to plotting
#' @param ... Additional parameters to be passed to fread
#' @return A data table formated for use by \code{PlotVar} function
#' @seealso \code{\link{fread}}
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @export
#' @examples
#' data(bankData)
#' setDT(bankData)
#' PrepData(bankData, dateNm = "date", dateGp = "months", dateGpBp = "quarters")
#' # columns have been assigned a plotting class (cntns/dscrt)
#' str(bankData) 
#' 
PrepData <- function(dataFl, dateNm, selectCols = NULL, dropCols = NULL,
                    dateFt = "%d%h%Y", dateGp = NULL, dateGpBp = NULL,  
                    weightNm = NULL, varNms = NULL, dropConstants = TRUE, ...){
  if (is.character(dataFl)) {
    fileExt = tolower(tools::file_ext(dataFl))
    if (fileExt %in% c("rdata", "rda")){
      dataFl <- readRDS(dataFl)
      setDT(dataFl)
      if (!is.null(selectCols) & !is.null(dropCols)) {
        stop('Dont\'t assign value to both selectCols and dropCols.')
      } else if (!is.null(selectCols)){
        dataFl <- dataFl[ , (tolower(names(dataFl)) %in% selectCols), with=FALSE]
      } else if (!is.null(dropCols)){
        dataFl <- dataFl[ , !(tolower(names(dataFl)) %in% dropCols), with=FALSE]
      }
    } else {
      stopifnot(! (!is.null(selectCols) & !is.null(dropCols)) )
      origHeader <- names(fread(dataFl, nrows = 0))
      select = origHeader
      if (!is.null(selectCols) | !is.null(dropCols)) {
        if (!is.null(selectCols)){
          select <- origHeader[match(tolower(selectCols), tolower(origHeader))] 
          dataFl <- fread(dataFl, select = select, stringsAsFactors = FALSE, integer64 = "double", ...)
        }
        if (!is.null(dropCols)){
          drop <- origHeader[match(tolower(dropCols), tolower(origHeader))]  
          dataFl <- fread(dataFl, drop = drop, stringsAsFactors = FALSE, integer64 = "double", ...)
        }
      } else {
        dataFl <- fread(dataFl, select = select, stringsAsFactors = FALSE, integer64 = "double", ...)
      }
    }
  } else {
    if (!is.data.table(dataFl)) {
      setDT(dataFl)
    }
  }
  
  stopifnot(tolower(c(dateNm, weightNm)) %in% tolower(names(dataFl)))
  
  # ensure all integer64 types are treated as numeric
 for (var in names(dataFl)) {
  	if (inherits(dataFl[[var]], "integer64")) {
  		dataFl[, (var) := as.numeric(get(var))]
  	}
  }
  
  
  if (any(c(sapply(dataFl, class)) %in% c("cntns", "dscrt"))) {
    message ("PrepData has already been run on this data set.")
  }
  
  if (!is.null(weightNm)) {
  	stopifnot(is.numeric(dataFl[[weightNm]]))
    if (dataFl[is.na(weightNm), .N ] > 0 ) {
      warning ("Missings in weight column. Imputing to zero.")
      dataFl[is.na(get(weightNm)), (weightNm) := 0]
    }
    # normalize weights for consistent treatment
    dataFl[, (weightNm) := get(weightNm)/sum(get(weightNm))]
  }
  
  # strip all special characters from column names (ggplot really dislikes them)
  setnames(dataFl, tolower(gsub("\\.|/|\\-|\"|\\s", "", names(dataFl))))
  
  if (!is.null(dropCols)) {
    # make sure any unwanted columns are dropped
    dropCols <- tolower(gsub("\\.|/|\\-|\"|\\s", "", dropCols))
    stopifnot(!c(dateNm, weightNm) %in% dropCols)
    if (any(dropCols %in% names(dataFl))) {
      dropCols <- dropCols[dropCols %in% names(dataFl)]
      dataFl[, c(dropCols) := NULL]
    }
  }
  
  # keep just a subset of columns, without throwing away the date 
  # and weight columns
  if (!is.null(selectCols)) {
    #strip special characters before trying to match
    selectCols <- tolower(gsub("\\.|/|\\-|\"|\\s", "", selectCols)) 
    selectCols <- unique(c(selectCols, dateNm, weightNm))
    stopifnot(c(dateNm, weightNm) %in% selectCols)
    if (length(setdiff(names(dataFl), selectCols)) > 0) {
      dataFl[, c(names(dataFl)[!names(dataFl) %in% selectCols]) := NULL]
    }
  }
  
  dateNm <- tolower(gsub("\\.|/|\\-|\"|\\s", "", dateNm))
  
  # Convert date to IDate according to provided format and give warning if format
  # produces NAs
  tmp.N = dataFl[is.na(get(dateNm)), .N] + dataFl[as.character(get(dateNm))=="", .N]
  nonNADateIndex <- which(!is.na(dataFl[, ..dateNm]))
  firstNonNA <- min(nonNADateIndex)
  nonNADate <- dataFl[firstNonNA, ..dateNm]
  if (grepl("([0-9]{4}-[0-9]{2}-[0-9]{2})", nonNADate[[1]])) {
    dateFt = "%Y-%m-%d"
  }
  dataFl[, c(dateNm) := as.IDate(get(dateNm), format = dateFt)]
  if( dataFl[is.na(get(dateNm)), .N] > tmp.N) {
  	warning (paste0("Formatting ", dateNm, " as \"", dateFt, "\" produces NAs"))
  }
  
  if (!is.null(dateGp) &&
      dateGp %in% c("weeks", "months", "quarters", "years")) {
    # dates are rounded to level specified and results are saved for faster 
    # aggregation. dateGp will be the primary DT key
    dataFl[, c(dateGp) := round(get(dateNm), dateGp)]
    setkeyv(dataFl, dateGp)
  } else {
    message (paste0("Time series plots will be grouped by ", dateNm, ".  
    If this was not your intention, make sure that dateGp is one of the 
    IDate rounding functions ('weeks', 'months', 'quarters', 'years')"))
    setkeyv(dataFl, dateNm)
  }
  
  if (!is.null(dateGpBp) && dateGpBp %in% 
      c("weeks", "months", "quarters", "years")) {
    # if a box plot grouping variable is given, a new rounded date variable is 
    # made for boxplots
    dataFl[, c(dateGpBp) := round(get(dateNm), dateGpBp)]
  } else {
    message (paste0("Boxplots will be grouped by ", dateNm, ". 
    If this was not your intention, make sure dateGpBp is one of the 
    IDate rounding functions ('weeks', 'months', 'quarters', 'years')"))
  }
  
  
  if (is.null(varNms)) {
    # if null, use all variables except date, weight
    vars <- names(dataFl)
  } else {
    if (is.numeric(varNms)) {
      vars <- names(dataFl)[varNms]
    } else {
      vars <- varNms
    }
  }
  
  vars <- vars[!vars %in% c(dateNm, weightNm)]
  
  # if current variable set to be weight/date has previously been given
  # a plotting type, remove it now
  
  if (!is.null(weightNm)) {
  	if (length(intersect(c("cntns", "dcsrt"), attr(dataFl[[weightNm]], "class"))) > 0) {
  		attr(dataFl[[weightNm]], "class") <- attr(dataFl[[weightNm]], "class")[[1]]
  	} 
  }
  
  if (length(intersect(c("cntns", "dcsrt"), attr(dataFl[[dateNm]], "class"))) > 0) {
  	attr(dataFl[[dateNm]], "class") <- attr(dataFl[[dateNm]], "class")[[1]]
  } 

  
  # check that this is working correctly
  # consider replacing the warning message with creating new missing indicators 
  # for variables with NA and 1 other unique value at least should export the 
  # message in a more concrete way so it's not missed
  bad_ind <- vapply(dataFl[, c(vars), with = FALSE], 
                    function(x) all(duplicated(stats::na.omit(x))[-1L]), logical(1))
  if (any(c(dateGp, dateGpBp) %in% vars[bad_ind])) {
    warning ("No variability in grouping variables. Select a new grouping level.")
    }
  
  if (dropConstants){
    if (sum(bad_ind) > 0) {
      warning (paste(
        c("The following variables have no variability and will be dropped: ",
          paste(vars[bad_ind], collapse = ", ")), collapse = ""))
      
      badVars <- vars[bad_ind]
      dataFl[, c(badVars) := NULL]
    }
  }
  
  vars <- vars[!bad_ind] # exclude variables with no variance
  stopifnot(length(vars) > 0) # stop if no variables to plot
  
  date_ind <- sapply(dataFl[, c(vars), with = FALSE], inherits, "Date")

  # we do not want to plot any dates
  vars <- vars[date_ind == 0] 
  # stop if no variables to plot
  stopifnot(length(vars) > 0) 
  # numeric variables
  num_ind  <- sapply(dataFl[, c(vars), with = FALSE], function(z) is.numeric(z))
  # categorical variables
  nom_ind  <- vapply(dataFl[, c(vars), with = FALSE], function(z)
    class(z)[1] %in% c("character", "factor"), logical(1) )
  # binary variables (nominal or numeric)
  bin_ind  <- sapply(dataFl[, c(vars), with = FALSE], function(z)
    uniqueN(stats::na.omit(z)) == 2)
  
  continuousVars <- vars[num_ind == 1 & nom_ind == 0 & bin_ind == 0]
  discreteVars <- vars[nom_ind  == 1 | bin_ind  == 1]
  
  if (length(continuousVars) > 0) {
    invisible(lapply(1:length(continuousVars), function(z) 
      setattr(dataFl[[continuousVars[z]]], "class", 
              unique(c(class(dataFl[[continuousVars[z]]]), "cntns")))))
  }
  
  if (length(discreteVars) > 0) {
    for (z in vars[bin_ind]) {
        dataFl[, (z) := as.character(get(z))]
      }
    
    invisible(lapply(1:length(discreteVars), function(z) 
      setattr(dataFl[[discreteVars[z]]], "class", 
              unique(c(class(dataFl[[discreteVars[z]]]), "dscrt")))))
  }
  
  message (paste(c("The following variables will be plotted:\nNumeric: ", 
              paste0(continuousVars, collapse = " "), "\nDiscrete: ", 
              paste0(discreteVars, collapse = " "),  "\n"), sep = " ") )

  return(dataFl)
}

###########################################
#           Prepare Labels                #
###########################################

#' Clean a dataset containing variable labels
#'
#' Prepares a dataset containing variable labels for use by otvPlots. To work
#' correctly, input must contain names in first column and labels in second
#' column. All other columns will be dropped. Special characters will create
#' errors and should be stripped outside of R. All labels will be truncated at
#' 145 characters.
#'
#' @param labelFl Either the name (and possibly path) of a dataset containing
#' labels, an R object convertible to data.table (eg data frame) or NULL. If
#' name is given, if the dataset is not in your working directory then
#' \code{labelFl} must include (relative or absolute) path to file. If NULL no
#' labels will be used. Label dataset must contain at least 2 columns:
#' variable names and variable label.
#' @param idx Vector of length 2 giving column index of variable names (first
#' position) and labels (second position)
#' @export
#' @return A data table formated for use by \code{PlotVar} function
#' @section License: 
#' Copyright 2016 Capital One Services, LLC Licensed under the
#' Apache License, Version 2.0 (the "License"); you may not use this file
#' except in compliance with the License. You may obtain a copy of the 
#' License at http://www.apache.org/licenses/LICENSE-2.0 Unless required by
#' applicable law or agreed to in writing, software distributed under the
#' License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
#' CONDITIONS OF ANY KIND, either express or implied. See the License for the
#' specific language governing permissions and limitations under the License.
#' @examples
#' data(bankLabels)
#' setDT(bankLabels)
#' PrepLabels(bankLabels)
PrepLabels <- function(labelFl, idx = 1:2) {
  varCol <- labelCol <- NULL
  if (!is.null(labelFl)) {
    if (is.character(labelFl)) {
      labelFl <- fread(labelFl, select = idx, stringsAsFactors = FALSE)
    } else {
      setDT(labelFl)
      vec <- 1:ncol(labelFl)
      if (tail(vec, 1) > 2) {
        labelFl[, vec[!vec %in% idx] := NULL]
      }
    }
    setnames(labelFl, c("varCol", "labelCol"))
    labelFl <- subset(labelFl, subset = (!varCol == ""))
    labelFl[, varCol := tolower(gsub("\\.|/|\\-|\"|\\s", "", varCol))]
    labelFl[, labelCol := stringi::stri_trans_general(labelCol, "latin-ascii")]
    labelFl[, labelCol := strtrim(labelCol, 145)]
    
    labelFl[nchar(labelCol) > 144, labelCol := paste0(labelCol, "...")]
  }
  return(labelFl)
}


###########################################
#          Order By R2                    #
###########################################

#' Create variable ranking using R2 of simple linear model using date to predict
#' variables
#' 
#' Calculates R2 of a linear model of the form \code{var} ~ \code{dateNm} for
#' each \code{var} of class \code{cntns} or \code{dscrt} and returns a vector of
#' variable names ordered by highest R2. The linear model can be calculated over
#' a subset of dates, see details of parameter \code{buildTm}. Non-numeric
#' variables are returned in alphabetical order after the sorted numberic
#' variables.
#'
#' @inheritParams PlotVar
#' @param buildTm Vector identify time period for ranking/anomaly detection
#' (most likely model build period). Allows for a subset of plotting time
#' period to be used for anomaly detection.
#' \itemize{
#'      \item Must be a vector of dates and must be inclusive i.e. buildTm[1]
#'        <= date <= buildTm[2] will define the time period.
#'      \item Must be either NULL, a vector of length 2, or a vector of length
#'        3. 
#'      \item If NULL, entire dataset will be used for ranking/anamoly
#'        detection. 
#'      \item If a length 2 vector, the format of the dates must be
#'        a character vector in default R date format (e.g. "2016-01-30"). 
#'      \item If a length 3 vector, the first two columns must contain dates 
#'        in any strptime format, while the 3rd column contains the strptime 
#'        format (see \code{\link{strptime}}). 
#'      \item The following are equivalent ways of selecting
#'        all of 2014:
#'      \itemize{
#'	      \item \code{c("2014-01-01","2014-12-31")}
#'      	\item \code{c("01JAN2014","31DEC2014", "\%d\%h\%Y")}
#'     	}
#' }
#' @param kSample Either NULL or integer. If a number, indicates the sample size
#' for both drawing boxplots and ordering numerical graphs by R2. For large
#' datasets, setting this to a reasonable (say 50K) value dramatically
#' increases processing speed. In larger datasets (e.g. > 10 percent system
#' memory), this parameter should not be set to NULL, or boxplots may take a
#' very long time to render. This setting has no impact on the accuracy of
#' time series plots.
#' @export
#' @return A vector of variable names sorted by R2 of lm of the form \code{var}
#' ~ \code{dateNm} (highest R2 to lowest)
#' @section License: 
#' Copyright 2016 Capital One Services, LLC Licensed under the
#' Apache License, Version 2.0 (the "License"); you may not use this file
#' except in compliance with the License. You may obtain a copy of the 
#' License at http://www.apache.org/licenses/LICENSE-2.0 Unless required by
#' applicable law or agreed to in writing, software distributed under the
#' License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
#' CONDITIONS OF ANY KIND, either express or implied. See the License for the
#' specific language governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' setDT(bankData)
#' # Nothing will be returned because the variables are uncategorized. Run
#' # PrepData first to get appropriate classes added to the columns.
#' \dontrun{OrderByR2(bankData, dateNm = "date", buildTm = NULL)}
#' # The returned vector will have the numeric columns first, sorted (pdays,
#' # previous, campaign, balance, duration, age) followed by the categorical
#' # variables in the same order they appear in bankData (job, marital,
#' # education, etc). Binary variables are always considered categorical.
#' PrepData(bankData, dateNm = "date", dateGp = "months", dateGpBp = "quarters")
#' OrderByR2(bankData, dateNm = "date")
#'
OrderByR2 <- function(dataFl, dateNm, buildTm = NULL, weightNm = NULL, 
                     kSample = 50000) {
           
  if(!is.null(weightNm)){ 	                  	
 	 if (any(is.na(dataFl[[weightNm]]))) {
 	 	warning("Weights column contains NAs--will be deleted casewise")
  	 }
  }
  
  if (any(is.na(dataFl[[dateNm]]))) {
  	warning("Date column contains NAs--will be deleted casewise")
  }
  
  # Convert buildTm to IDate format
  buildTm <- switch(as.character(length(buildTm)), "2" = as.IDate(buildTm),
    "3" = as.IDate(buildTm[1:2], buildTm[3]), 
    # avoid inheritence as list using [[]]
    dataFl[c(1, .N), dateNm, with = FALSE][[1]])  
  
  num_vars <- names(Filter(is.cntns, dataFl)) 
  cat_vars <- names(Filter(is.dscrt, dataFl))
  
  if (length(num_vars > 0)) {
    # Sorting by R2 only works for numeric variables.
  
    # Using sample directly in dataFl parameter for brevity, 
    # which reorders the input to CalcR2 but does not change output
    r2 <- vapply(num_vars, CalcR2, 
                dataFl = dataFl[buildTm[1] <= get(dateNm) & 
                                  get(dateNm) <= buildTm[2], ][
                                    sample(.N, min(.N, kSample))], 
                dateNm = dateNm, weightNm = weightNm, imputeValue = NULL, 
                numeric(1))
    sortVars <- c(num_vars[order(r2, decreasing = TRUE)], cat_vars)
  } else {
    sortVars <- cat_vars
  }
  
  return(sortVars)
}


###########################################
#           CalcR2 Function               #
###########################################

#' Calculates R2 of variable using date as predictor
#'
#' Calculates weighted R2 of a univariate weighted linear model with
#' \code{dateNm} as x and \code{myVar} as y using the workhorse lm.fit and
#' lm.wfit functions
#'
#' @param myVar Name of variable to model
#' @param dataFl A data.table containing myVar, dateNm and inweight
#' @param dateNm Name of column containing date variable (to be modeled as 
#' numeric)
#' @param weightNm Name of column containing row weights. If all(w==1), lm.fit
#' will be called, else lm.wfit will be called.
#' @param imputeValue Either NULL or numeric. If NULL model will be fit on
#' non-NA components of myVar. (dateNm, inweight must not have NA cases). If
#' numeric, missing cases of myVar will be imputed to imputeValue
#' @section License:
#' Copyright 2016 Capital One Services, LLC Licensed under the Apache License,
#' Version 2.0 (the "License"); you may not use this file except in compliance
#' with the License. You may obtain a copy of the  License at
#' http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law
#' or agreed to in writing, software distributed under the License is 
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY 
#' KIND, either express or implied. See the License for the specific language 
#' governing permissions and limitations under the License.
#' @export
CalcR2 <- function(myVar, dataFl, dateNm, weightNm = NULL, imputeValue = NULL) {
  message("Calculating R2 of ", myVar)
  
  if (sum(!is.na(dataFl[[myVar]])) < 2) {
    return(Inf)
    # if kSample is not null, then we need to recheck that the subsample is not 
    # all missing. If there are less than 2 numeric values left after sampling 
    # we can't calculate R2
  } else {
  	y <- dataFl[[myVar]]
  	
  	# if imputeValue is available, we impute everywhere Y is missing
    if (!is.null(imputeValue)) {
    	y[is.na(y)] <- imputeValue
   	} 
   	yIdx <- which(is.na(y))

	# We perform casewise deletion anywhere X, Y or W (if not null) is missing
    if (!is.null(weightNm)) {
      w <- dataFl[[weightNm]]
      wIdx <- which(is.na(w))
      yIdx = unique(c(yIdx, wIdx))
    }    
    
    x <- cbind(1, as.matrix(as.numeric(dataFl[[dateNm]]), ncol = 1))
    xIdx <- which(is.na(x[, 2]))
    yIdx <- unique(c(xIdx, yIdx))
      
      
    if (length(yIdx) > 0) {
	  if (!is.null(weightNm)) {
    		w <- w[-c(yIdx)]
   	  } 
    
      y <- y[-c(yIdx)]
      x <- x[-c(yIdx),]
    } 
          
    if (is.null(weightNm)) {
      mod <- lm.fit(x = x, y = y)
      r2  <- 1 - sum(mod$resid ^ 2) / sum( (y - mean(y)) ^ 2)
    } else {
      mod <- lm.wfit(x = x, y = y, w = w)
      r2  <- 1 - sum(w * 
                       mod$resid ^ 2) / sum(w * (y - Hmisc::wtd.mean(y, w, normwt = TRUE)) ^ 2)
    }
    return(r2)
  }
}


###########################################
#           Utility Functions             #
###########################################

is.cntns <- function(x)  inherits(x, "cntns") 
is.dscrt <- function(x)  inherits(x, "dscrt")

wtd.quantile_NA <- function(x, weights, probs = c(.0, .25, .5, .75, 1), 
                            ...) {
  tryCatch(as.double(Hmisc::wtd.quantile(x, weights, probs, 
  	normwt=TRUE, na.rm=TRUE, ...)),
    error = function(e) rep(NA_real_, length(probs)))
}
