###########################################
#           Create output                 #
###########################################

#' Create pdf output with plots for each variable arranged on a single page
#'
#' Creates plots and outputs results to a pdf named as \code{outFl}. Creates
#' letter-sized output with each page corresponding to a single variable.
#'
#' @inheritParams PlotVar
#' @param outFl Name of output file (e.g., "otvPlots.pdf"). PDF will be
#'   saved to your working directory unless a path is included in \code{outFl}
#'   (e.g. "../plots/otvPlots.pdf").
#' @param sortVars A character vector of variable names in the order they will
#'   be plotted. 
#' @return A pdf of plots saved to file \code{outFl}.
#' 
#' @seealso Functions depend on this function:
#'          \code{\link{vlm}}.
#' @seealso This function depends on:
#'          \code{\link{PlotVar}},
#'          \code{\link{PrepData}}.
#'          
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
                       kCategories = 9) {
  
  catSummary <- NULL
  numSummary <- NULL
  . <- NULL
  
  plotList <-
    lapply(sortVars, PlotVar,
           dataFl = dataFl, weightNm = weightNm, dateNm = dateNm,
           dateGp = dateGp, dateGpBp = dateGpBp, labelFl = labelFl,
           highlightNms = highlightNms, skewOpt = skewOpt,
           fuzzyLabelFn = fuzzyLabelFn, kCategories = kCategories)
  
  grDevices::pdf(file = outFl,  width = 11, height = 8, pointsize = 12,
      onefile = TRUE)
  
  for (x in plotList)  {
    grid::grid.newpage()
    grid::grid.draw(x$p)
    
    if(x$varType == "ctgrl")
      catSummary = rbind(catSummary, x$varSummary) 

    if(x$varType == "nmrcl")
      numSummary = rbind(numSummary, x$varSummary) 
  }
  dev.off()
  
  ## Compute counts in each time
  if (is.null(weightNm)){  
    total_counts = dataFl[, list(count = .N), by = dateGp]
  } else{
    total_counts = dataFl[, list(count = sum(get(weightNm))), by = dateGp]
  }
  total_counts = dcast(total_counts, . ~ months, value.var = 'count')
  total_counts[, . := NULL]
  
  ## Add a row of counts at the begining of catSummary
  catSummary = rbind(as.list(rep(NA, ncol(catSummary))), catSummary)
  catSummary[1, 1:2] = list('ALL_DATA', 'COUNTS')
  catSummary[1, 3:4] = list(sum(total_counts), 1)
  catSummary[1, names(catSummary)[-(1:4)] := total_counts];  

  ## Add a row of counts at the begining of numSummary
  numSummary = rbind(as.list(rep(NA, ncol(numSummary))), numSummary)
  numSummary[1, 1:2] = list('ALL_DATA', 'COUNTS')
  numSummary[1, 3] = sum(total_counts)
  numSummary[1, names(numSummary)[-(1:3)] := total_counts];  
  
  
  return(list(catSummary = catSummary, numSummary = numSummary))
}

###########################################
#          Main Plot Function             #
###########################################

#' Create overtime variable plots for one variable
#' 
#' For a categorical variable (including a numerical variable with no more than 2
#' unique levels not including NA), frequency/rate graphs are used. The output 
#' includes 
#' \itemize{
#' \item a frequency bar plot on the left, and 
#' \item a grid of trace plots on categories' proportions over time. If the
#' variable contains more than \code{kCategories} number of categories, trace
#' plots of only the largest \code{kCategories} will be plotted. 
#' }
#' For a numerical variable, the output includes 
#' \itemize{
#' \item side-by-side boxplotx grouped by \code{dateGpBp}, 
#' \item a trace plot of p1, p50 and p99 percentiles, 
#' \item a trace plot of mean and +-1 SD control limits, and 
#' \item a trace plot of missing and zerorates,
#' }
#' with all trace plots being grouped by \code{dateGp}.
#'
#' @inheritParams PlotCatVar
#' @inheritParams PlotNumVar
#' @inheritParams OrderByR2
#' @param dataFl A \code{data.table} containing at least the following columns:
#'   \code{myVar}, \code{weightNm}, \code{dateGp}, \code{dateGpBp}; usually an
#'   output of the \code{\link{PrepData}} function.
#' @param myVar Name of the variable to be plotted.
#' @param labelFl A \code{data.table} containing variable labels, or \code{NULL}
#'   for no labels; usually an output of \code{\link{PrepLabels}}.
#' @param highlightNms Either \code{NULL} or a character vector of variables to
#'   recieve red label. Currently \code{NULL} means all variables will get a 
#'   black legend. Ignored this argument if \code{labelFl == NULL}.
#' @param fuzzyLabelFn Either \code{NULL} or a function of 2 parameters: A label
#'   file in the format of an output by \code{\link{PrepLabels}} and a string
#'   giving a variable name. The function should return the label corresponding
#'   to the variable given by the second parameter. This function should 
#'   describe how fuzzy matching should be performed to find labels (see example
#'   below). If \code{NULL}, only exact matches will be retuned.
#' @return A \code{grob} (i.e., \code{ggplot} grid) object. 
#' @export
#' 
#' @seealso Functions depend on this function:
#'          \code{\link{PrintPlots}}.
#' @seealso This function depends on:
#'          \code{\link{PlotCatVar}},
#'          \code{\link{PlotNumVar}},
#'          \code{\link{PrepData}}.
#'          
#' @section License: Copyright 2016 Capital One Services, LLC Licensed under the
#' Apache License, Version 2.0 (the "License"); you may not use this file
#' except in compliance with the License. You may obtain a copy of the  License
#' at http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable
#' law or agreed to in writing, software distributed under the License is
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#' KIND, either express or implied. See the License for the specific language
#' governing permissions and limitations under the License.
#' @examples
#' data(bankData)
#' bankData = PrepData(bankData, dateNm = "date", dateGp = "months", dateGpBp = "quarters")
#' data(bankLabels)
#' bankLabels = PrepLabels(bankLabels)
#'
#' ## PlotVar will treat numerical and categorical data differently. 
#' ## Binary data is always treated as categorical.
#' plot(PlotVar(bankData, myVar = "duration", weightNm = NULL, dateNm = "date", 
#'      dateGp = "months", dateGpBp =  "quarters", labelFl = bankLabels)$p)
#' plot(PlotVar(bankData, myVar = "job", weightNm = NULL, dateNm = "date", 
#'      dateGp = "months", dateGpBp =  "quarters", labelFl = bankLabels)$p)
#' plot(PlotVar(bankData, myVar = "loan", weightNm = NULL, dateNm = "date", 
#'      dateGp = "months", dateGpBp =  "quarters", labelFl = bankLabels)$p)
#'
PlotVar <- function(dataFl, myVar, weightNm, dateNm, dateGp, dateGpBp = NULL,
                    labelFl = NULL, highlightNms = NULL, skewOpt = NULL,
                    kSample = 50000, fuzzyLabelFn = NULL, kCategories = 9) {
  
  varCol <- labelCol <- NULL
  message(paste("Plotting ", myVar))
  
  ## Make sure that myVar is not a date type
  if (any(is.element(unlist(dataFl[, class(get(myVar))]),
                     c("Date", "IDate")))) {
    stop("Cannot plot dates")
  }
  
  ## Label myVar type to be "nmrcl" or "ctgrl" if not labeled yet
  if (!(inherits(myVar, "ctgrl") | inherits(myVar, "nmrcl"))) {
    if (dataFl[, class(get(myVar))] %in% c("character", "factor") ||
        dataFl[, length(unique(stats::na.omit(get(myVar))))] == 2) {
      setattr(dataFl[, get(myVar)], "class", "ctgrl")
    } else {
      setattr(dataFl[, get(myVar)], "class", "nmrcl")
    }
  }
  
  ## Generate a grid of plots
  if (inherits(dataFl[[myVar]], "ctgrl")) {
    p_all <- PlotCatVar(myVar, dataFl, weightNm, dateNm, dateGp, kCategories)
    p <- p_all$p
    varSummary <- p_all$catVarSummary
    varType <- "ctgrl"
  } else if (inherits(dataFl[[myVar]], "nmrcl")) {
    p_all <- PlotNumVar(myVar, dataFl, weightNm, dateGp, dateGpBp, skewOpt,
                     kSample)
    p <- p_all$p
    varSummary = p_all$numVarSummary
    varType <- "nmrcl"
  }
  
  ## If no fuzzy matching functions are provided, provide exact matches on the 
  ## first column, otherwise use logic defined in fuzzyLabelFn
  ll <- myVar
  subHeight <- grid::unit(12, "points")
  if (!is.null(labelFl)) {
    if (is.null(fuzzyLabelFn)) {
      ll <- paste0(labelFl[varCol == myVar, labelCol])
    } else {
      ll <- fuzzyLabelFn(labelFl, myVar)
    }
    ll <- paste0(myVar, " (", ll, ")", "\n")
  }
  
  ## Label color
  subCol <- "black"
  if (!is.null(highlightNms)) {
    highlightNms <- gsub("/|\\-|\"|\\s", "", highlightNms)
    if (myVar %in% highlightNms) {
      # should add other ways to trigger red labels
      subCol <- "red"
    }
  }
  
  ## Add the page title as myVar and its label above the grid of plots
  subText <- grid::textGrob(ll, gp = grid::gpar(col = subCol, fontface="bold"))
  grobHeights <- grid::unit.c(grid::unit(1, "npc") - subHeight, subHeight)
  p <- gridExtra::arrangeGrob(p, top = subText)
  
  return(list(p = p, varSummary = varSummary, varType = varType))
}
