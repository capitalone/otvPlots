###########################################
#      Discrete Plot Method               #
###########################################
#' Frequency and proportion plots for discrete variables
#'
#' @inheritParams PrepData
#' @param myVar Name of the variable to be plotted
#' @param kCategories If a categorical variable has more than kCategories, only 
#' a global histogram will be plotted, rate plots for all categories will also be
#' plotted.
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
#       Discrete Plotting Functions       #
###########################################


#' Plot Histogram of Discrete Variable
#'
#' @inheritParams PrepData
#' @inheritParams PlotDiscreteVar
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
  
  newLevels <-
    unlist(glbTotals[, myVar, with = FALSE][order(glbTotals[, -count])])
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
#' @inheritParams PlotDiscreteVar
#' @inheritParams PrepData
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
PlotHistOverTime <- function(dataFl, dateGp, myVar, normBy = "time",
                             weightNm = NULL, newLevels = NULL){
  N.x <- NULL
  N.y <- NULL
  rate <- NULL
  N <- NULL
  count <- NULL
  
  dataSub <- dataFl[, c(dateGp, myVar, weightNm), with = FALSE]
  dataSub[is.na(get(myVar)), (myVar) := "NA"]
  
  if (is.null(newLevels)){
    if (is.null(weightNm)) {
      glbTotals <- dataSub[, list(count = .N), by = myVar]
    } else {
      glbTotals <- dataSub[, list(count = sum(get(weightNm))), by = myVar]
    }
    
    newLevels <- glbTotals[[myVar]][order(glbTotals[, -count])]
  }
  
  hex <- scales::hue_pal()(length(newLevels))[match(newLevels, c(dataFl[, sort(unique(get(myVar)))], "NA"))]
  
  if (is.null(weightNm)) {
    countData <- dataSub[, .N, by = c(myVar, dateGp)]
    if (normBy == "time"){
      countBy <- dataSub[, .N, by = c(dateGp)]
    } else {
      if (normBy == "var") {
        countBy <- dataSub[, .N, by = c(myVar)]
      }
    }
  } else {
    countData <- dataSub[, list(N = sum(get(weightNm))), by = c(myVar, dateGp)]
    if (normBy == "time"){
      countBy <- dataSub[, list(N = sum(get(weightNm))), by = c(dateGp)]
    } else {
      if (normBy == "var") {
        countBy <- dataSub[, list(N = sum(get(weightNm))), by = c(myVar)]
      }
    }
  }
  
  
  crossLevels <- CJ(unique(countData[[dateGp]]), unique(countData[[myVar]]))
  setnames(crossLevels, c("V1", "V2"), c(dateGp, myVar))
  countData <- merge(crossLevels, countData, all.x = TRUE, by = c(dateGp, myVar))
  countData[is.na(N), N := 0]
  countData[, (myVar) := factor(get(myVar), levels = newLevels)]
  
  if (normBy == "time"){
    rateBy <- merge(countData, countBy, by = dateGp)
  } else {
    if (normBy == "var") {
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
    ggplot2::scale_x_date() +
    ggplot2::theme(axis.text.x=element_text(angle = 30, hjust = 1))
  
  return(p)
  
}
