###########################################
#      Discrete Plot Method               #
###########################################
#' Creates over time variable plots for a categorical variable
#' 
#' Output plots include a bar plot with cateogries ordered by counts,
#' and trace plots of categories' proportions over time. This function is also
#' appliable to a binary varible, which is treated as categorical in this 
#' package.
#' 
#'
#' @inheritParams PrepData
#' @param myVar Name of the variable to be plotted
#' @param kCategories If a categorical variable has more than \code{kCategories},
#'   trace plots of only the \code{kCategories} most prevalent categories will
#'   be plotted.  
#' @param normBy The normalization factor for rate plots, can be \code{"time"}
#'   or \code{"var"}. If \code{"time"}, then for each time period of 
#'   \code{dateGp}, counts are normalized by the total counts over all 
#'   categories in that time period. This illustrates changes of categories' 
#'   proportions over time. If \code{"var"}, then for each category, its counts 
#'   are normalized by the total counts over time from only this category. This
#'   illustrates changes of categories' volumns over time.
#' @export
#' @return a \code{ggplot} object, including a histogram, and trace plots of 
#'   categories' proportions if number of categories is less than 
#'   \code{kCategories}. 
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
#' bankData = PrepData(bankData, dateNm = "date", dateGp = "months", 
#'                     dateGpBp = "quarters", weightNm = NULL)
#' # Single histogram is plotted for job type since there are 12 categories
#' plot(PlotCategoricalVar(myVar = "job", dataFl = bankData, weightNm =  NULL, 
#'                      dateNm = "date", dateGp = "months"))
#'                      
#' plot(PlotCategoricalVar(myVar = "job", dataFl = bankData, weightNm = NULL, 
#'                      dateNm = "date", dateGp = "months", kCategories = 12))
#'
#'
#' ## Binary data is treated as categorical, 
#' ## and only the less frequent category is plotted over time.
#' plot(PlotCategoricalVar(myVar = "default", dataFl = bankData, weightNm = NULL, 
#'                      dateNm = "date", dateGp = "months"))

PlotCategoricalVar <- function(myVar, dataFl, weightNm = NULL, dateNm, dateGp,
                            kCategories = 9, normBy = "time") { #!# previous name: PlotDiscreteVar
  count <- NULL
  
  p <- PlotBarplot(dataFl = dataFl, myVar = myVar, weightNm = weightNm)
  newLevels <- as.character(p$data[order(-count)][[myVar]])
  
  p2 <- PlotRatesOverTime(dataFl = dataFl, dateGp = dateGp, weightNm = weightNm,
                          myVar = myVar, newLevels = newLevels, normBy = normBy,
                          kCategories = kCategories)
  
  p  <- gridExtra::arrangeGrob(ggplotGrob(p), p2, widths = c(1, 2))
  
  return(p)
}

###########################################
#       Discrete Plotting Functions       #
###########################################
#' Creates a bar plot for a discrete (or binary) variable
#'
#' @inheritParams PrepData
#' @inheritParams PlotCategoricalVar
#' @export
#' @return A \code{ggplot} object with a histogram of \code{myVar} ordered by 
#'   category frequency
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
#' bankData = PrepData(bankData, dateNm = "date", dateGp = "months", 
#'                     dateGpBp = "quarters", weightNm = NULL)
#' PlotBarplot(bankData, "job")
#' 
#' ## NA will be included as a category if any NA are present
#' bankData[sample.int(.N)[1:1000], education := NA]
#' PlotBarplot(bankData, "education")

PlotBarplot <- function(dataFl, myVar, weightNm = NULL){ #!# previous name: PlotHistogram

  count <- NULL
  
  ## A subset dataset to work on
  dataSub <- dataFl[, c(myVar, weightNm), with = FALSE]
  ## NA is converted to a character, i.e., treated as a new category
  dataSub[is.na(get(myVar)), (myVar) := "NA"]
  
  ## Create glbTotals, a frequency table of myVar 
  if (is.null(weightNm)) {
    glbTotals <- dataSub[, list(count = .N), by = myVar]
  } else {
    glbTotals <- dataSub[, list(count = sum(get(weightNm))), by = myVar]
  }
  
  ## Create newLevels, a vector of category names, in descending order of counts
  newLevels <- unlist(glbTotals[order(-count), myVar, with = FALSE])
  glbTotals[, (myVar) := factor(get(myVar), levels = newLevels)]
  
  p <- ggplot2::ggplot(glbTotals, ggplot2::aes_string(x = myVar,
                                                      y = "count",
                                                      group = myVar)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_discrete(labels = abbreviate, breaks = newLevels) +
    ggplot2::theme(text = ggplot2::element_text(size = 10))
  return(p)
}


#' Creates trace plots of categories' propotions over time for a discrete (or
#' binary) variable
#'
#' @inheritParams PlotCategoricalVar
#' @inheritParams PrepData
#' @param newLevels categories of \code{myVar} in order of global frequency
#' @export
#' @return A \code{ggplot} object, trace plots of categories' propotions 
#'   \code{myVar} over time.
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
#' bankData = PrepData(bankData, dateNm = "date", dateGp = "months", 
#'                     dateGpBp = "quarters", weightNm = "weight")
#' PlotRatesOverTime(dataFl = bankData, dateGp = "months", weightNm = "weight",
#'                   myVar = "job", newLevels = NULL, normBy = "time")
#' PlotRatesOverTime(dataFl = bankData, dateGp = "months",  weightNm = "weight",
#'                   myVar = "job", newLevels = NULL, normBy = "var")
#' 
PlotRatesOverTime <- function(dataFl, dateGp, myVar, normBy = "time",
                             weightNm = NULL, newLevels = NULL, kCategories = 9){ #!# previous name: PlotHistOverTime
  N.x <- NULL
  N.y <- NULL
  rate <- NULL
  N <- NULL
  count <- NULL
  
  ## A subset dataset to work on
  dataSub <- dataFl[, c(dateGp, myVar, weightNm), with = FALSE]
  ## NA is converted to a character, i.e., treated as a new category
  dataSub[is.na(get(myVar)), (myVar) := "NA"]
  
  ## Create glbTotals, a frequency table of myVar 
  ## Create newLevels, a vector of category names, in descending order of counts
  if (is.null(newLevels)){
    if (is.null(weightNm)) {
      glbTotals <- dataSub[, list(count = .N), by = myVar]
    } else {
      glbTotals <- dataSub[, list(count = sum(get(weightNm))), by = myVar]
    }
    
    newLevels <- glbTotals[order(-count), myVar, with = FALSE][[myVar]]
  }
  
  ## Compute counts by category and time
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
  
  ## Make sure countData contains all cateogires and all times
  crossLevels <- CJ(unique(countData[[dateGp]]), unique(countData[[myVar]]))
  setnames(crossLevels, c("V1", "V2"), c(dateGp, myVar))
  countData <- merge(crossLevels, countData, all.x = TRUE, by = c(dateGp, myVar))
  countData[is.na(N), N := 0]
  countData[, (myVar) := factor(get(myVar), levels = newLevels)]
  
  ## Combine countData (numerator) and countBy (denominator) as rateBy
  if (normBy == "time"){
    rateBy <- merge(countData, countBy, by = dateGp)
  } else {
    if (normBy == "var") {
      rateBy  <- merge(countData, countBy, by = myVar)
    }
  }
  
  ## Compute the rates
  rateBy[, rate := N.x / N.y]
  rateBy[, (myVar) := factor(get(myVar), levels = newLevels)]
  
  ## Plot less frequent category only for a binary variable.
  ## This helps when there is a large class imbalance, because the range of y-axis for all trace plots is the same.
  if (length(newLevels) == 2) {
    rateBy <- rateBy[get(myVar) == newLevels[2]]
  }
  
  if(length(newLevels) <= kCategories){
    p <- ggplot2::ggplot(rateBy,
                         ggplot2::aes_string(x = dateGp, y = "rate"))   
  } else {
    p <- ggplot2::ggplot(rateBy[get(myVar) %in% newLevels[1:kCategories]],
                         ggplot2::aes_string(x = dateGp, y = "rate"))
  }
  
  p <- p +
    ggplot2::geom_line(stat = "identity")  +
    ggplot2::facet_wrap(stats::as.formula(paste("~", myVar))) +
    ggplot2::ylab("") +
    ggplot2::scale_x_date() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle = 30, hjust = 1)) +
    ggplot2::scale_y_continuous(labels=scales::percent)
  
  return(p)
  
}
