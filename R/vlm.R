###########################################
#          The Main Function              #
###########################################

#' Create over time variable plots and summary statistics for variable level monitoring
#' 
#' Sorts variables according to either user input or correlation with time 
#' (among numerical variables only), and create output files including:
#' \itemize{
#'  \item A PDF file of plots saved as \code{outFl}.pdf, with each indivual page 
#'  on one variable. Variables are plotted in the order indicated in the argument
#'  \code{sortVars} or \code{sortFn}. 
#'  For each numerical variable, the output plots include 
#'  \itemize{
#'    \item side-by-side boxplots grouped by \code{dateGpBp} (left), 
#'    \item a trace plot of p1, p50 and p99 percentiles, grouped by \code{dateGp}
#'      (top right), 
#'    \item a trace plot of mean and +-1 SD control limits, grouped by 
#'      \code{dateGp}(middle right), and 
#'    \item a trace plot of missing and zerorates, grouped by \code{dateGp} 
#'      (bottom right).
#'   }
#'   For each categorical variable (including a numerical variable with no more 
#'   than 2 unique levels not including NA), the output plots include 
#'   \itemize{
#'     \item a frequency bar plot (left), and 
#'     \item a grid of trace plots on categories' proportions over time (right). 
#'       If the variable contains more than \code{kCategories} number of 
#'       categories, trace plots of only the largest \code{kCategories} will be 
#'       plotted. If the variable contains only two categories, then only the 
#'       trace plot of the less prevalent cateogy will be plotted.
#'   }
#'   \item CSV file(s) on summary statistics of variable, both globally and over
#'   time aggregated by \code{dateGp}. The order of variables in the CSV files
#'   are the same as in the PDF file. 
#'   \itemize{
#'     \item For numerical varaibles, number of observations (counts), p1, p25, 
#'     p50, p75, and p99 qunatiles, mean, SD, missing and zerorates are saved
#'     as \code{outFl}_numerical_summary.csv.
#'     \item For categorical varaibles, number of observations (counts) and 
#'     categories' proportions are saved as \code{outFl}_categorical_summary.csv. 
#'     Each row is a category of a categorical (or binary) variable.
#'     The row whose \code{category == 'NA'} corresponds to missing. Categories
#'     among the same variable are ordered by global prevalence in a descending 
#'     order.
#'   }
#' }
#' 
#' If the arugment \code{dataNeedPrep} is set to \code{FALSE}, then 
#' \itemize{
#' \item \code{dataFl} must be a \code{data.table} containing variables 
#'   \code{weightNm}, \code{dateNm}, \code{dateGp}, and \code{dateGpBp}, and 
#'   names of these variables must be the same as the corresponding arguments
#'   of the \code{\link{vlm}} function.
#' \item the arguments \code{selectCols}, \code{dropCols}, \code{dateFt}, 
#'   \code{dropConstants} will be ignored by the \code{\link{vlm}} function.
#' \item When analyze a dataset for the first time, it is recommended to first
#'   run the \code{\link{PrepData}} function on it, and then apply the 
#'   \code{\link{vlm}} function with the arguement \code{dataNeedPrep = FALSE}.
#'   Please see the examples for details. 
#' }
#' 
#' @inheritParams PrepData
#' @inheritParams PrepLabels
#' @inheritParams OrderByR2
#' @inheritParams PrintPlots
#' @param sortVars Determines which variables to be plotted and their order. 
#'   Either a character vector of variable names to plot variables in the same
#'   order as in the \code{sortVars} argument), or \code{NULL} to keep the 
#'   original ordering, with numerical variables will being plotted before 
#'   categorical and binary ones. \code{sortVars} should be \code{NULL} when the
#'   \code{sortFn} argument is used.
#' @param sortFn A sorting function which returns \code{sortVars} as an output. 
#'   The function may take the following variables as input: \code{dataFl}, 
#'   \code{dateNm}, \code{buildTm}, \code{weightNm}, \code{kSample}. Currently, 
#'   the only build-in sorting function is \code{\link{OrderByR2}}, which sorts
#'   numerical variables in the order of strength of linear association with date,
#'   and adds categorical (and binary) variables sorted in alphabetical order
#'   after the numerical ones. 
#' @param dataNeedPrep Logical, indicates if data should be run through the 
#'   \code{\link{PrepData}} function. This should be set to \code{TRUE} unless 
#'   the \code{\link{PrepData}} function has been applied to the input data 
#'   \code{dataFl}. 
#' @export
#' 
#' @seealso This function depends on:
#'          \code{\link{PrintPlots}},
#'          \code{\link{OrderByR2}},
#'          \code{\link{PrepData}},
#'          \code{\link{PrepLabels}}.
#'          
#' @section License: Copyright 2016 Capital One Services, LLC Licensed under the
#' Apache License, Version 2.0 (the "License"); you may not use this file 
#' except in compliance with the License. You may obtain a copy of the License
#' at http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable
#' law or agreed to in writing, software distributed under the License is
#' distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#' KIND, either express or implied. See the License for the specific language
#' governing permissions and limitations under the License.
#' @examples
#' ## Load the data and its label
#' data(bankData)
#' data(bankLabels)
#' 
#' ## The PrepData function should only need to be run once on a dataset, 
#' ## after that vlm can be run with the argument dataNeedPrep = FALSE
#' bankData <- PrepData(bankData, dateNm = "date", dateGp = "months", 
#'                     dateGpBp = "quarters")
#' bankLabels <- PrepLabels(bankLabels)
#'
#'\dontrun{ 
#' vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
#'     sortFn = "OrderByR2", dateGp = "months", dateGpBp = "quarters", 
#'     outFl = "bank")
#'     
#' ## If csv files of summary statistics are not need, set genCSV = FALSE
#' vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, genCSV = FALSE,
#'     sortFn = "OrderByR2", dateGp = "months", dateGpBp = "quarters", 
#'     outFl = "bank")
#'     
#' ## If weights are provided, they will be used in all statistical calculations
#' bankData[, weight := rnorm(.N, 1, .1)]
#' vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels,
#'     dateGp = "months", dateGpBp = "quarters", weightNm = "weight", 
#'     outFl = "bank")
#'
#' ## Customize plotting order by passing a vector of variable names to 
#' ## sortVars, but the "date" column must be excluded from sortVars
#' sortVars <- sort(bankLabels[varCol!="date", varCol])
#' vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
#'     dateGp = "months", dateGpBp = "quarters", outFl = "bank", 
#'     sortVars = sortVars)
#'             
#' ## Create plots for a specific variable using the varNms parameter
#' vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
#'     dateGp = "months", dateGpBp = "quarters", outFl = "bank", 
#'     varNms = "age", sortVars = NULL)
#'}

vlm <- function(dataFl, dateNm, labelFl = NULL, outFl = "otvplots", 
                genCSV = TRUE, dataNeedPrep = FALSE, dateGp = NULL, 
                dateGpBp = NULL, weightNm = NULL, varNms = NULL, 
                sortVars = NULL, sortFn = NULL, selectCols = NULL, 
                dropCols = NULL, dateFt = "%d%h%Y", buildTm = NULL, 
                highlightNms = NULL, skewOpt = NULL, kSample = 50000, 
                fuzzyLabelFn = NULL, dropConstants = FALSE, kCategories = 9, ...) {
  
  ## Assert statements about inputs
  if (!is.null(sortVars) & !is.null(sortFn)) {
    stop ("Please choose between sortVars (predetermined order of plotting) and
          sortFn (function to determine plotting order)")}
  
  if (!is.null(sortVars) & !is.null(varNms) &&
      !all(varNms %in% sortVars)) {
    stop ("Please make certain that varNms is a subset of sortVars")
  }
  
  if (!is.null(selectCols) & !is.null(dropCols)) {
    stop("Please choose between selectCols or dropCols.")
  }

  ## Apply the PrepData function if not previously on dataFl
  if (dataNeedPrep) { 
    # Need to prepare data first
    dataFl <- PrepData(dataFl = dataFl, dateNm = dateNm,
                         selectCols = selectCols, dropCols = dropCols,
                         dateFt = dateFt, dateGp = dateGp, dateGpBp = dateGpBp,
                         weightNm = weightNm, varNms = varNms,
                         dropConstants = dropConstants, ...)
  } else {
    stopifnot(is.data.table(dataFl) &&
                all(c(weightNm, dateNm, dateGp, dateGpBp) %in% names(dataFl)))
    ## Change integer64 data type to numeric
    for (var in names(dataFl)) {
      if (inherits(dataFl[[var]], "integer64")) {
        dataFl[, (var) := as.numeric(get(var))]
      }
    }
  }

  ## Apply the PrepLabels function 
  labelFl <- PrepLabels(labelFl)

  ## Apply sortFn to generate sortVars
  if (!is.null(sortFn) && is.character(sortFn)) {
    sortVars <- do.call(sortFn, list(dataFl = dataFl, dateNm = dateNm,
                                     buildTm = buildTm, weightNm = weightNm,
                                     kSample = kSample))
  } else {
    if (is.null(sortVars)) {
      num_vars <- names(dataFl)[sapply(dataFl, inherits, "nmrcl")]
      cat_vars <- names(dataFl)[sapply(dataFl, inherits, "ctgrl")]
      sortVars <- c(num_vars, cat_vars)
    }
  }
  
  ## Create the plots
  if (!is.null(varNms)) {
    PrintPlots(outFl = outFl,
               dataFl = dataFl[, c(varNms, dateNm, dateGp, dateGpBp, weightNm),
                               with = FALSE],
               sortVars = sortVars[sortVars %in% varNms], dateNm = dateNm,
               dateGp = dateGp, dateGpBp = dateGpBp, weightNm = weightNm,
               labelFl = labelFl, genCSV = genCSV, highlightNms = highlightNms,
               skewOpt = skewOpt, kSample = kSample,
               fuzzyLabelFn = fuzzyLabelFn, kCategories = kCategories)
  } else {
    PrintPlots(outFl = outFl, dataFl = dataFl, sortVars = sortVars,
               dateNm = dateNm, dateGp = dateGp, dateGpBp = dateGpBp,
               weightNm = weightNm, labelFl = labelFl, genCSV = genCSV,
               highlightNms = highlightNms, skewOpt = skewOpt,
               kSample = kSample, fuzzyLabelFn = fuzzyLabelFn,
               kCategories = kCategories)
  }
}

