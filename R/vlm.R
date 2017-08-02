###########################################
#          The Main Function              #
###########################################

#' Create over time variable plots for variable level monitoring
#' 
#' Prepares input dataset and labels, sorts variables according to either user 
#' input or correlation with time (among numerical variables only), 
#' and outputs the plots to a pdf file, with each page about one variable.
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
#' @inheritParams PrepData
#' @inheritParams PrepLabels
#' @inheritParams OrderByR2
#' @inheritParams PrintPlots
#' @param sortVars Variables to be plotted. Either a character vector of variable
#'   names, then variables will be plotted in the same order as in the 
#'   \code{sortVars} argument, or \code{NULL} to keep original ordering, with  
#'   numerical variables will being plotted before categorical and binary ones. 
#'   \code{sortVars} should be \code{NULL} when the \code{sortFn} argument is 
#'   used.
#' @param sortFn A sorting function which returns \code{sortVars} as an output. 
#'   The function may take the following variables as input: \code{dataFl}, 
#'   \code{dateNm}, \code{buildTm}, \code{weightNm}, \code{kSample}. Currently, 
#'   the only build-in sorting function is \code{\link{OrderByR2}}, which sorts
#'   numerical variables in the order of strength of linear association with date. 
#' @param dataNeedPrep Logical, indicates if data should be run through the 
#'   \code{\link{PrepData}} function. This should be set to \code{TRUE} unless 
#'   the \code{\link{PrepData}} function has been applied to the input data 
#'   \code{dataFl}. 
#' @return A pdf file of VLM report saved as \code{outFl}.
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
#' ## Load the data and its label
#' data(bankData)
#' data(bankLabels)
#' 
#' ## The PrepData function should only need to be run once on a dataset, 
#' ## after that vlm can be run with the argument dataNeedPrep = FALSE
#' bankData = PrepData(bankData, dateNm = "date", dateGp = "months", 
#'                    dateGpBp = "quarters")
#' bankLabels = PrepLabels(bankLabels)
#'\dontrun{ 
#' vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
#'     sortFn = "OrderByR2", dateGp = "months", dateGpBp = "quarters", 
#'     outFl = "bank.pdf", dataNeedPrep = FALSE)
#'} 
#' # Different values of kSample can affect the appearance of boxplots (and 
#' # possibly the order of variable output if sortVars = 'R2' is used), but does 
#' # not affect the time series plots, which always use all of the data 
#'\dontrun{
#'
#'vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels,
#'             dateGp = "months", dateGpBp = "quarters", outFl = "bank.pdf", 
#'             dataNeedPrep = FALSE, kSample = 500)
#'}
#' 
#' #  If weights are provided they will be used in all statistical calculations
#'\dontrun{
#' bankData[, weight := rnorm(.N, 1, .1)]
#' vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels,
#'             dateGp = "months", dateGpBp = "quarters", weightNm = "weight", 
#'             outFl = "bank.pdf", dataNeedPrep = FALSE, kSample = NULL)
#'}
#' # vlm is designed for non-interactive use, and both dataFl and
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
#' vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
#'             dateGp = "months", dateGpBp="quarters", weightNm = NULL, 
#'             outFl = "bank.pdf", dataNeedPrep = TRUE, kSample = NULL)
#'}
#' # We can pass a vector of variable names to customize plotting order using
#' # sortVars, but we must exclude the "date" column from sortVars or the 
#' # function will stop with a message warning us it cannot plot dates
#'\dontrun{ 
#' sortVars = sort(bankLabels[varCol!="date", varCol])
#' vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
#'             dateGp = "months", dateGpBp = "quarters", weightNm = NULL, 
#'             outFl = "bank.pdf", dataNeedPrep = FALSE, kSample = NULL, 
#'             sortVars = sortVars, kCategories = 9)
#'} 
#' # We can test that the function is working with a specific variable using 
#' # the varNms parameter
#' vlm(dataFl = bankData, dateNm = "date", labelFl = bankLabels, 
#'             dateGp = "months", dateGpBp = "quarters", weightNm = NULL, 
#'             outFl = "bank.pdf", dataNeedPrep = TRUE, kSample = NULL, 
#'             varNms = "age", sortVars = NULL)
#' 
#' # See otvPlots::PlotVar for examples in interactive use, 
#' # including use of the fuzzyLabels parameter
#' 
vlm <- function(dataFl, dateNm, labelFl = NULL, outFl = "otvplots.pdf", 
                dataNeedPrep = TRUE, dateGp = NULL, dateGpBp = NULL, weightNm = NULL, 
                varNms = NULL, sortVars = NULL, sortFn = NULL, selectCols = NULL, 
                dropCols = NULL, dateFt = "%d%h%Y", buildTm = NULL, 
                highlightNms = NULL, skewOpt = NULL, kSample = 50000, 
                fuzzyLabelFn = NULL, dropConstants = TRUE, kCategories = 9, ...) {
  
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
    ## Need to prepare data first
    # dataFl <- PrepData(dataFl = dataFl, dateNm = dateNm,
    #                      selectCols = selectCols, dropCols = dropCols,
    #                      dateFt = dateFt, dateGp = dateGp, dateGpBp = dateGpBp,
    #                      weightNm = weightNm, varNms = varNms,
    #                      dropConstants = dropConstants, ...)
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

