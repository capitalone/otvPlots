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
#' @seealso \code{\link{PrepData}}
#' @seealso \code{\link{PrepLabels}}
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
#         Prepare Data                    #
###########################################

#' Clean an input dataset for plotting
#'
#' Cleans an input dataset for use by otvPlots. \code{dataFl} must contain, at a
#' minimum, a date column \code{dateNm} and a variable to be plotted.
#'
#' @param dataFl Either the name of an object that can be converted using
#' as.data.table (e.g. a data frame), or a character string containing the name
#' of dataset that can be loaded using fread (e.g. a csv file), or a file path 
#' of Rdata file. If dataset is not in your working directory then \code{dataFl}
#' must include (relative or absolute) path to file
#' @param selectCols Either NULL, or a vector of names or indices of
#' variables to read into memory -- must include \code{dateNm},
#' \code{weightNm} (if not null) and all variables to be plotted. If both
#' selectCols and dropCols are null, then all variables will be read in.
#' @param dropCols Either NULL, or a vector of variables names or indices of
#' variables not to read into memory.
#' @param dateNm Name of column containing \code{date} variable
#' @param dateFt strptime format of date variable. Default is SAS format ("\%d\%h\%Y"). But 
#' input data with R date format (\%Y-\%m-\%d) will also be detected. Both of two formats 
#' can be parsed automaticallly. See ?strptime
#' @param dateGp Name of the variable the time series plots should be grouped
#' by. Options are NULL, "weeks", "months", "quarters", "years". See
#' data.table::IDate. If NULL \code{dateNm} will be used.
#' @param dateGpBp Name of variable the boxplots should be grouped by. Same
#' options as \code{dateGp}. If NULL \code{dateGp} will be used.
#' @param weightNm Name of variable containing row weights or NULL for no
#' weights (all rows recieve weight 1)
#' @param varNms Either NULL or a vector of names or indices of variables to 
#' be plotted. If null, will default to all columns which are not \code{dateNm}
#' or \code{weightNm}. Can also be a vector of indices of the column names,
#' after dropCols or selectCols have been applied, if applicable, and not 
#' including dateGp, dateGpBp (which will be added to the data.table)
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
  
  ## Remove all '/', '-', '"', and spaces in the string inputs: dateNm, weightNm
  dateNm <- gsub("/|\\-|\"|\\s", "", dateNm)
  if (!is.null(weightNm)) {
    weightNm <- gsub("/|\\-|\"|\\s", "", weightNm)
  }
  
  ## Ensure valid inputs: selectCols and dropCols
  if (!is.null(selectCols) & !is.character(selectCols) & !is.numeric(selectCols)) {
    stop("selectCols can only be a vector of names/indices of variables")
  }
  if (!is.null(dropCols) & !is.character(dropCols) & !is.numeric(dropCols)) {
    stop("dropCols can only be a vector of names/indices of variables")
  }
  
  
  ## Read the dataFl as a data.table. 
  csvfile = FALSE
  if (is.character(dataFl)) {
    fileExt <- tolower(tools::file_ext(dataFl))
    if (fileExt %in% c("csv")) { ## for csv input file
      if (!is.null(dropCols)) {
        dataFl <- fread(dataFl, drop = dropCols,stringsAsFactors = FALSE,
                        integer64 = "double", ...)
      } else if (!is.null(selectCols)) {
        dataFl <- fread(dataFl, select = selectCols, stringsAsFactors = FALSE,
                        integer64 = "double", ...)
      } else {
        dataFl <- fread(dataFl, stringsAsFactors = FALSE,
                        integer64 = "double",...)
      }
      csvfile = TRUE
    } else if (fileExt %in% c("rdata", "rda")) { ## for rda input file
      dataFl <- readRDS(dataFl) ## This function seems to only work for rds files, not rda!
      setDT(dataFl)
    } else {
      stop("Please make sure the input file is a csv file or Rdata file.")
    }
  } else {
    if (!is.data.table(dataFl)) {
      setDT(dataFl)
    }
  }
  
  ## Clean up all special characters from column names
  setnames(dataFl, gsub("/|\\-|\"|\\s", "", names(dataFl)))
  if (!is.null(selectCols) & is.character(selectCols)) {
    selectCols <- gsub("/|\\-|\"|\\s", "", selectCols)
  }
  if (!is.null(dropCols) & is.character(dropCols)) {
    dropCols <- gsub("/|\\-|\"|\\s", "", dropCols)
  }
  ## Apply selectCols and dropCols only apply to non-csv input dataFl.
  if (!csvfile) {
    if (!is.null(selectCols)) {
      if (is.character(selectCols)) {
        selectCols <- selectCols[selectCols %in% names(dataFl)]
      }
      dataFl <- dataFl[, selectCols, with=FALSE]
    } else if (!is.null(dropCols)) {
      if (is.character(dropCols)) {
        dataFl <- dataFl[, !(names(dataFl) %in% dropCols), with=FALSE]
      } else {
        dataFl <- dataFl[, -dropCols, with=FALSE]
      }
    }
  }
  
  ## Check that these columns are in dataFl: dateNm, weightNm
  stopifnot(c(dateNm, weightNm) %in% names(dataFl))
  
  ## Ensure all integer64 types are treated as numeric
  for (var in names(dataFl)) {
    if (inherits(dataFl[[var]], "integer64")) {
      dataFl[, (var) := as.numeric(get(var))]
    }
  }
  
  ## Check if PrepData is already run on this dataset
  if (any(c(sapply(dataFl, class)) %in% c("cntns", "dscrt"))) {
    message ("PrepData has already been run on this data set.")
  }
  
  ##  Normalize weights
  if (!is.null(weightNm)) {
    stopifnot(is.numeric(dataFl[[weightNm]]))
    ## If current variable set to be weight/date has previously been given
    ## a plotting type, remove it now
    if (length(intersect(c("cntns", "dcsrt"), attr(dataFl[[weightNm]], "class"))) > 0) {
      attr(dataFl[[weightNm]], "class") <- attr(dataFl[[weightNm]], "class")[[1]]
    }
    if (dataFl[is.na(weightNm), .N ] > 0 ) {
      warning ("Missings in weight column. Imputing to zero.")
      dataFl[is.na(get(weightNm)), (weightNm) := 0]
    }
    # Normalize weights for consistent treatment
    dataFl[, c(weightNm) := get(weightNm) / sum(get(weightNm))]
  }
  # Convert date to IDate according to provided format and give warning
  # if format produces NAs
  tmp.N <-
    dataFl[is.na(get(dateNm)), .N] + dataFl[as.character(get(dateNm)) == "", .N]
  nonNADateIndex <- which(!is.na(dataFl[, dateNm, with = FALSE]))
  firstNonNA <- min(nonNADateIndex)
  ## Recognize data format (overwrite user input dataFt!). 
  ## Question: handle user input dataFt? 
  nonNADate <- dataFl[firstNonNA, dateNm, with = FALSE]
  if (grepl("([0-9]{4}-[0-9]{2}-[0-9]{2})", nonNADate[[1]])) {
    dateFt <- "%Y-%m-%d"
  }
  dataFl[, c(dateNm) := as.IDate(get(dateNm), format = dateFt)]
  if (dataFl[is.na(get(dateNm)), .N] > tmp.N) {
    warning (paste0("Formatting ", dateNm, " as \"", dateFt, "\" produces NAs"))
  }
  
  ## Round the dataNm by dateGp, and add a column dataGp
  if (!is.null(dateGp) &&
      dateGp %in% c("weeks", "months", "quarters", "years")) {
    ## Dates are rounded to level specified and results are saved for faster
    ## aggregation. dateGp will be the primary DT key
    dataFl[, c(dateGp) := round(get(dateNm), dateGp)]
    setkeyv(dataFl, dateGp)
  } else {
    message (paste0("Time series plots will be grouped by ", dateNm, ".
                    If this was not your intention, make sure that dateGp is one of the
                    IDate rounding units in c('weeks', 'months', 'quarters', 'years')"))
    setkeyv(dataFl, dateNm)
  }
  
  ## Round the dataNm by dataGpBp, and add a column dataGpBp
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
  
  ## Create a vector of column names called vars (from varNms if not NULL)
  ## Columns in var are to be plotted
  if (is.null(varNms)) {
    vars <- names(dataFl)
  } else {
    if (is.numeric(varNms)) {
      vars <- names(dataFl)[varNms]
    } else {
      vars <- varNms
    }
  }
  ## Remove dateNm and weightNm if in vars
  vars <- vars[!vars %in% c(dateNm, weightNm)]
  
  if (length(intersect(c("cntns", "dcsrt"), attr(dataFl[[dateNm]], "class"))) > 0) {
    attr(dataFl[[dateNm]], "class") <- attr(dataFl[[dateNm]], "class")[[1]]
  }
  
  ## Check for any (non-NA) constant columns in vars
  bad_ind <- vapply(dataFl[, c(vars), with = FALSE],
                    function(x) all(length(unique(x)) == 1,
                                    sum(is.na(x)) == 0), logical(1))
  
  if (any(c(dateGp, dateGpBp) %in% vars[bad_ind])) {
    warning ("No variability in grouping variables. Select a new grouping level.")
  }
  if (length(vars[!bad_ind]) == 0) {
    stop("All the variables selected have no variability")
  }
  
  ## Drop constant columns
  if (dropConstants){
    if (sum(bad_ind) > 0) {
      warning (paste(
        c("The following variables have no variability and will be dropped: ",
          paste(vars[bad_ind], collapse = ", ")), collapse = ""))
      badVars <- vars[bad_ind]
      vars <- vars[!bad_ind]
      dataFl[, c(badVars) := NULL]
    }
  }
  
  ## Logical indicator vector: columns of type Date
  date_ind <- sapply(dataFl[, c(vars), with = FALSE], inherits, "Date")
  
  ## Remove all columns of type Date in var, becuase we do not want to plot any dates
  vars <- vars[!date_ind]
  # Logical indicator vector: numeric columns
  num_ind  <- sapply(dataFl[, c(vars), with = FALSE], function(z) is.numeric(z))
  # Logical indicator vector: categorical (i.e., nominal) columns
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
#' @param labelFl Either the path of a dataset (csv or Rdata file) containing
#' labels, an R object convertible to data.table (e.g. data frame) or NULL. If
#' NULL no labels will be used. Label dataset must contain at least 2 columns:
#' varCol (variable names) and labelCol (variable label).
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
      fileExt <- tolower(tools::file_ext(labelFl))
      if (fileExt %in% c("csv")) {
        labelFl <- fread(labelFl, select = idx, stringsAsFactors = FALSE)
      } else if (fileExt %in% c("rdata", "rda")) {
        labelFl <- readRDS(labelFl)
        setDT(labelFl)
      } else {
        stop("Please make sure the input file is a csv file or Rdata file.")
      }
    } else {
      setDT(labelFl)
      vec <- 1:ncol(labelFl)
      if (tail(vec, 1) > 2) {
        labelFl[, vec[!vec %in% idx] := NULL]
      }
    }
    setnames(labelFl, c("varCol", "labelCol"))
    labelFl <- subset(labelFl, subset = (!varCol == ""))
    labelFl[, varCol := gsub("/|\\-|\"|\\s", "", varCol)]
    labelFl[, labelCol := stringi::stri_trans_general(labelCol, "latin-ascii")]
    labelFl[, labelCol := strtrim(labelCol, 145)]
    labelFl[nchar(labelCol) > 144, labelCol := paste0(labelCol, "...")]
  }
  return(labelFl)
}


