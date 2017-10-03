# SPDX-Copyright: Copyright (c) Capital One Services, LLC 
# SPDX-License-Identifier: Apache-2.0 
# Copyright 2017 Capital One Services, LLC 
#
# Licensed under the Apache License, Version 2.0 (the "License"); 
# you may not use this file except in compliance with the License. 
#
# You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0 
#
# Unless required by applicable law or agreed to in writing, software distributed 
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
# OF ANY KIND, either express or implied. 
# 
# See the License for the specific language governing permissions and limitations under the License. 


#' Direct marketing campaigns of a Portuguese banking institution
#'
#' The marketing campaigns were based on phone calls.
#' Often, more than one contact to the same client was required, in order to 
#' access if the product (bank term deposit) would be ('yes') or not ('no') 
#' subscribed. Records are ordered by date (from May 2008 to November 2010), 
#' similar to data analyzed in Moro et al. [2014].
#'
#'
#' @format A data frame with 45,211 rows and 19 variables:
#' \describe{
#'   \item{age}{Age of the client, numeric.}
#'   \item{job}{Type of job, a categorical variable with the levels: 
#'     \code{'admin.'}, \code{'blue-collar'}, \code{'entrepreneur'},
#'     \code{'housemaid'}, \code{'management'}, \code{'retired'},
#'     \code{'self-employed'}, \code{'services'}, \code{'student'},
#'     \code{'technician'}, \code{'unemployed'}, and \code{'unknown'}.}
#'   \item{marital}{Marital status, a categorical variable with levels: 
#'     \code{'divorced'}, \code{'married'}, \code{'single'}, and \code{'unknown'}.
#'     Note that \code{'divorced'} means either divorced or widowed.}
#'   \item{education}{A categorical variable with levels: \code{'basic.4y'},
#'     \code{'basic.6y'}, \code{'basic.9y'}, \code{'high.school'},
#'     \code{'illiterate'}, \code{'professional.course'}, 
#'     \code{'university.degree'}, and \code{'unknown'}.}
#'   \item{default}{Whether credit is in default, a categorical variable with 
#'     levels: \code{'no'}, \code{'yes'}, and \code{'unknown'}.}
#'   \item{balance}{Account balance, numeric.}
#'   \item{housing}{Whether the client has a housing loan, a categorical variable
#'     with levels: \code{'no'}, \code{'yes'}, and \code{'unknown'}.}
#'   \item{loan}{Whether the client has personal loan, a categorical variable
#'     with levels: \code{'no'}, \code{'yes'}, and \code{'unknown'}.}
#'   \item{contact}{Type of contact communication, a categorical variable
#'     with levels: \code{'cellular'} and \code{'telephone'}.}
#'   \item{duration}{Last contact duration in seconds, a numeric variable.}
#'   \item{campaign}{Number of contacts performed during this campaign for 
#'     this client, including the last contact; a numeric variable.}
#'   \item{pdays}{Number of days that passed by after the client was last 
#'     contacted from a previous campaign; a numeric variable, with \code{999} 
#'     means that client was not previously contacted.}
#'   \item{previous}{Number of contacts performed before this campaign for this
#'     client, a numeric variable.}
#'   \item{poutcome}{Outcome of the previous marketing campaign, a categorical
#'     variable with levels: \code{'failure'}, \code{'nonexistent'},
#'     and \code{'success'}.}
#'   \item{y}{Whether the client has subscribed a term deposit, a categorical
#'     variable with levels: \code{'yes'} and \code{'no'}.}
#'   \item{date}{Last contact date.}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Bank+Marketing}
#' @source \cite{Lichman, M. (2013). UCI Machine Learning Repository 
#'   [\url{http://archive.ics.uci.edu/ml}]. Irvine, CA: University of California, 
#'   School of Information and Computer Science.}
#' @source \cite{S. Moro, P. Cortez, and P. Rita. (2014) A Data-Driven Approach
#'   to Predict the Success of Bank Telemarketing. Decision Support Systems, 
#'   Elsevier, 62:22-31, June 2014.}
"bankData"

#' Labels for bankData
#'
#' A dataset containing the attribute labels also found in \code{\link{bankData}}.
#' This data set is used to illustrate the \code{\link{PrepLabels}} function and
#' other label functionality in the \code{\link{otvPlots}} package in R.
#'
#' @format A data frame with 16 rows and 3 variables:
#' \describe{
#'   \item{V1}{Name of each variable in \code{\link{bankData}}.}
#'   \item{V2}{Label of each variable in \code{\link{bankData}}.}
#'   \item{V3}{A numeric variable, corresponding to the row number.}
#' }
"bankLabels"
