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


###########################################
#           Utility Functions             #
###########################################

is.nmrcl <- function(x)  inherits(x, "nmrcl")
is.ctgrl <- function(x)  inherits(x, "ctgrl")

wtd_quantile_NA <- function(x, weights, probs = c(.0, .25, .5, .75, 1),
                            ...) { #!# previous name: wtd.quantile_NA
  tryCatch(as.double(Hmisc::wtd.quantile(x, weights, probs,
                                         normwt = TRUE, na.rm = TRUE, ...)),
           error = function(e) rep(NA_real_, length(probs)))
}

## The color-blind friendly color palette
## Source: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cbbPalette <- c("#D55E00", "#009E73", "#0072B2", "#000000", "#E69F00", "#56B4E9",  "#F0E442",  "#CC79A7")

# # An example function for fuzzy label matching
# # To be used an input of the \code{\link{PlotVar}} function.
# # If variables look like VAR_nameofvar, and the attribute dictionary contains
# # defintions only for nameofvar, then a fuzzy matching function can be
# # provided which would first attempt to match exactly, and then to attempt to
# # match on the longest piece after splitting on the underscore:
# 
# Fuzzy = function(LabelFl, myVar){
#    ll = labelFl[varCol == myVar, labelCol] # exact match
#    if (ll == ""){
#        # split on "_", search for exact match of longest piece
#        shortNm = names(which.max(sapply(strsplit(myVar, "_")[[1]], nchar)))
#        ll = labelFl[varCol == shortNm, labelCol]
#    }
#    return(ll)
#  }
