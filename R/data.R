#' The example QA data included in the package
#'
#' Once 'ITNQASTUDY' is loaded by \code{data} function, an \code{environment} 'db' is generated, which contains:
#' \itemize{
#'  \item gs a \code{list} of \code{GatingSet} objects
#'  \item gstbl a \code{data.frame} that records the 'gsid' and 'gsname' of each object within \code{gs}
#'  \item stats a\code{data.table} that stores the statistics pre-caluclated from \code{gs}
#'  \item outlierResult a \code{data.frame} stores the outlier detection results
#'  \item GroupOutlierResult a \code{data.frame} stores the group outlier detection results
#' }
#' 
#' \code{qaCheckList} is a example \code{spreadsheet} of the definitions of qaTasks
#' 
#' \code{tubesevents} is an example \code{spreadsheet} of thresholds for total number of events based on each 'tube'
#'   
#' @name ITNQASTUDY
#' @aliases db qaCheckList tubesevents
#' @docType data
#' @keywords data
NULL