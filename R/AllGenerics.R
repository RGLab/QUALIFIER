#setGeneric("gateBoundary", function(x,y,...) standardGeneric("gateBoundary"))

###########################################

setGeneric("qaID", function(x) standardGeneric("qaID"))
setGeneric("getName", function(x) standardGeneric("getName"))
setGeneric("qaLevel", function(x) standardGeneric("qaLevel"))
setGeneric("getPop", function(x) standardGeneric("getPop"))
setGeneric("plotType", function(x) standardGeneric("plotType"))
setGeneric("qaCheck", function(obj,...) standardGeneric("qaCheck"))
setGeneric("qaReport", function(obj,...) standardGeneric("qaReport"))
setGeneric("getFormula",function (x, ...)standardGeneric("getFormula"))
#' @export
setGeneric("qpar",function (x, ...)standardGeneric("qpar"))
#' @export
setGeneric("qpar<-",function(object,value) standardGeneric("qpar<-"))
#' @export
setGeneric("scatterPar",function (x, ...)standardGeneric("scatterPar"))
#' @export
setGeneric("scatterPar<-",function(object,value) standardGeneric("scatterPar<-"))
setGeneric("width",function (x)standardGeneric("width"))
setGeneric("height",function (x)standardGeneric("height"))
#' @export
setGeneric("htmlReport",function (x)standardGeneric("htmlReport"))
#' @export
setGeneric("htmlReport<-",function (x,value)standardGeneric("htmlReport<-"))
#' @export
setGeneric("rFunc",function (x)standardGeneric("rFunc"))
#' @export
setGeneric("rFunc<-",function (x,value)standardGeneric("rFunc<-"))

setGeneric("highlight",function (x)standardGeneric("highlight"))
setGeneric("highlight<-",function (object,value)standardGeneric("highlight<-"))


setGeneric("getPath",function (x, ...)standardGeneric("getPath"))



