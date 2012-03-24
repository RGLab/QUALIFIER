setGeneric("gateBoundary", function(x,y,...) standardGeneric("gateBoundary"))

###########################################

setGeneric("qaID", function(x) standardGeneric("qaID"))
setGeneric("getName", function(x) standardGeneric("getName"))
setGeneric("qaLevel", function(x) standardGeneric("qaLevel"))
setGeneric("getPop", function(x) standardGeneric("getPop"))
setGeneric("plotType", function(x) standardGeneric("plotType"))
setGeneric("qaCheck", function(obj,...) standardGeneric("qaCheck"))
setGeneric("qaReport", function(obj,...) standardGeneric("qaReport"))
setGeneric("getFormula",function (x, ...)standardGeneric("getFormula"))
setGeneric("qpar",function (x, ...)standardGeneric("qpar"))
setGeneric("qpar<-",function(object,value) standardGeneric("qpar<-"))
setGeneric("scatterPar",function (x, ...)standardGeneric("scatterPar"))
setGeneric("scatterPar<-",function(object,value) standardGeneric("scatterPar<-"))
setGeneric("width",function (x)standardGeneric("width"))
setGeneric("height",function (x)standardGeneric("height"))
setGeneric("htmlReport",function (x)standardGeneric("htmlReport"))
setGeneric("htmlReport<-",function (x,value)standardGeneric("htmlReport<-"))

setGeneric("getQAStats",function (obj, ...)standardGeneric("getQAStats"))
setGeneric("getPath",function (x, ...)standardGeneric("getPath"))
#setGeneric("addStats",function (x,definition, ...)standardGeneric("addStats"))
