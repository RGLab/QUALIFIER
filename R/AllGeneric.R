setGeneric("gateBoundary", function(x,y,...) standardGeneric("gateBoundary"))

#setGeneric("plotGate", function(x,y, ...) standardGeneric("plotGate"))

#setGeneric("checkRedundantStain", function(x,y, ...) standardGeneric("checkRedundantStain"))

#setGeneric("mergePanel", function(x,y,...) standardGeneric("mergePanel"))

#setGeneric("getAliquots", function(x,y, ...) standardGeneric("getAliquots"))

#setGeneric("getPanelTable", function(x, ...) standardGeneric("getPanelTable"))

#setGeneric("splitByRedundantStain", function(x,y,z, ...) standardGeneric("splitByRedundantStain"))

#setGeneric("getMFI", function(x,y, ...) standardGeneric("getMFI"))

#setGeneric("saveQAResult", function(x,y,z,...) standardGeneric("saveQAResult"))
###########################################

setGeneric("qaID", function(x) standardGeneric("qaID"))
setGeneric("getName", function(x) standardGeneric("getName"))
setGeneric("qaLevel", function(x) standardGeneric("qaLevel"))
setGeneric("getPop", function(x) standardGeneric("getPop"))
setGeneric("plotType", function(x) standardGeneric("plotType"))
setGeneric("qaCheck", function(obj,...) standardGeneric("qaCheck"))
#setGeneric("getData", function(obj) standardGeneric("getData")) #already defined in flowWorkspace
#setGeneric("description",function (object, ...)standardGeneric("description"))#already defined in Biobase
setGeneric("formula",function (x, ...)standardGeneric("formula"))

setGeneric("getQAStats",function (obj, ...)standardGeneric("getQAStats"))
