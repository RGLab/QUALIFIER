# TODO: Add comment
# 
# Author: wjiang2
###############################################################################

setMethod("qaID", signature=c(x="qaTask"),
		function(x){
			x@qaID
		})
setMethod("highlight", signature=c(x="qaTask"),
		function(x){
			x@highlight
		})

setReplaceMethod("highlight",
		signature=signature(object="qaTask",value="character"),
		definition=function(object, value)
		{
#			browser()
			object@highlight<-value
			return(object)
		})

setMethod("qpar", signature=c(x="qaTask"),
		function(x){
			x@par
		})

setReplaceMethod("qpar",
		signature=signature(object="qaTask",
				value="list"),
		definition=function(object, value)
		{
			object@par<-lattice:::updateList(object@par,value)
			return(object)
		})

setMethod("scatterPar", signature=c(x="qaTask"),
		function(x){
			x@scatterPar
		})

setReplaceMethod("scatterPar",
		signature=signature(object="qaTask",
				value="list"),
		definition=function(object, value)
		{
			object@scatterPar<-lattice:::updateList(object@scatterPar,value)
			return(object)
		})


setMethod("htmlReport", signature=c(x="qaTask"),
		function(x){
			x@htmlReport
		})

setReplaceMethod("htmlReport",
		signature=signature(x="qaTask",
				value="logical"),
		definition=function(x, value)
		{
			x@htmlReport<-value
			return(x)
		})

setMethod("rFunc", signature=c(x="qaTask"),
		function(x){
			x@rFunc
		})

setReplaceMethod("rFunc",
		signature=signature(x="qaTask",
				value="ANY"),
		definition=function(x, value)
		{
			x@rFunc<-value
			return(x)
		})

setMethod("getName", signature=c(x="qaTask"),
		function(x){
			x@qaName
		})

setMethod("width", signature=c(x="qaTask"),
		function(x){
			x@width
		})

setMethod("height", signature=c(x="qaTask"),
		function(x){
			x@height
		})

setMethod("description", signature=c(object="qaTask"),
		function(object){
			object@description
		})

setMethod("qaLevel", signature=c(x="qaTask"),
		function(x){
			x@qaLevel
		})

setMethod("getPop", signature=c(x="qaTask"),
		function(x){
			x@pop
		})

setMethod("getData", signature=c(obj="qaTask"),
		function(obj){
			obj@db
		})

setMethod("getFormula", signature=c(x="qaTask"),
		function(x){
			x@formula
		})

setMethod("plotType", signature=c(x="qaTask"),
		function(x){
			x@plotType
		})
#
#
setMethod("show",
		signature=signature(object="qaTask"),
		definition=function(object)
		{ 
			cat("qaTask:", getName(object),"\n")
			cat("Level :", qaLevel(object), "\n") 
			cat("Description :", description(object), "\n")

			cat("population: ", getPop(object))
			cat("\n")
			cat("Default formula :")
			print(getFormula(object))
			cat("Plot type: ", plotType(object))
			cat("\n")
#			browser()
			print(qpar(object))
			
			#checkParameters(object) 
		})