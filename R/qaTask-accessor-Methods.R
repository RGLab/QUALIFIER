# TODO: Add comment
# 
# Author: wjiang2
###############################################################################

setMethod("qaID", signature=c(x="qaTask"),
		function(x){
			x@qaID
		})


setMethod("getName", signature=c(x="qaTask"),
		function(x){
			x@qaName
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

setMethod("formula", signature=c(x="qaTask"),
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
			cat("Plot type: ", plotType(object))
			cat("\n")
			cat("Gated node: ", getPop(object))
			cat("\n")
			cat("Default formula :")
			print(formula(object))
			cat("\n")
			#checkParameters(object) 
		})