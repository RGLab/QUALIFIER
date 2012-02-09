# TODO: Add comment
# 
# Author: wjiang2
###############################################################################


setMethod("gateBoundary", signature=c(x="rectangleGate",y="logicalFilterResult"),
		function(x,y,...){
#			browser()
			mat<-as.matrix(c(x@min,x@max))
			colnames(mat)<-parameters(x)
			list(mat)
		})

setMethod("gateBoundary", signature(x="curv1Filter",y="multipleFilterResult"),
		function(x,y,...){
			
			
			lapply(filterDetails(y)[[1]]$boundaries,function(p){
						b<-as.matrix(p,col=1)
						colnames(b)<-parameters(x)
						b
					})
			
		})



setMethod("gateBoundary", signature(x="polygonGate",y="logicalFilterResult"),
		function(x,y,...){
#			browser()
			list(x@boundaries)
			
		})


setMethod("gateBoundary", signature(x="curv2Filter",y="multipleFilterResult"),
		function(x,y,...){
			
			
			lapply(filterDetails(y)[[1]]$polygons,function(p){
						b<-as.matrix(as.data.frame(list(x=p$x,y=p$y)))
						colnames(b)<-parameters(x)
						b
					})
			
		})
