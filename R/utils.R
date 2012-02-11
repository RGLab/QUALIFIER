# TODO: Add comment
# These function are not generic and  designed as the convenience wrapper for the special use case
# Author: wjiang2
###############################################################################



.TubeNameMapping<-function(db,tubesEvents)
{
	tt<-unique(pData(db$G)[,c("Tube","tubeID")])
	rownames(tt)<-NULL
	tt$events<-tubesEvents[tt$tubeID,]
	rownames(tt)<-tt$Tube
	tt[,"events",drop=FALSE]

}