# TODO: Add comment
# These function are not generic and  designed as the convenience wrapper for the special use case
# Author: wjiang2
###############################################################################



.TubeNameMapping<-function(db=.db,tubesEvents,gsid)
{
	if(missing(gsid))
		gsid<-max(db$gstbl$gsid)
	tt<-unique(pData(db$gs[[gsid]])[,c("Tube","tubeID")])
#	browser()
	rownames(tt)<-NULL
	tt$events<-tubesEvents[tt$tubeID,]
	rownames(tt)<-tt$Tube
	tt[!is.na(tt$events),"events",drop=FALSE]
	

}