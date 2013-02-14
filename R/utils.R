# 
# These function are not generic and  designed as the convenience wrapper 
#for the special use case of ITN
# 
###############################################################################


.parseTubeID<-function(db=.db){
	##extract tubeID from filename by stripping the first two prefix (presummably date and fileid on each tube)
	pData(db$gs[[1]])$tubeID<-unlist(lapply(pData(db$gs[[1]])$name,function(x){
#			
						strsplit(
								paste(strsplit(as.character(x),"_")[[1]][c(-1,-2)],collapse="_")
								,"\\.")[[1]][[1]]
					}))
}

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