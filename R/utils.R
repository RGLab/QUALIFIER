#require(grDevices)
.db <- new.env()
.qa.options <- new.env()
createDbSchema <- function(db)
{
  #qaTask table
  db$qaTaskTbl<-data.frame(qaID=integer()
      ,qaName=character()
      ,description=character()
      ,qaLevel=character()
      ,pop=character()
      ,formula=character()
      ,plotType=character()
      ,stringsAsFactors=F
  )
  #outlier table
  db$GroupOutlierResult<-db$outlierResult<-data.frame(sid=integer(),qaID=integer(),stringsAsFactors=F)
  #stats table
  db$stats<-data.frame(sid=integer() #statsID:unique for each stat entry
      ,id=integer()#fileID:unique for each FCS
      ,gsid=integer()#gatignSetID:unique fore each gatingSet
      ,population=character()
      ,stats=character()
      ,node=character()
      ,channel=character()
      ,stain=character()
      ,value=numeric()
      ,stringsAsFactors=F
  )
  db$stats <- reshape::rename(db$stats,c(id = qa.par.get("idCol")))                    
  #gating set table
  db$gstbl<-data.frame(gsid=integer()
      ,gsname=character()
      ,objlink=character()
      ,stringsAsFactors=F
  )
  db$gs<-list()
}

.setupPlotTheme <- function(theme = standard.theme()){
  
  .db$lattice<-list(par.settings=lattice:::updateList(theme
          ,list(box.dot=list(pch=22
                  ,cex=0.4
              )
              ,superpose.symbol = list(col = theme$superpose.symbol$col[c(5:6,1:4,7)]
              )#adjust the order to display dots in blue and outlier in red
          )
      )
      ,scales=list(x=list(rot=45))
      ,par.strip.text=list(lines=2)
  )
}

qa.par.set <- function (name, value){
  assign(name, value, .qa.options)
}
qa.par.get <- function (name){
  get(name, .qa.options)
}

#' @importFrom latticeExtra ggplot2like axis.grid
.setupPlotTheme(ggplot2like())

qa.par.set("idCol","fileid")

createDbSchema(.db)




############################################################################### 
# These function are not generic and  designed as the convenience wrapper 
#for the special use case of ITN
# 
###############################################################################


.parseTubeID<-function(db=.db){
	##extract tubeID from filename by stripping the first two prefix (presummably date and fileid on each tube)
	pData(db$gs[[1]])$tubeID<-unlist(lapply(pData(db$gs[[1]])$name,function(x){
		strsplit( paste(strsplit(as.character(x),"_")[[1]][c(-1,-2)],collapse="_"),"\\.")[[1]][[1]]
	}))
}

.TubeNameMapping<-function(db=.db,tubesEvents,gsid)
{
	tt<-unique(pData(db$gs[[gsid]])[,c("Tube","tubeID")])
#	browser()
	rownames(tt)<-NULL
	tt$events<-tubesEvents[tt$tubeID,]
	rownames(tt)<-tt$Tube
	tt[!is.na(tt$events),"events",drop=FALSE]
	

}
