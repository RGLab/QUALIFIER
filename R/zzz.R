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
qa.par.get <- function (name, value){
  get(name, .qa.options)
}
#' @importFrom latticeExtra ggplot2like axis.grid
.onLoad <- function(libname, pkgname) 
{
	
	 .setupPlotTheme(ggplot2like())
     qa.par.set("idCol","fileid")
     createDbSchema(.db)
}






