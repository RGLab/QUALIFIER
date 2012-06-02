


.db <- new.env()

createDbSchema <- function(db)
{
	#qaTask table
	db$qaTaskList<-data.frame(qaID=integer()
								,qaName=character()
								,desciption=character()
								,qaLevel=character()
								,pop=character()
								,formula=character()
								,plotType=character()
							)
	#outlier table
	db$GroupOutlierResult<-db$outlierResult<-data.frame(sid=integer(),qaID=integer(),stringsAsFactors=F)
	#stats table
	db$stats<-data.frame(sid=integer() #statesID:unique for each stat entry
							,id=integer()#fileID:unique for each FCS
							,gsid=integer()#gatignSetID:unique fore each gatingSet
							,population=character()
							,stats=character()
							,node=character()
							,channel=character()
							,value=numeric()
					)
	#gating set table
	db$gstbl<-data.frame(gsid=integer()
						,gsname=character()
						)
	db$gs<-list()
}



.onLoad <- function(libname, pkgname) 
{
	createDbSchema(.db)
}





