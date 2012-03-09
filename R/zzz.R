


.db <- new.env()

createDbSchema <- function()
{
	#qaTask table
	.db$qaTaskList<-data.frame(qaID=integer()
								,qaName=character()
								,desciption=character()
								,qaLevel=character()
								,pop=character()
								,formula=character()
								,plotType=character()
							)
	#outlier table
	.db$GroupOutlierResult<-db$outlierResult<-data.frame(sid=integer(),qaID=integer(),stringsAsFactors=F)
	#stats table
	.db$statsofGS<-data.frame(sid=integer()
							,id=integer()
							,population=character()
							,stats=character()
							,node=character()
							,channel=character()
							,value=numeric()
					)
	
}



.onLoad <- function(libname, pkgname) 
{
	createDbSchema()
}





