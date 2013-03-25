


.db <- new.env()

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
	#gating set table
	db$gstbl<-data.frame(gsid=integer()
						,gsname=character()
						,objlink=character()
						,stringsAsFactors=F
						)
	db$gs<-list()
}



.onLoad <- function(libname, pkgname) 
{
	lib <- library
	lib(RColorBrewer)
	lib(lattice)
	lib(grDevices)

	createDbSchema(.db)
	myColorPal<-brewer.pal(7, "Set1")
	.db$lattice<-list(par.settings=lattice:::updateList(standard.theme()
														,list(strip.background=list(col=rev(gray(seq(0.3,0.8,length=5))))
#															,strip.text=list(lines=2)#not sure why this argument is not working ,we have to use par.strip.text outside of par.setting list
															,background=list(col="white")
															,plot.symbol=list(pch=19
																			,col=myColorPal[2])
															,superpose.symbol=list(pch=rep(19,7)
																					,col=myColorPal[c(2,1,3:7)])
															,box.dot=list(pch=22
																		,col=myColorPal[2]
																		,cex=0.4
																		)
															,box.rectangle=list(col=myColorPal[2])
															,box.umbrella=list(col=myColorPal[2])
															,plot.polygon=list(col=myColorPal[2])
															)
																				
														)
						,scales=list(x=list(rot=45))
						,par.strip.text=list(lines=2)
						)
																				
}






