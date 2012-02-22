# TODO: Add comment
# 
# Author: mike
###############################################################################
#.postProcessBoxPlotSVG<-function(sfile)
#{
#	doc = xmlParse(sfile)
#	top<-xmlRoot(doc)
#	nodes<-xmlChildren(top)
#	toMove<-nodes[[length(nodes)-1]]
#	removeChildren(top,list(length(nodes)-1))##remove the node from back
#	toMove<-toMove[[1]]##remove link
#	removeChildren(toMove,list(1))##remove title
#	addChildren(top,kids=list(toMove),at=5)##add it to top
#	saveXML(top,sfile )
#	
#}

.postProcessSVG<-function(sfile)
{

#	browser()
	
	srcFile<-list.files(system.file("javascript",package="QUALIFIER"),pattern="highlight.js",full=T)
	srcFile<-file(srcFile, "r")
	srcCode<-readLines(srcFile)
	
	close(srcFile)
	
	doc = xmlParse(sfile)
	
	top<-xmlRoot(doc)
	
	
	oldNode<-top[["script"]]
	newNode<-xmlNode("script",attrs=c(type="text/ecmascript"))
	newNode<-addChildren(newNode,xmlCDataNode(paste(srcCode,collapse="\n")))
	
	replaceNodes(oldNode,newNode)
		
	saveXML(top,sfile)
	
}
#TODO:refere to latticeParseFormula for more generic parser

.formulaParser<-function(formula)
{
#	browser()
	
	
	bTerm<-formula[[3]]
	cond<-""
	if(length(bTerm)>1)
	{
		xTerm<-bTerm[[2]]
		cond<-bTerm[[3]]
	}else
	{
		xTerm<-bTerm
	}
	
	if(length(cond)>1)
	{
		groupBy<-as.character(cond)[-1]
	}else
	{
		if(length(bTerm)>1)
		{
			groupBy<-as.character(cond)	
		}else
		{
			groupBy<-NULL
		}
		
	}
	
	yTerm<-formula[[2]]
	func<-NULL
	if(length(yTerm)>1)
	{
		func<-yTerm[[1]]
		yTerm<-yTerm[[2]]
	}
	
	list(xTerm=xTerm,yTerm=yTerm,func=func,groupBy=groupBy)
}
.isRoot<-function(gh,node)
{
	return(ifelse(length(getParent(gh,node))==0,TRUE,FALSE))
}



#cell number(first node in gating hierachy) marginal events and MFI are also based on sub-populations defined by manual gates
#which are extracted during the batch process of storing % and MFI

saveToDB<-function(db,G,annoData)
{
	#####load sample info from xls
#	annoData<-read.csv(metaFile, as.is=TRUE)
	
	###import global ID for each fcs from labkey
#	ncfs<-ncFlowSet(G)
#	objID.table<-ncfs@phenoData@data
#	objID.table$id<-1:nrow(objID.table)
#	annoData<-merge(annoData,objID.table,by.x="FCS_Files",by.y="name")
	
	annoData$id<-1:nrow(annoData)
	if(!"name"%in%colnames(annoData))
		stop("'name' column that stores FCS file names is missing in annotation data!")
#	browser()
	#do some filtering for annoData
#	annoData<-subset(annoData,!annoData$Tube%in%c("EMA/EMA/EMA/EMA/EMA","VD1/VD2/GD/BLK/CD3"))
	annoData<-subset(annoData,name%in%getSamples(G))
	
	#do some format converting
#	annoData$RecdDt<-as.Date(annoData$RecdDt,"%m/%d/%y")
#	annoData$AnalysisDt<-as.Date(annoData$AnalysisDt,"%m/%d/%y")

		
	##fit it into GatingSet(or flowSet)
#	colnames(annoData)[which(colnames(annoData)=="FCS_Files")]<-"name"
	rownames(annoData)<-annoData$name
	
	G<-G[which(getSamples(G)%in%annoData$name)]
	
	annoData<-annoData[getSamples(G),]	#sort by sample order in gh

	##extract tubeID from filename by stripping the first two prefix (presummably date and fileid on each tube)
	annoData$tubeID<-unlist(lapply(annoData$name,function(x){
#			browser()
						strsplit(
								paste(strsplit(as.character(x),"_")[[1]][c(-1,-2)],collapse="_")
								,"\\.")[[1]][[1]]
					}))


	pData(G)<-annoData
	#do the filtering for Gating set
	
	
	###append the data to db

	db$params<-colnames(getData(G[[1]]))
	db$G<-G
	db$GroupOutlierResult<-db$outlierResult<-data.frame(sid=integer(),qaID=integer(),stringsAsFactors=F)
	db
}



makeQaTask<-function(db,checkListFile)
{
	db$qaCheckList<-read.csv(checkListFile)
	
	qaTask.list<-apply(db$qaCheckList,1,function(curRow,db){
#browser()			
				curQa<-new("qaTask"
						,qaID=as.integer(curRow["qaID"])
						,qaName=curRow["qaName"]
						,description=curRow["description"]
						,qaLevel=curRow["qaLevel"]
						,pop=curRow["pop"]
						,formula=as.formula(curRow["formula"])
						,plotType=curRow["plotType"]
						,db=db
				)
				curQa					
			},db)
	names(qaTask.list)<-db$qaCheckList$qaName
	qaTask.list
}

queryStats<-function(db,formula,subset=NULL,pop=character(0))
{
#	browser()
	formuRes<-.formulaParser(formula)
	
	yTerm<-formuRes$yTerm
	func<-formuRes$func
	groupBy<-formuRes$groupBy

	
	statsType<-as.character(yTerm)
	
	if(tolower(statsType)=="percent")##take percent as the same as proportion
		statsType="proportion"
	ret_anno<-pData(db$G)
	
	ret_stats<-db$statsOfGS
	#filter by subset 
	if(length(pop)!=0)
	{
		ret_stats<-subset(ret_stats,population%in%pop)	
	}
	ret_stats<-subset(ret_stats,stats%in%statsType)
	
	ret<-merge(ret_stats,ret_anno,by.x="id",by.y="id")
	
	##add stain column from tube and channel
	ret$stain<-apply(ret,1,function(x){
				curChannel<-as.character(x["channel"])
#									browser()
				
				if(is.na(curChannel)||curChannel%in%db$params[1:2])
				{	
					curStain<-NA
				}else
				{
					chnlInd<-which(db$params[3:7]==curChannel)
					curStain<-strsplit(x["Tube"],"\\/")[[1]][chnlInd]
				}
				curStain
			})

#	browser()
	#filter by subset 
	if(!is.null(subset))
	{
		ret<-subset(ret,eval(parse(text=subset)))	
	}
	##apply the function to value in each group
	if(!is.null(func))
	{
		factors<-lapply(groupBy,function(x){

					eval(substitute(ret$v,list(v=x)))
				})
#					browser()		
		ret<-by(ret,factors,function(x){
#							browser()
					x$value<-eval(substitute(f(x$value),list(f=func)))
					x
				})
		ret<-do.call("rbind",ret)
	}
	
	ret
	
	
}




openDB<-function()
{
#	dbFile<-system.file("data/qa.db",package="flowQ")
	dbFile<-"~/rglab/workspace/flowQ/data/qa.db"
	m <- dbDriver("SQLite")
	cn<-dbConnect(m, dbname = dbFile)
	cn
}

closeDB<-function(cn){
	# clean up
	dbDisconnect(cn)
	
}


qa.report<-function(db,outDir,splash=TRUE,plotAll=FALSE)
{
	if(missing(outDir))
		stop("outDir has to be specified!")
	options(warn=0)
	anno<-pData(db$G)
#	browser()
	##output outlier results to csv
	outResult<-merge(db$outlierResult,db$statsOfGS[,c("sid","id","channel")])
	outResult<-merge(outResult,db$qaCheckList)
	outResult<-merge(outResult,anno)[,c("sid","id","name","qaName","channel","Tube")]
	
	names(outResult)<-c("sid","id","fcsFile" ,"qaTask","channel","Tube")
	
	
	gOutResult<-merge(db$GroupOutlierResult,db$statsOfGS[,c("sid","id","channel")])
	gOutResult<-merge(gOutResult,db$qaCheckList)
	gOutResult<-merge(gOutResult,anno)
	
	names(gOutResult)[names(gOutResult)=="qaName"]<-"qaTask"
	
	imageDir<-file.path(outDir,"image")
	#init the image folder
	
	dir.create(imageDir,recursive=TRUE,showWarnings=F)
#	file.remove(list.files(imageDir,full=TRUE))
	from<-list.files(system.file("htmlTemplates",package="QUALIFIER"),pattern="qaReport",full=T)
	dir.create(imageDir)
 	imageDir<-file.path(imageDir,basename(from))	
	file.copy(from=from
				,to=imageDir)
	

#	library(reshape)
#	library(hwriter)
	
	
	p <- openPage(dirname=outDir
						,filename="index.html"
						,link.css=file.path(basename(imageDir),"qaReport.css")
						,link.javascript=file.path(basename(imageDir),"qaReport.js")
						,title = "qa report"
	)
	hwrite("Flow Data Quality Accessment Report",p,class="ReportTitle",div=TRUE,br=TRUE)
	hwrite("ITN029ST",p,class="ReportSubTitle",div=TRUE,br=TRUE)
	if(splash)
		hwrite(paste("Generated on"
					,date()	
					, "by QUALIFIER 0.99.1"
				)
				,div=TRUE
				,class="splash"
				,p
			)
	db$objcount<-0
#	browser()
	by(db$qaCheckList,db$qaCheckList$qaLevel,function(sub1,db){
				
#				browser()
			hwrite(paste(unique(sub1$qaLevel),"Level"),p,heading=1)
			apply(sub1,1,function(curRow,db){
#						browser()	
					curQa<-new("qaTask"
								,qaID=as.integer(curRow["qaID"])
								,qaName=as.character(curRow["qaName"])
								,description=as.character(curRow["description"])
								,qaLevel=as.character(curRow["qaLevel"])
								,pop=as.character(curRow["pop"])
								,formula=as.formula(as.character(curRow["formula"]))
								,plotType=as.character(curRow["plotType"])
								,db=db
								)
						
					
					t1<-subset(outResult,qaTask==getName(curQa))
					g1<-subset(gOutResult,qaTask==getName(curQa))	

					hwrite(paste(description(curQa)
#										,hwrite(nrow(t1),class='count')
								)
							,p
							,heading=3)
#					browser()
					nFscFailed<-length(unique(t1$fcsFile))
					nGroupFailed<-0
					
					formula1<-formula(curQa)
					cond<-NULL
					if(length(formula1[[3]])>1)
					{
						cond<-formula1[[3]][[3]]
						xTerm<-formula1[[3]][[2]]
					}else
					{
						xTerm<-formula1[[3]]
					}
					groupField<-NULL
					if(plotType(curQa)=="bwplot")
					{
						groupField<-as.character(xTerm)
						nGroupFailed<-length(unique(
												eval(substitute(g1$v
																,list(v=groupField)
																)
													)
												)
											)	
					}
					
					if(nFscFailed>0||nGroupFailed>0)
					{
						db$objcount<-db$objcount+1
						hwrite(paste(
								 hwrite("+",div=TRUE
									,id=paste("detTriggerIn",db$objcount,sep="_") 
									,class="QADetTrigger" 
									,onclick=paste("toggleSection(",db$objcount,")",sep="")
									,style="display: block;")
					
								,hwrite("&ndash;",div=TRUE
									,id=paste("detTriggerOut",db$objcount,sep="_") 
									,class="QADetTrigger" 
									,onclick=paste("toggleSection(",db$objcount,")",sep="")
									,style="display: none;")
								,ifelse(nFscFailed>0	
										,paste(
												hwrite(nFscFailed,class='count')					
												," FCS files "
												)
										,"")
								,ifelse(nGroupFailed>0	
										,paste(
												hwrite(nGroupFailed,class='count')					
												,groupField
												)
										,"")
								," failed the check"
								)
							,heading=4
							,p
							)
					
						
						
						
						#the conditioning section may contain multiple variables
						#the first one is only used for grouped outlier detection
						#here is used to plot a subgroup
						#the second conditioning variable is used for the plot
#						browser()
												
						
						if(length(cond)>1)
						{
							##individual outlier
							groupBy<-as.character(cond[[2]])
							groupByStr<-paste("t1$",groupBy,sep="")
							
							formula1[[3]][[3]]<-cond[[3]]
															
							
							if(nFscFailed>0)
							{
								if(getName(curQa)=="spike")
								{
									f1<-as.formula(paste("fcsFile","channel",sep="~"))
									
									
								}else
								{
									f1<-as.formula(paste("fcsFile",groupBy,sep="~"))
									
								}
								m.outResult<-melt(t1,measure.vars="qaTask")
								castResult<-cast(m.outResult,f1
										,fun.aggregate=length)
								castResult<-as.data.frame(castResult)
								castResult$subTotal<-rowSums(castResult[,-1])
								castResult<-castResult[order(castResult$subTotal,decreasing=T),]
								castResult$fcsFile<-as.character(castResult$fcsFile)
								castResult<-rbind(castResult,c(fcsFile="Total",colSums(castResult[,-1])))
								rownames(castResult)<-NULL#1:nrow(castResult)
								hwrite(
										paste(hwrite("hide/show table"#add toggle word
													,onclick=paste("toggleTable(",db$objcount,")",sep="")
													,link="#"
													,class="showtable"
													)
											,hwrite(#encapsulate into div in order to have an id
													hwrite(castResult#output table
															,row.class="firstline"
															,col.class=list("fcsFile"="firstcolumn",'subTotal'="lastcolumn")
													)
													,div=TRUE
													,style="display: none;"
													,id=paste("table",db$objcount,sep="_")
													)
											
											,sep=""
											)
										,p
										,div=TRUE
										,style="display: none;"
										,id=paste("section",db$objcount,sep="_")
								
									)
#									browser()
									
							}
							##group outlier
							if(nGroupFailed>0)
							{
								if(getName(curQa)=="spike")
								{
									f1<-paste(groupField,"channel",sep="~")
									
								}else
								{
									f1<-paste(groupField,groupBy,sep="~")
								}
								f1<-as.formula(f1)
								m.outResult<-melt(g1,measure.vars="qaTask")
								castResult<-cast(m.outResult,f1
										,fun.aggregate=length)
								castResult<-as.data.frame(castResult)
								castResult$subTotal<-rowSums(castResult[,-1])
								castResult<-castResult[order(castResult$subTotal,decreasing=T),]
								eval(substitute(
										castResult$v<-as.character(castResult$v)
										,list(v=groupField)
										)
									)
								castResult<-rbind(castResult,c("Total",colSums(castResult[,-1])))
									
								
								rownames(castResult)<-NULL#1:nrow(castResult)
								
								hwrite(
										paste(hwrite("hide/show table"#add toggle word
														,onclick=paste("toggleTable(",db$objcount,")",sep="")
														,link="#"
														,class="showtable"
												)
												,hwrite(#encapsulate into div in order to have an id
														hwrite(castResult
																,row.class="firstline"
																,col.class=eval(parse(text=paste("list('"
																						,groupField
																						,"'='firstcolumn','subTotal'='lastcolumn')"
																						,sep="")
																		)
																)
														)
														,div=TRUE
														,style="display: none;"
														,id=paste("table",db$objcount,sep="_")
												)
												
												,sep=""
										)
										
										,p
										,div=TRUE
										,style="display: none;"
										,id=paste("section",db$objcount,sep="_")
								
								)
							}
#							browser()
							yy<-queryStats(db,formula1,pop=getPop(curQa))
							factors<-lapply(groupBy,function(x){
										eval(substitute(yy$v,list(v=x)))
									})
							by(yy,factors,function(sub2,curQa,groupBy){
										
#											browser()
										#find the outliers of the current pannael
										#matching sid 
										curOut<-t1[t1$sid%in%sub2$sid,]
										curgOut<-g1[g1$sid%in%sub2$sid,]
										
										curGroup<-unique(eval(parse(text=paste("sub2$",groupBy,sep=""))))
#
										db<-getData(curQa)
										db$objcount<-db$objcount+1		
#										browser()
										##heading
										hwrite(paste(
														hwrite("+",div=TRUE
																,id=paste("detTriggerIn",db$objcount,sep="_") 
																,class="QADetTrigger" 
																,onclick=paste("toggleSection(",db$objcount,")",sep="")
																,style="display: block;")
														
														,hwrite("&ndash;",div=TRUE
																,id=paste("detTriggerOut",db$objcount,sep="_") 
																,class="QADetTrigger" 
																,onclick=paste("toggleSection(",db$objcount,")",sep="")
																,style="display: none;")
														,curGroup
#														,hwrite(length(unique(sub2$name)),class='count')
														,ifelse(nrow(curOut)>0	
																,paste(
																		hwrite(length(unique(curOut$id)),class='count')					
																		," FCS files "
																)
																,"")
														,ifelse(nrow(curgOut)>0	
																,paste(
																		hwrite(length(unique(eval(substitute(curgOut$v
																										,list(v=groupField)
																										))
																							))
																				,class='count')					
																		,groupField
																)
																,"")
												)
												,heading=4
												,p
										)
										
#										##table+image
#
										
#										if(nrow(curOut)>0||nrow(curgOut)>0)
										if(getName(curQa)=="MFIOverTime")
										{
											relation<-"free"										
											rFunc<-rlm
										}else
										{
											relation<-NULL										
											rFunc<-NULL
										}	
										plotCallStr<-paste("plot(curQa,formula1,dest=imageDir,relation=relation,rFunc.=rFunc,plotAll=plotAll,subset=\""
												,groupBy,"=='",curGroup,"'\")",sep="")
#										browser()
										imageName<-eval(parse(text=plotCallStr))
										rownames(curOut)<-NULL#1:nrow(sub2)
										rownames(curgOut)<-NULL#1:nrow(sub2)
										#section
										hwrite(paste(
														##tables+toggler	
														paste(	#toggler
																hwrite("hide/show table"
																		,onclick=paste("toggleTable(",db$objcount,")",sep="")
																		,link="#"
																		,class="showtable"
																)
																#encapsulate tables into div in order to have an id
																,hwrite(
																		paste(
																				ifelse(nrow(curOut)>0	
																					,hwrite(curOut[,c("fcsFile","channel")]
																							,row.class="firstline"
																							,col.class=list("fcsFile"="firstcolumn",'subTotal'="lastcolumn")
																							)
																					,"")
																				,ifelse(nrow(curgOut)>0	
																					,hwrite(unique(curgOut[,c(groupField,"channel")])
																							,row.class="firstline"
																							,col.class=eval(parse(text=paste("list('"
																													,groupField
																													,"'='firstcolumn','subTotal'='lastcolumn')"
																													,sep="")
																												)
																											)
																					
																							)
																					,"")
																			,sep=""
																			)
																		,div=TRUE
																		,style="display: none;"
																		,id=paste("table",db$objcount,sep="_")
																		)
																		
																
																,sep=""
																)
														
														##image								
														,hwrite(paste("<embed src='"
																		,file.path(basename(imageDir),imageName)
																		,"' type='image/svg+xml' width=1000 height=800/>"
																		,sep=""
																	)
																,div=TRUE
																)
													)
												,p
												,div=TRUE
												,style="display: none;"
												,id=paste("section",db$objcount,sep="_")
										)
									}
								,curQa
								,groupBy
								)
						}else
						{
							#if only one conditioning variable
							#simply order by it and output the fcsfile list
							if(length(cond)==0)
							{
#								groupBy<-as.character(formula1[[3]])
								castResult<-eval(substitute(unique(u[,c(w),drop=FALSE])
															,list(u=as.symbol("t1"),w="fcsFile")
															)
												)
								gcastResult<-eval(substitute(unique(u[,c(w),drop=FALSE])
															,list(u=as.symbol("g1"),w=groupField)
															)
													)
								
							}else
							{
								groupBy<-as.character(cond)
#								groupByStr<-paste("t1$",groupBy,sep="")
								castResult<-eval(substitute(u[order(u$v),c(w,v)]
															,list(u=as.symbol("t1"),v=groupBy,w="fcsFile")
															)
													)
								#t1[order(eval(parse(text=groupByStr))),c("fcsFile",groupBy)]
								gcastResult<-eval(substitute(u[order(u$v),c(w,v)]
															,list(u=as.symbol("g1"),v=groupBy,w=groupField)
															)
												)
							}
							
							
#							browser()
							##make sure the w and h pass to plot and large enough to display strip text
							imageName<-plot(curQa
											,formula(curQa)
#											,subset=paste("Tube%in%c('"
#															,paste(unique(castResult$Tube),collapse="','")
#															,"')"
#															,sep="")
											,plotAll.=plotAll
											,dest=imageDir
											,width=27,height=13)
							
#							browser()
							rownames(castResult)<-NULL#1:nrow(castResult)
							#section
							hwrite(paste(
											paste(
													hwrite("hide/show table"#add toggle word
															,onclick=paste("toggleTable(",db$objcount,")",sep="")
															,link="#"
															,class="showtable"
															)
													,hwrite(#encapsulate into div in order to have an id
															paste(
																	ifelse(nrow(t1)>0	
																			,hwrite(castResult
																					,row.class="firstline"
																					,col.class=list("fcsFile"="firstcolumn",'subTotal'="lastcolumn")
																			)
																			,"")
																	,ifelse(nrow(g1)>0	
																			,hwrite(gcastResult
																					,row.class="firstline"
																					,col.class=eval(parse(text=paste("list('"
																											,groupField
																											,"'='firstcolumn','subTotal'='lastcolumn')"
																											,sep="")
																											)
																									)
																			
																					)
																			,"")
																	,sep=""
																	)
															,div=TRUE
															,style="display: none;"
															,id=paste("table",db$objcount,sep="_")
														)
													
													,sep=""
													)
							
											##table	
											
															##image								
											,hwrite(paste("<embed src='"
															,file.path(basename(imageDir),imageName)															,"' type='image/svg+xml' width=1000 height=800/>"
															,sep=""
															)
													,div=TRUE
													)
											,sep=""
											)
									,p
									,div=TRUE
									,style="display: none;"
									,id=paste("section",db$objcount,sep="_")
									)
							
						}
					}
							
				},db)
			},db)
	

	closePage(p, splash=FALSE)
}



### wrap all the qa preprocessing steps into to on batch call 
qa.batch<-function(db)
{
	qa.report(db)
}
	
