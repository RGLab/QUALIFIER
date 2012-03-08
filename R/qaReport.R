# TODO: Add comment
# 
# Author: wjiang2
###############################################################################
##generate report for qaTask list
setMethod("qaReport", signature=c(obj="list"),
		function(obj,outDir,plotAll=FALSE,...){
			if(missing(outDir))
				stop("outDir has to be specified!")
			p<-.writeHead(outDir,...)
#			browser()
			qaWrite.list(obj,p,outDir,plotAll)
		})
##generate report for single qaTask 
setMethod("qaReport", signature=c(obj="qaTask"),
		function(obj,...){
			
#			browser()
			qaReport(list(obj),...)
		})

qaWrite.list<-function(x,page,...){
			
			db<-getData(x[[1]])
			db$objcount<-0
			
			tasksBylevel<-split(x,unlist(lapply(x,qaLevel)))

			lapply(tasksBylevel,function(curTaskGroup){			
#				browser()
						hwrite(paste(qaLevel(curTaskGroup[[1]]),"Level"),page,heading=1)
						lapply(curTaskGroup,qaWrite.task,page,...)

					})
			message("report generated!")
			
			closePage(page, splash=FALSE)
		}

qaWrite.task<-function(x,p,outDir,plotAll){
			
			imageDir<-file.path(outDir,"image")
			
			db<-getData(x)
			anno<-pData(db$G)
			curQaID<-qaID(x)
#			browser()
			outResult<-subset(db$outlierResult,qaID==curQaID)
			outResult<-merge(outResult,db$statsOfGS[,c("sid","id","channel")])
			outResult<-merge(outResult,anno)#[,c("sid","id","name","channel","Tube")]
#			names(outResult)<-c("sid","id","fcsFile" ,"channel","Tube")
			colnames(outResult)[colnames(outResult)=="name"]<-"fcsFile"
			if(nrow(outResult)>0)
				outResult$qaTask<-getName(x)
			
			gOutResult<-subset(db$GroupOutlierResult,qaID==curQaID)
			gOutResult<-merge(gOutResult,db$statsOfGS)
			gOutResult<-merge(gOutResult,anno)
			nFscFailed<-length(unique(outResult$fcsFile))
			if(nrow(gOutResult)>0)
				gOutResult$qaTask<-getName(x)			
	
			
			hwrite(paste(description(x)
#										,hwrite(nrow(outResult),class='count')
					)
					,p
					,heading=3)
#					browser()
#			nFscFailed<-length(unique(outResult$fcsFile))
			nGroupFailed<-0
			
			formula1<-formula(x)
			
			formuRes<-.formulaParser(formula1)
			xTerm<-formuRes$xTerm
#			groupBy<-formuRes$groupBy
			
			statsType<-matchStatType(db,formuRes)
#			cond<-NULL
#			if(length(formula1[[3]])>1)
#			{
#				cond<-formula1[[3]][[3]]
#				xTerm<-formula1[[3]][[2]]
#			}else
#			{
#				xTerm<-formula1[[3]]
#			}
			groupField<-NULL
			if(plotType(x)=="bwplot")
			{
				
				if(qpar(x)$horiz)
					groupField<-as.character(formuRes$yTerm)	
				else
					groupField<-as.character(formuRes$xTerm)
								
				nGroupFailed<-length(unique(
								eval(substitute(gOutResult$v
												,list(v=groupField)
										)
								)
						)
				)	
			}
#			browser()
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
				
				
				if(length(formuRes$groupBy)>1)
				{
					##individual outlier
					groupBy<-as.character(formuRes$groupBy[1])
					groupByStr<-paste("outResult$",groupBy,sep="")
					
					formula1[[3]][[3]]<-as.symbol(paste(formuRes$groupBy[-1],collapse="*"))
					
					
					if(nFscFailed>0)
					{
						if(getName(x)=="spike")
						{
							f1<-as.formula(paste("fcsFile","channel",sep="~"))
							
							
						}else
						{
							f1<-as.formula(paste("fcsFile",groupBy,sep="~"))
							
						}
						m.outResult<-melt(outResult,measure.vars="qaTask")
						castResult<-cast(m.outResult,f1
								,fun.aggregate=length)
						castResult<-as.data.frame(castResult)
#								browser()
						castResult$subTotal<-rowSums(castResult[,-1,drop=FALSE])
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
						if(getName(x)=="spike")
						{
							f1<-paste(groupField,"channel",sep="~")
							
						}else
						{
							f1<-paste(groupField,groupBy,sep="~")
						}
						f1<-as.formula(f1)
						m.outResult<-melt(gOutResult,measure.vars="qaTask")
						castResult<-cast(m.outResult,f1
								,fun.aggregate=length)
						castResult<-as.data.frame(castResult)
						castResult$subTotal<-rowSums(castResult[,-1,drop=FALSE])
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
#					yy<-queryStats(db,formula1,pop=getPop(x))
					yy<-queryStats(db,statsType=statsType,pop=getPop(x),isTerminal=T,fixed=F)

					factors<-lapply(groupBy,function(x){
								eval(substitute(yy$v,list(v=x)))
							})
					by(yy,factors,function(sub2,x,groupBy){
								
#											browser()
								#find the outliers of the current pannael
								#matching sid 
								curOut<-outResult[outResult$sid%in%sub2$sid,]
								curgOut<-gOutResult[gOutResult$sid%in%sub2$sid,]
								
								curGroup<-unique(eval(parse(text=paste("sub2$",groupBy,sep=""))))
#
								db<-getData(x)
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
#								if(getName(x)=="MFIOverTime")
#								{
#									relation<-"free"										
#									rFunc<-rlm
#								}else
#								{
#									relation<-NULL										
#									rFunc<-NULL
#								}
								
#								if(getName(x)%in%c("RedundantStain","MNC"))
#								{
#									xaxis.draw<-FALSE
#								}else
#								{
#									xaxis.draw<-TRUE
#								}
#								if(getName(x)%in%c("spike"))
#								{
#									ylab<-"cumulative z-score"
#								}else
#								{
#									ylab<-NULL
#								}
#										browser()
										
							
								plotCallStr<-quote(plot(x
														,formula1
														,dest=imageDir
#														,par=list(ylab=ylab
#																	,scales=list(x=c(draw=xaxis.draw)
#																				,y=c(relation=relation)
#																				)
#																	)
														,rFunc=rFunc
														,plotAll=plotAll
														,subset=groupBy==curGroup
														)
														)
								plotCallStr$subset[[2]]<-as.symbol(eval(plotCallStr$subset[[2]]))
								plotCallStr$subset[[3]]<-as.character(eval(plotCallStr$subset[[3]]))
#	
#						
								imageName<-eval(plotCallStr)

#								imageName<-eval(parse(text=plotCallStr))
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
							,x
							,groupBy
					)
				}else
				{
#					browser()
					#if only one conditioning variable
					#simply order by it and output the fcsfile list
					if(length(formuRes$groupBy)==0)
					{
#								groupBy<-as.character(formula1[[3]])
						castResult<-eval(substitute(unique(u[,c(w),drop=FALSE])
										,list(u=as.symbol("outResult"),w="fcsFile")
								)
						)
						gcastResult<-eval(substitute(unique(u[,c(w),drop=FALSE])
										,list(u=as.symbol("gOutResult"),w=groupField)
								)
						)
						
					}else
					{
						groupBy<-formuRes$groupBy
#								groupByStr<-paste("outResult$",groupBy,sep="")
						castResult<-eval(substitute(u[order(u$v),c(w,v)]
													,list(u=as.symbol("outResult"),v=groupBy,w="fcsFile")
												)
											)
						#outResult[order(eval(parse(text=groupByStr))),c("fcsFile",groupBy)]
						gcastResult<-eval(substitute(u[order(u$v),c(w,v)]
														,list(u=as.symbol("gOutResult"),v=groupBy,w=groupField)
												)
										)
					}
					
					
#							browser()
					##make sure the w and h pass to plot and large enough to display strip text
					imageName<-plot(x
							,y=formula(x)
							,plotAll=plotAll
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
															ifelse(nrow(outResult)>0	
																	,hwrite(castResult
																			,row.class="firstline"
																			,col.class=list("fcsFile"="firstcolumn",'subTotal'="lastcolumn")
																	)
																	,"")
															,ifelse(nrow(gOutResult)>0	
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
			
		
			
}


.writeHead<-function(outDir,title="Flow Data Quality Accessment Report",subTitle="",splash=TRUE)
{
	
	options(warn=0)

	imageDir<-file.path(outDir,"image")
	#init the image folder
	dir.create(imageDir,recursive=TRUE,showWarnings=F)
#	file.remove(list.files(imageDir,full=TRUE))
	from<-list.files(system.file("htmlTemplates",package="QUALIFIER"),pattern="qaReport",full.names=TRUE)

	file.copy(from=from,to=imageDir)

	p <- openPage(dirname=outDir
			,filename="index.html"
			,link.css=file.path(basename(imageDir),"qaReport.css")
			,link.javascript=file.path(basename(imageDir),"qaReport.js")
			,title = "qa report"
	)
#		browser()
	
	hwrite(title,p,class="ReportTitle",div=TRUE,br=TRUE)
	hwrite(subTitle,p,class="ReportSubTitle",div=TRUE,br=TRUE)
	if(splash)
		hwrite(paste("Generated on"
						,date()	
						, "by QUALIFIER 0.99.1"
				)
				,div=TRUE
				,class="splash"
				,p
		)
	p
}
