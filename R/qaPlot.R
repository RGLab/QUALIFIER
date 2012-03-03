# TODO: Add comment
# 
# Author: mike
###############################################################################
.FileNameGen<-function(prefix=NA,ID=NA,population=NA,channel=NA,stain=NA,stats=NA,...)
{
	fileName<-c(prefix,ID,basename(population),channel,stain,stats,...)
	fileName<-fileName[!is.na(fileName)]
	paste(fileName,collapse="_")
	
}
#TODO:to merge this single plot to groupplot since groupPlot is also able to produce the same xyplot
#just need to add timeline plot to groupPlot routine.
##single plot for each FCS
qa.singlePlot<-function(db,yy,statsType)
{
#	browser()
#	for(i in 1:nrow(yy))
#	{
#		curRow<-yy[i,]
	curRow<-yy
	ghInd<-which(getSamples(db$G)==curRow$name)
	if(length(ghInd)>0)
	{
		curGh<-db$G[[ghInd]]
		parentNode<-getParent(curGh,as.character(curRow$node))
		if(length(parentNode)>0)
		{
			x<-getData(curGh,parentNode)
		}else
		{
			x<-getData(curGh)
		}
		
		curGate<-getGate(curGh,as.character(curRow$node))
		
		#		curGate<-getGate(G[[which(getSamples(G)==curRow$name)]],as.character(curRow$node))
		#				browser()
		individualPlot(x,curGate,curRow,statsType)
	}
#	}	
}
individualPlot<-function(x,curGate,curRow,statsType)
{
	cols <- colorRampPalette(IDPcolorRamp(21,
					t(col2hsv(c("blue","green","yellow","red"))),
					fr=c(0.7,0)))
	
	#get gate
#	statsType<-curRow$stat
	channel<-as.character(curRow$channel)
	pop<-as.character(curRow$population)

#	curID<-pData(x)$id
	params<-flowCore:::colnames(x)

	if(is.object(curGate))
	{
		chnls<-parameters(curGate)
		fres <- filter(x,curGate)
	}else
	{
		chnls<-ifelse(statsType=="spike",channel,NA)
		fres<-NULL
	}
	
	

	if(statsType=="count"&&pop=="/root")##total cell count
	{
		##individual xyplot without gate
		xyplot(eval(parse(text=paste("`",params[2],"`~`",params[1],"`",sep="")))
						,data=x,smooth=FALSE,colramp=cols)
	}else
	{

		
		if(length(chnls)==1)
		{
			
			
			if(statsType=="spike")
			{
				t1<-paste("`",chnls[1],"`~`",params[grep("time",params,ignore.case=T)],"`",sep="")					
			}else
			{
				t1<-paste("`",params[2],"`~`",chnls[1],"`",sep="")
			}
		}else
		{
			t1<-paste("`",chnls[2],"`~`",chnls[1],"`",sep="")	
		}
		
		if(is.object(curGate))
		{
			gateName<-strsplit(curGate@filterId,split="\\.")[[1]][2]##remove prefix			
			mainTitle<-paste(gateName,"Gate")	
		}else
		{
			mainTitle<-""
		}
		
#			browser()	

		xyplot(x=as.formula(t1)
						,data=x
#							xlim=range(exprs(x[[1]])[,parameters(curGate)[1]]),
#							ylim=range(exprs(x[[1]])[,parameters(curGate)[2]]),
						,smooth=FALSE
						,colramp=cols
						,filter=fres
						,names=FALSE
#									pd=pData(x),
						,main=mainTitle
						,par.settings=list(gate.text=list(text=0.7
												,alpha=1
												,cex=1
												,font=1)
										,gate=list(
												fill="transparent"
												,lwd<-2
												,lty="solid"
												,alpha=1
												,col="red"
											)
										,flow.symbol=list(cex=ifelse(statsType=="spike",2,flowViz.par.get("flow.symbol")$cex))
								)
						,panel=panel.xyplot.flowframeEx
				)

	}

}
#TODO:check why colnames from flowCore is not dispatched correctly without namespace explicitly specified
##group plot for each sampleID or other aggregation ID
qa.GroupPlot<-function(db,yy)
{
	cols <- colorRampPalette(IDPcolorRamp(21,
					t(col2hsv(c("blue","green","yellow","red"))),
					fr=c(0.7,0)))

#	browser()
	fcsNames<-as.character(unique(yy$name))
	##TODO:strange behavior happens again here :idexing G by sampleName failed
	#make sure to extract gateing set by the order of yy$name
	sampleInds<-match(fcsNames,getSamples(db$G))
	
	
	if(length(sampleInds)>0)#check if the target exist in the gateing hierarchy 
	{
		#get the parent population for the scatter plot
		curRow<-yy[1,]
#		statsType<-curRow$stat
#		channel<-as.character(curRow$channel)
		pop<-as.character(curRow$population)
		
		curSampleInd<-which(getSamples(db$G)%in%curRow[,"name"])
		curGh<-db$G[[curSampleInd]]
		curNode<-as.character(curRow[,"node"])
		curGate<-getGate(curGh,curNode)
		parentNode<-getParent(curGh,curNode)
		parentNodeInd<-which(getNodes(curGh)%in%parentNode)
		if(length(parentNodeInd)>0)
		{
			fs1<-getData(db$G[sampleInds],parentNodeInd)
		}else
		{
			fs1<-getData(db$G[sampleInds])
		}
		sampleNames(fs1)<-fcsNames

		if(!"outlier"%in%colnames(yy))
			yy$outlier<-FALSE
		pData(fs1)$outlier<-yy[,]$outlier
		varMetadata(fs1)["outlier",]<-"outlier"
		obj<-NULL
		if(!pop=="/root")##total cell count
		{
			
		
			fres<-filter(fs1,curGate)
			if(length(parameters(fres[[1]]))==2)
			{
				t1<-paste("`",parameters(curGate)[1],"`~`",parameters(curGate)[2],"`",sep="")
			}else
			{
				t1<-paste("`",flowCore::colnames(fs1)[grep("SSC",flowCore::colnames(fs1))],"`~`",parameters(fres[[1]])[1],"`",sep="")
			}
	
			obj<-xyplot(x=as.formula(t1),
							data=fs1,
							smooth=FALSE,
							colramp=cols,
							filter=fres,
							names=FALSE,
							pd=pData(fs1),
							par.settings=list(gate.text=list(text=0.7
															,alpha=1
															,cex=1
															,lineheight=2
															,font=1)
												,gate=list(
														fill="transparent"
														,lwd<-2
														,lty="solid"
														,alpha=1
														,col="red"
														)
												),
							panel=panel.xyplot.flowsetEx
					)
			}
	}
			
			
	
	return(obj)
	
#	plotObj
}





setMethod("plot", signature=c(x="qaTask"),
		function(x,y,...){
			
			if(missing(y))
				y<-NULL
			plot.qaTask(qaObj=x,formula=y,...)

		})

plot.qaTask<-function(qaObj,formula,Subset,width=10,height=10,par,isTerminal=TRUE,fixed=FALSE,...)#,channel=NA,stain=NA,tube=NA
{
#	browser()
	par_old<-qpar(qaObj)
	if(!missing(par))##overwrite the elements of par slot of qa object if provided by argument
	{
		for(x in names(par))
			eval(substitute(par_old$v<-par$v,list(v=x)))
				
		qpar(qaObj)<-par_old
	}
	lattice.options(print.function=QUALIFIER:::plot.trellisEx)
#	browser()
	db<-getData(qaObj)
	##query db
	curGroup<-NULL
#	browser()
	if(is.null(formula))
	{
		formula<-formula(qaObj)
	}

	
	if(is.null(qpar(qaObj)$horiz))
		qpar(qaObj)$horiz<-FALSE


	#parse the formula
	formuRes<-.formulaParser(formula)
	#decide the statsType(currently only one of the terms can be statType,we want to extend both in the future)
	
	statsType<-matchStatType(db,formuRes)
#	browser()
	
	
	if(missing(Subset))
		yy<-queryStats(db,statsType=statsType,pop=getPop(qaObj),isTerminal=isTerminal,fixed=fixed)
	else
		yy<-queryStats(db,statsType=statsType,substitute(Subset),pop=getPop(qaObj),isTerminal=isTerminal,fixed=fixed)
	if(nrow(yy)==0)
	{
		message("no samples are matched!")
		return()
		
	}
	
#	browser()
	#check if the conditioning variable is of factor type
	for(curGroupBy in formuRes$groupBy)
	{

		curCol<-substitute(yy$v,list(v=curGroupBy))
		if(!class(eval(curCol))=="factor")
		{
			eval(substitute(v<-as.factor(v),list(v=curCol)))
		}
	}
#			browser()
	if(getName(qaObj)=="BoundaryEvents")
		yy<-subset(yy,value>min(yy$value))##filter out those zero-value records which may cause slow plotting

	#append the outlier flag
	yy$outlier<-yy$sid%in%subset(db$outlierResult,qaID==qaID(qaObj))$sid
	yy$gOutlier<-yy$sid%in%subset(db$GroupOutlierResult,qaID==qaID(qaObj))$sid
	
	#reshape the data to include the column of the statType which can be passed to lattice	as it is
	yy<-cast(yy,...~stats)
#	browser()

	dest=list(...)$dest

	if(!is.null(dest))
	{	
		sfile<-tempfile(pattern=getName(qaObj),tmpdir=dest,fileext=".svg")
#		browser()
		devSVGTips(sfile,width=width,height=height)
		isSvg<-TRUE
	}else
	{
		isSvg<-FALSE
	}
	
	rFunc<-list(...)$rFunc
	plotAll<-list(...)$plotAll
	if(is.null(plotAll))
		plotAll<-FALSE
	
	
#	browser()
	plotObjs=new.env()
	scatterPlot<-list(...)$scatterPlot
	if(!is.null(scatterPlot)&&scatterPlot)
	{
		##if scatterPlot flag is true then just plot the scatter plot
		
		if(statsType=="spike"||(statsType=="count"&&yy$population[1]=="/root"))
		{
#			browser()
			thisCall<-lapply(1:nrow(yy),function(i)
			{
#						browser()
						qa.singlePlot(db,yy[i,,drop=FALSE],statsType)
			})
			
			
		}else
		{
			thisCall<-qa.GroupPlot(db,yy)
		}
		
		
	}else
	{#otherwise, plot the summary plot (either xyplot or bwplot)

		par<-qpar(qaObj)
		par$subscripts<-TRUE
		par$strip<-TRUE
		
		xlab<-par$xlab
		ylab<-par$ylab
		main<-par$main
		pch<-par$pch
		layout<-par$layout
		cex<-par$cex
		par.strip.text<-par$par.strip.text
		scales<-par$scales
		xscale.components<-par$xscale.components
		if(plotType(qaObj)=="xyplot")
		{
			##parse the viz par
			if(is.null(main))
				par$main<-paste(description(qaObj),curGroup,sep=":")	
			if(is.null(pch))
				par$pch<-19		
			if(is.null(par.strip.text))
				par$par.strip.text<-list(lines=2)	
			if(is.null(scales))
				par$scales<-list(x=c(cex=0.7
						#						,rot=45	
											))
#			if(is.null(xscale.components))
#				par$xscale.components<-function(...) {
#					ans <- xscale.components.default(...)
#					ans$bottom$ticks$at<-seq(from=min(yy$RecdDt),to=max(yy$RecdDt),by="2 month")
#					ans$bottom$labels$at<-seq(from=min(yy$RecdDt),to=max(yy$RecdDt),by="2 month")
#					ans$bottom$labels$labels <- zoo::as.yearmon(seq(from=min(yy$RecdDt),to=max(yy$RecdDt),by="2 month"))
#					ans
#				}
			thisCall<-quote(
							xyplot(x=formula,data=yy
									,groups=outlier
									,panel=function(x=x,y=y,data=yy,dest.=dest,plotObjs.=plotObjs,plotAll.=plotAll,...){
										panel.xyplotEx(x,y,data=data,dest.=dest,plotObjs.=plotObjs,plotAll.=plotAll,db=db,...)
								#if regression function is supplied, then plot the regression line
										if(!is.null(rFunc))
										{
	#							browser()
										
											reg.res<-try(rFunc(y~x),silent=TRUE)
											if(all(class(reg.res)!="try-error"))
											{
												sumry<-summary(reg.res)
												if(class(sumry)=="summary.rlm"){
													coefs<-coef(sumry)
													t.value<-coefs[,"t value"]
													slope<-coefs[2,"Value"]
													intercept<-coefs[1,"Value"]
													df<-summary(reg.res)$df
													pvalues<-pt(abs(t.value),df=df[1],lower.tail=FALSE)
													intercept.p<-pvalues[1]
													slope.p<-pvalues[2]
												}else if (class(sumry)=="summary.lm"){
													pvalues<-coefficients(sumry)[,4]
													slope<-coefficients(sumry)[2,1]
													intercept.p<-pvalues[1]
													slope.p<-pvalues[2]
													
												}	
												if(any(pvalues<0.05))
												{
													regLine.col<-"red"
												}else
												{
													regLine.col<-"black"
												}
												curVp<-current.viewport()
												
												
												#							panel.lines(y=rFunc(y~x)$fitted,x=x,type="l",col="black",lty="solid")
												panel.text(x=mean(curVp$xscale)
														,y=quantile(curVp$yscale)[4]
														,labels=paste("s=",format(slope*30,digits=2)
																#														," v=",format(var(y),digits=2)
																,"\np=",paste(format(slope.p,digits=2),collapse=",")
														)
														,cex=0.5
												#										,col="white"		
												)
												
												panel.abline(reg.res,col=regLine.col,lty="dashed")
												}
										}
								
									}
								)
							)
		}
		
		if(plotType(qaObj)=="bwplot")
		{
			
			plot.symbol<-trellis.par.get("plot.symbol")
			plot.symbol$col<-"red"
			trellis.par.set("plot.symbol",plot.symbol)
			
		lattice.options(print.function=plot.trellisEx)
	
			if(qpar(qaObj)$horiz)
			{
				groupBy.Panel<-as.character(formuRes$yTerm)#formula[[3]][[2]])
				
			}else
			{
				groupBy.Panel<-as.character(formuRes$xTerm)
		}

			
			
			
	
#			if(is.null(xlab))
#				par$xlab<-groupBy.Panel
#			if(is.null(ylab))
#				par$ylab<-statsType
			if(is.null(main))
				par$main<-paste(description(qaObj),curGroup,sep=":")	
			if(is.null(pch))
				par$pch<-"."	
			if(is.null(cex))
				par$cex<-5
			if(is.null(par.strip.text))
				par$par.strip.text<-list(lines=2)	
			if(is.null(scales))
				par$scales<-list(x=c(cex=0.9
											,rot=45	
											)
										)
									
			thisCall<-quote(bwplot(x=formula,data=yy
									,groupBy=groupBy.Panel
									,panel=function(data=yy,dest.=dest,plotObjs.=plotObjs,plotAll.=plotAll,...){
#										browser()
												panel.bwplotEx(data.=data,dest.=dest,plotObjs.=plotObjs,plotAll.=plotAll,db=db,...)
											}
									)
							)
			
		}
		#append the par list
		thisCall<-as.call(c(as.list(thisCall),par))
#		browser()
		thisCall<-eval(thisCall)
#		print(thisCall)
	}
	
#	browser()
	if(isSvg)
	{
		print(thisCall)
		ret<-dev.off()
		message("Saving to ",sfile)


		fileNames<-ls(plotObjs)
		
		if(!is.null(fileNames)&&plotAll!="none")
		{
#			browser()
#			t1<-Sys.time()
			for(dfile in fileNames)
			{
#				browser()
				curPlotObj<-get(dfile,plotObjs)
				#determine the pic size by the number of pannels
				nPanels<-length(curPlotObj$packet.sizes)
				wRatio<-ceiling(sqrt(nPanels))
				hRatio<-floor(sqrt(nPanels))
				png(file.path(dest,"individual",dfile),width=300*wRatio,height=300*hRatio)
				print(curPlotObj)
				dev.off()
				
			}
			
#			t2<-Sys.time()
			message("individual plots genetated.")
			
		}
		
#		browser()
		.postProcessSVG(sfile)
			
		return(basename(sfile))
	}else
	{
		thisCall
	}
	
	
	
}

