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
qa.singlePlot<-function(db,yy,statsType,par=list(type="xyplot"))
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
		individualPlot(x,curGate,curRow,statsType,par)
	}
#	}	
}
individualPlot<-function(x,curGate,curRow,statsType,par)
{
	cols <- colorRampPalette(IDPcolorRamp(21,
					t(col2hsv(c("blue","green","yellow","red"))),
					fr=c(0.7,0)))
	
	#get gate
#	statsType<-curRow$stat
	channel<-as.character(curRow$channel)
	pop<-as.character(curRow$population)

#browser()
	params<-colnames(x)

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
				yterm<-as.symbol(chnls[1])
				xterm<-as.symbol(params[grep("time",params,ignore.case=T)])
				t1<-substitute(y~x,list(y=yterm,x=xterm))
			}else
			{
				yterm<-as.symbol(params[2])
				xterm<-as.symbol(chnls[1])
				if(par$type=="xyplot")
					t1<-substitute(y~x,list(y=yterm,x=xterm))
				else
					t1<-substitute(~x,list(x=xterm))			
			}
		}else
		{
			yterm<-as.symbol(chnls[2])
			xterm<-as.symbol(chnls[1])
			t1<-substitute(y~x,list(y=yterm,x=xterm))
		}
				
		t1<-as.formula(t1)
#		browser()
		#convert frame to flowSet to plot
		fcsName<-as.character(curRow$name)
		if(!is.null(fres))
			fres@frameId<-fcsName			
		fs<-flowSet(x)
		sampleNames(fs)<-fcsName
		fres<-list(fres)
		names(fres)<-fcsName
		
		xlog<-par$scales$x$log
		if(is.null(xlog))xlog<-FALSE
#		ylog<-par$scales$y$log
#		if(is.null(ylog))ylog<-FALSE
#		browser()
		scales<-list()
		if((is.logical(xlog)&&xlog)||!is.logical(xlog))
		{
			res<-reScaleData(fs,fres,xterm,xlog)
#			browser()
			fs<-res$fs
			fres<-res$fres
			scales$x<-res$scales
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
	if(par$type=="xyplot")
		xyplot(x=t1
				,data=fs
				,smooth=FALSE
				,colramp=cols
				,filter=fres
				,names=FALSE
				,scales=scales
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
				,panel=panel.xyplot.flowsetEx
		)
	else
	{
		
		densityplot(data=fs
				,x=t1
				,smooth=FALSE
				,filter=fres
				,names=FALSE
				,scales=scales
				,main=mainTitle
				,panel=qa.panel.densityplot
		)	
	}	

	}

}
#TODO:check why colnames from flowCore is not dispatched correctly without namespace explicitly specified
##group plot for each sampleID or other aggregation ID
qa.GroupPlot<-function(db,yy,par=list(type="xyplot"))
{
	cols <- colorRampPalette(IDPcolorRamp(21,
					t(col2hsv(c("blue","green","yellow","red"))),
					fr=c(0.7,0)))

	pop<-unique(as.character(yy$population))
	if(length(pop)>1)
		stop("not the same population in lattice group plot!")
#	browser()
	#extract flowFrame and gate from each gating hierarchy
	frlist<-apply(yy,1,function(curRow){
#		browser()
		#get the parent population for the scatter plot
		
		curSampleInd<-which(getSamples(db$G)%in%curRow["name"])
		curGh<-db$G[[curSampleInd]]
		curNode<-as.character(curRow["node"])
		curGate<-getGate(curGh,curNode)
		parentNode<-getParent(curGh,curNode)
		parentNodeInd<-which(getNodes(curGh)%in%parentNode)
		if(length(parentNodeInd)>0)
		{
			fr<-getData(curGh,parentNodeInd)
		}else
		{
			fr<-getData(curGh)
		}
		list(frame=fr,gate=curGate)
	})
	names(frlist)<-yy[,"name"]
	
	#merge frames into flowSet for flowViz plot
	fs1<-flowSet(lapply(frlist,"[[","frame"))
	#append outlier flags
	if(!"outlier"%in%colnames(yy))
		yy$outlier<-FALSE
	pData(fs1)$outlier<-yy$outlier
	varMetadata(fs1)["outlier",]<-"outlier"

	#extract gates from the list
	gates<-lapply(frlist,"[[","gate")
	
	obj<-NULL
	if(!pop=="/root")##total cell count
	{
		
	
		fres<-filter(fs1,gates)
		if(par$type=="xyplot")
		{
			
			if(length(parameters(fres[[1]]))==2)
			{
				xterm<-as.symbol(parameters(gates[[1]])[2])
				yterm<-as.symbol(parameters(gates[[1]])[1])
			}else
			{
				xterm<-as.symbol(parameters(gates[[1]])[1])
				yterm<-as.symbol(flowCore::colnames(fs1)[grep("SSC",flowCore::colnames(fs1))])
			}
			t1<-substitute(y~x,list(y=yterm,x=xterm))
			
		}else
		{
			xterm<-as.symbol(parameters(fres[[1]])[1])
			t1<-substitute(~x,list(x=xterm))
			yterm<-NULL
		}
		t1<-as.formula(t1)		
		#unfortunately	we have to manually transform the data here since flowViz does not take the scale argument
		xlog<-par$scales$x$log
		if(is.null(xlog))xlog<-FALSE
#		ylog<-par$scales$y$log
#		if(is.null(ylog))ylog<-FALSE
		
#		browser()
		scales<-list()
		if((is.logical(xlog)&&xlog)||!is.logical(xlog))
		{
			res<-reScaleData(fs1,fres,xterm,xlog)
#			browser()
			fs1<-res$fs
			fres<-res$fres
			scales$x<-res$scales
		}
#		if((is.logical(ylog)&&ylog)||!is.logical(ylog))
#			res<-reScaleData(fs1,fres,yterm,ylog)
		
#		browser()
		if(par$type=="xyplot")
			obj<-xyplot(x=t1,
						data=fs1,
						smooth=FALSE,
						colramp=cols,
						filter=fres,
						names=FALSE,
						pd=pData(fs1)
						,scales=scales
						,par.settings=list(gate.text=list(text=0.7
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
		else
		{
			
			obj<-densityplot(data=fs1
								,x=t1
								,smooth=FALSE
								,filter=fres
								,names=FALSE
								,pd=pData(fs1)
								,scales=scales
								,panel=qa.panel.densityplot
							)
		}
	}
	
			
			
	
	return(obj)
	
}


reScaleData<-function(fs,fres,channel,logScale)
{
	#set the logbase if neccessary
	if(!is.logical(logScale))
	{
		logbase<-logScale
		logScale<-TRUE
	}else
	{
		if(logScale)logbase<-10
	}
	
#	browser()
	if(logScale)
	{
#		logTrans <- eval(substitute(logTransform(transformationId="log-transformation", logbase=v, r=1, d=1),list(v=logbase)))
		#somhow transform on logTrans doesn't work with different base,due to the way transform function handle the expression parsing
		fs <- eval(parse(text=paste("flowCore::transform(fs,`",channel,"`=log(`",channel,"`,base=",logbase,"))",sep="")))

		#log transform the filter result
		fres<-lapply(fres,function(curFres){
						max<-curFres@filterDetails[[1]]$filter@max
						curFres@filterDetails[[1]]$filter@max<-log(max,logbase)
						min<-curFres@filterDetails[[1]]$filter@min
#						browser()
						#make sure to keep the the name of the scalar value in order for the flowViz plot properly
						min1<-max(0,log(min,logbase))
						names(min1)<-names(min)
						curFres@filterDetails[[1]]$filter@min<-min1
						curFres
					})
						
	}
#	browser()
	#and manually calculate the axis labels in order to preserve the raw scale labels
	rg.new<-range(eapply(fs@frames, range, as.character(channel)))
	ats<- seq(from=max(0,min(rg.new)),to=max(rg.new),length.out=5)
	labels<-round(logbase^ats)
	
	list(fs=fs,fres=fres,scales=list(labels=labels,at=ats))
}

setMethod("plot", signature=c(x="qaTask"),
		function(x,y,...){
#browser()
			#assign null to formula if it is missing
			if(missing(y))
				y<-getFormula(x)
			plot.qaTask(qaObj=x,formula1=y,...)
		})

plot.qaTask<-function(qaObj,formula1,subset,pop,width,height,par,scatterPar,isTerminal=TRUE,fixed=FALSE,dest=NULL,rFunc=NULL,plotAll=FALSE,scatterPlot=FALSE)
{
#	browser()
	par_old<-qpar(qaObj)
	if(!missing(par))##overwrite the elements of par slot of qa object if provided by argument
	{
		for(x in names(par))
			eval(substitute(par_old$v<-par$v,list(v=x)))
				
		qpar(qaObj)<-par_old
	}
	
#	browser()
	par_old<-QUALIFIER:::scatterPar(qaObj)
	if(!missing(scatterPar))##overwrite the elements of par slot of qa object if provided by argument
	{
		for(x in names(scatterPar))
			eval(substitute(par_old$v<-scatterPar$v,list(v=x)))
		
		QUALIFIER:::scatterPar(qaObj)<-par_old
	}
	
#	browser()
	if(missing(width))
		width<-QUALIFIER:::width(qaObj)
	if(missing(height))
		height<-QUALIFIER:::height(qaObj)
	lattice.options(print.function=plot.trellisEx)
#	browser()
	db<-getData(qaObj)
	##query db
	curGroup<-NULL
#	browser()
	
	if(is.null(qpar(qaObj)$horiz))
		qpar(qaObj)$horiz<-FALSE
	
	if(is.null(rFunc))
		rFunc<-rFunc(qaObj)
	
	#parse the formula
	formuRes<-.formulaParser(formula1)
	#decide the statsType(currently only one of the terms can be statType,we want to extend both in the future)
	
	statsType<-matchStatType(db,formuRes)
	if(missing(pop))
		pop<-getPop(qaObj)
#		browser()
	
	if(missing(subset))
		yy<-queryStats(db,statsType=statsType,pop=pop,isTerminal=isTerminal,fixed=fixed)
	else
		yy<-queryStats(db,statsType=statsType,substitute(subset),pop=pop,isTerminal=isTerminal,fixed=fixed)
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
#	browser()
	if(getName(qaObj)=="BoundaryEvents")
		yy<-base::subset(yy,value>0)##filter out those zero-value records which may cause slow plotting
			
	if(nrow(yy)==0)
	{
		message("no samples are matched!")
		return()
		
	}
	#append the outlier flag
	yy$outlier<-yy$sid%in%base::subset(db$outlierResult,qaID==qaID(qaObj))$sid
	yy$gOutlier<-yy$sid%in%base::subset(db$GroupOutlierResult,qaID==qaID(qaObj))$sid
	
	#reshape the data to include the column of the statType which can be passed to lattice	as it is
	yy<-as.data.frame(cast(yy,...~stats))

#	dest=list(...)$dest

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
	
	
#	browser()
	plotObjs=new.env()
	if(scatterPlot)
	{
		##if scatterPlot flag is true then just plot the scatter plot
		
		if(statsType=="spike"||(statsType=="count"&&yy$population[1]=="/root"))
		{
#			browser()
			thisCall<-lapply(1:nrow(yy),function(i)
			{
#						browser()
						qa.singlePlot(db,yy[i,,drop=FALSE],statsType,QUALIFIER:::scatterPar(qaObj))
			})
			
			
		}else
		{
			thisCall<-qa.GroupPlot(db,yy,QUALIFIER:::scatterPar(qaObj))
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
#browser()
			thisCall<-quote(
							xyplot(x=formula1,data=yy
									,groups=outlier
									,panel=function(x=x,y=y,data=yy,dest.=dest,plotObjs.=plotObjs,plotAll.=plotAll,statsType.=statsType
													,scatterPar=QUALIFIER:::scatterPar(qaObj)
													,...){
#												browser()
												panel.xyplotEx(x,y,data=data,dest.=dest,plotObjs.=plotObjs,plotAll.=plotAll
																,statsType.=statsType,db=db
																,scatterPar=scatterPar
																,...)
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
			
#		lattice.options(print.function=plot.trellisEx)
	
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
									
			thisCall<-quote(bwplot(x=formula1,data=yy
									,groupBy=groupBy.Panel
									,panel=function(data=yy,dest.=dest,plotObjs.=plotObjs
														,plotAll.=plotAll
														,statsType.=statsType,scatterPar=QUALIFIER:::scatterPar(qaObj)
														,...){
#										browser()
											panel.bwplotEx(data.=data,dest.=dest
															,plotObjs.=plotObjs,plotAll.=plotAll
															,statsType.=statsType,scatterPar=scatterPar
															,db=db,...)
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

