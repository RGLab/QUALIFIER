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


##group plot for each sampleID or other aggregation ID
qa.GroupPlot<-function(db,df,statsType,par)
{
#browser()
	type<-par$type
	xlog<-par$xlog
	ylog<-par$ylog
	#remove from par list before pass to xyplot
	par$type<-NULL
	par$xlog<-NULL
	par$ylog<-NULL
	if(is.null(type))
		type<-"xyplot"
	if(is.null(xlog))
		xlog<-FALSE
	if(is.null(ylog))
		ylog<-FALSE
	
	pop<-unique(as.character(df$population))
	if(length(pop)>1)
		stop("not the same population in lattice group plot!")
#	browser()
	#extract flowFrame and gate from each gating hierarchy
	frlist<-apply(df,1,function(curRow){

		#get the parent population for the scatter plot
				
		gsid<-as.integer(curRow[["gsid"]])
		curGS<-db$gs[[gsid]]
		curSampleInd<-which(getSamples(curGS)%in%curRow["name"])
		curGh<-curGS[[curSampleInd]]
		curNode<-as.character(curRow["node"])
#				browser()
		
#		if(is.na(curNode))
#		{
#			curPop<-curRow["population"]
#			
#		}	
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
	names(frlist)<-df[,"name"]
#	browser()
	#merge frames into flowSet for flowViz plot
	fs1<-flowSet(lapply(frlist,"[[","frame"))
	#append outlier flags
	if(!"outlier"%in%colnames(df))
		df$outlier<-FALSE
	pData(fs1)$outlier<-df$outlier
	varMetadata(fs1)["outlier",]<-"outlier"

	#extract gates from the list
	gates<-lapply(frlist,"[[","gate")
#	browser()
	thisCall<-NULL
	if(statsType=="count"&&pop=="/root")##total cell count
	{
		##individual xyplot without gate
		thisCall<-xyplot(`SSC-A`~`FSC-A`,data=fs1,smooth=FALSE)
	}else
	{
					
			
		if(statsType=="spike")
		{
			chnl<-unique(as.character(df$channel))
			fres<-NULL
			if(length(chnl)>1)
				stop("can't display multiple channels at a time!")
			else
			yterm<-as.symbol(chnl)
			xterm<-as.symbol(colnames(fs1[[1]])[grep("time",colnames(fs1[[1]]),ignore.case=T)])
			t1<-substitute(y~x,list(y=yterm,x=xterm))
		}else
		{
	
			fres<-filter(fs1,gates)
			if(type=="xyplot")
			{
				
				if(length(parameters(fres[[1]]))==2)
				{
					xterm<-as.symbol(parameters(gates[[1]])[1])
					yterm<-as.symbol(parameters(gates[[1]])[2])
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
		}
#		browser()
		t1<-as.formula(t1)		
		#unfortunately	we have to manually transform the data here since flowViz does not take the scale argument
		if((is.logical(xlog)&&xlog)||!is.logical(xlog))
		{
			res<-reScaleData(fs1,fres,xterm,xlog)
		#			browser()
			fs1<-res$fs
			fres<-res$fres
		}
		if((is.logical(ylog)&&ylog)||!is.logical(ylog))
		{
			res<-reScaleData(fs1,fres,yterm,ylog)
		#			browser()
			fs1<-res$fs
			fres<-res$fres
		}
		
#		browser()

		if(type=="xyplot")
			thisCall<-quote(
							xyplot(t1
									,fs1
									,filter=fres
									,pd=pData(fs1)
									,panel=panel.xyplot.flowsetEx
									)
						)
		
		thisCall<-as.call(c(as.list(thisCall),par))
		thisCall<-eval(thisCall)			
	
	}
	
			
			
	
	return(thisCall)
	
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
		if(!is.null(fres))
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
		function(x, y, ...){
			
          plot.qaTask(qaObj=x,y = y, ...)
            
		})

plot.qaTask<-function(qaObj,y,subset,pop,width,height
						,scatterPar=list()
						,dest=NULL,rFunc=NULL,plotAll=FALSE
						,scatterPlot=FALSE,gsid=NULL,highlight='fileid'
						,horizontal=FALSE
                        ,panel = NULL
						,...)
{
#  browser()
  #assign null to formula if it is missing
  if(missing(y))
    formula1 <- getFormula(qaObj)
  else
    formula1 <- y
#	browser()
	par<-.db$lattice
	par<-lattice:::updateList(par,list(main=description(qaObj)))
	#overwrite par with par slot of the qa object
	par<-lattice:::updateList(par,qpar(qaObj))
	#overwrite par with whatever provided in ...
	par<-lattice:::updateList(par,list(...))
	

	scatterP<-scatterPar(qaObj)
#	browser()
	scatterP<-lattice:::updateList(scatterP,scatterPar)
	

	if(missing(width))
		width<-QUALIFIER:::width(qaObj)
	if(missing(height))
		height<-QUALIFIER:::height(qaObj)
    if(is.null(panel)){
      lattice.options(print.function=plot.trellisEx)  
    }  
	

	db<-getData(qaObj)
	##query db
	curGroup<-NULL
	
	if(is.null(qpar(qaObj)$horiz))
		qpar(qaObj)$horiz <- FALSE
	
	if(is.null(rFunc))
		rFunc<-rFunc(qaObj)
		
	
	#parse the formula

	formuRes<-.formulaParser(formula1)
	#decide the statsType(currently only one of the terms can be statType,we want to extend both in the future)
	
	statsType<-matchStatType(db,formuRes)
	if(missing(pop))
		pop<-getPop(qaObj)
		
#	browser()
    if(missing(subset))
      subset <- qaObj@subset
    subset <- substitute(subset)
    ##query db
#    browser()
    if(length(subset) == 0||is.na(subset))
    	res<-.queryStats(db,statsType=statsType,pop=pop,gsid=gsid, type = qaObj@type)
	else
		res<-.queryStats(db,statsType=statsType,substitute(subset),pop=pop, gsid=gsid, type = qaObj@type)
	if(nrow(res)==0)
	{
		return("no samples are matched!")
		
		
	}
	
#	browser()
	#check if the conditioning variable is of factor type
	for(curGroupBy in formuRes$groupBy)
	{

		curCol<-substitute(res$v,list(v=curGroupBy))
		if(!class(eval(curCol))=="factor")
		{
			eval(substitute(v<-as.factor(v),list(v=curCol)))
		}
	}
#	browser()
#	if(getName(qaObj)=="BoundaryEvents")
#		res<-base::subset(res,value>0)##filter out those zero-value records which may cause slow plotting
			
#	if(nrow(res)==0)
#	{
##		message()
#		return("no samples with the value>0 matched!")
#		
#	}
	#append the outlier flag
	res[, outlier := res$sid%in%base::subset(db$outlierResult,qaID==qaID(qaObj))$sid]
	res[, gOutlier := res$sid%in%base::subset(db$GroupOutlierResult,qaID==qaID(qaObj))$sid]
#	browser()

	res <- reshape::rename(res,c("value"=statsType))
	

	if(!highlight%in%colnames(res))
		stop(paste(highlight,"not found in the data"))
#	browser()	
	if(!is.null(dest))
	{	
		if(!file.exists(dest))
			stop(paste("folder '",dest,"' does not exist!",sep=""))
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
			thisCall<-qa.GroupPlot(db=db,df=res,statsType=statsType,par=scatterP)
	else
	{#otherwise, plot the summary plot (either xyplot or bwplot)
				
		if(plotType(qaObj)=="xyplot")
		{
#			browser()
            if(is.null(panel)){
              panel <- panel.xyplot.qa
            }
			thisCall<-quote(
							xyplot(x=formula1
									,data=res
									,groups=outlier
									,panel= panel
									,df=res
									,dest=dest
									,plotObjs=plotObjs
									,plotAll=plotAll
									,statsType=statsType
									,db=db
									,scatterPar=scatterP
									,highlight=highlight
									,rFunc=rFunc
								)
							)
		}
		
		if(plotType(qaObj)=="bwplot")
		{
			

			par <- lattice:::updateList(par,list(par.settings=list(plot.symbol=list(col="#E41A1C"
																					,pch=21
																					)
																	)
												)
										)
			
		
			if(horizontal)
			{
				groupBy.Panel<-as.character(formuRes$yTerm)#formula[[3]][[2]])
				
			}else
			{
				groupBy.Panel <- as.character(formuRes$xTerm)
		}

			
          if(is.null(panel)){
            panel <- panel.bwplotEx
          }

			thisCall<-quote(bwplot(x = formula1
									,data=res ##this argument does not get passed to panel function
									,panel=panel
									,df=res#arguments from this are passed to panel function
									,groupBy=groupBy.Panel
									,dest=dest
									,plotObjs=plotObjs
									,plotAll=plotAll
									,statsType=statsType
									,scatterPar=scatterP
                                    ,horizontal = horizontal
									,db=db
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

