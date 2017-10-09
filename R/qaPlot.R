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
	
	pop <- unique(as.character(df$population))
	if(length(pop)>1)
		stop("not the same population in lattice group plot!")
#	browser()
	#extract flowFrame and gate from each gating hierarchy
	frlist <- apply(df,1,function(curRow){

		#get the parent population for the scatter plot
			
		gsid <- as.integer(curRow[["gsid"]])
		curGh <- db$gs[[gsid]][[curRow["name"]]]
		curNode <- as.character(curRow["node"])
	
		curGate <- getGate(curGh, curNode)
        
        curProp <- getProp(curGh, curNode, xml = FALSE)
        fr_pd <- pData(parameters(getData(curGh, use.exprs = FALSE)))
        thiscolnames <- fr_pd[, "name"]
#        browser()
        
        if(!extends(class(curGate),"filter")){
          if(statsType=="count"&&pop=="/root")##total cell count
          {
            param <- c("FSC-A", "SSC-A")  
          }else if(statsType=="spike"){
            chnl <- unique(as.character(df[,channel]))
            if(length(chnl)>1)
              stop("can't display multiple channels at a time!")
            
            timeChnl <- thiscolnames[grep("time",thiscolnames, ignore.case=T)]
            param <- c(timeChnl, chnl)
          }else{
            stop ("How do you end up to here?")
          }    
        }else
          param <- as.vector(parameters(curGate))
          if(length(param) == 1 && type == "xyplot"){
            sscChnl <- thiscolnames[grep("SSC",thiscolnames, ignore.case=T)]
            param <- c(param, sscChnl)
          }
            
        if(curNode == "root"){
          fr <- getData(curGh, j = param)
        }else{
          parentNode <- getParent(curGh, curNode)
          fr <- getData(curGh,parentNode, j = param)
        }
		list(frame = fr ,gate = curGate, stats = curProp, param = param)
	})
	names(frlist) <- df[["name"]]
	
	#merge frames into flowSet for flowViz plot
	fs1 <- flowSet(sapply(frlist,"[[","frame"))

	#extract gates from the list
	gates <- sapply(frlist,"[[","gate")
    stats <- sapply(frlist,"[[","stats", simplify = FALSE)
    param <- sapply(frlist,"[[","param", simplify = FALSE)[[1]]
#	browser()
	thisCall <- NULL
	if(statsType=="count"&&pop=="/root")##total cell count
	{
		##individual xyplot without gate
        t1 <- flowWorkspace:::mkformula(param)
		thisCall <- quote(xyplot(t1, data=fs1))
	}else
	{
					
			
		if(statsType=="spike")
		{
			
            gates <- NULL
            xterm <- as.symbol(param[1])
            yterm <- as.symbol(param[2])
            
            t1<-substitute(y~x,list(y=yterm,x=xterm))
            
			
		}else
		{
	

			if(type=="xyplot")
			{
				
				xterm <- as.symbol(param[1])
				yterm <- as.symbol(param[2])

				t1<-substitute(y~x,list(y=yterm,x=xterm))
				
			}else
			{
				xterm<-as.symbol(param[1])
				t1<-substitute(~x,list(x=xterm))
				yterm <- NULL
			}
		}
#		browser()
		t1 <- as.formula(t1)		
		#unfortunately	we have to manually transform the data here since flowViz does not take the scale argument
		if((is.logical(xlog)&&xlog)||!is.logical(xlog))
		{
			res <- reScaleData(fs1,gates,xterm,xlog)
		#			browser()
			fs1<-res$fs
            gates<-res$gates
		}
		if((is.logical(ylog)&&ylog)||!is.logical(ylog))
		{
			res<-reScaleData(fs1,gates,yterm,ylog)
		#			browser()
			fs1<-res$fs
            gates<-res$gates
		}
		
#		browser()
        if(!"outlier"%in%colnames(df))
        		outlier <- rep(FALSE, nrow(df))
        outlier <- df[, outlier]
        names(outlier) <- df[, name]	
        
		if(type == "xyplot")
			thisCall <- quote(
							xyplot(t1
									, fs1
									, filter = gates
                                    , stats = stats
									, outlier = outlier
									, panel = panel.xyplot.flowsetEx
									)
						)
	}
    thisCall <- as.call(c(as.list(thisCall),par))
    thisCall <- eval(thisCall)
	
	return(thisCall)
	
}


reScaleData<-function(fs,gates,channel,logScale)
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
		if(!is.null(gates))
          gates<-sapply(gates,function(curFilter){
						max <- curFilter@max
                        curFilter@max <- log(max,logbase)
						min <- curFilter@min
#						browser()
						#make sure to keep the the name of the scalar value in order for the flowViz plot properly
						min1<-max(0,log(min,logbase))
						names(min1) <- names(min)
                        curFilter@min <- min1
                        curFilter
					})
						
	}
#	browser()
	#and manually calculate the axis labels in order to preserve the raw scale labels
	rg.new<-range(eapply(fs@frames, range, as.character(channel)))
	ats<- seq(from=max(0,min(rg.new)),to=max(rg.new),length.out=5)
	labels<-round(logbase^ats)
	
	list(fs=fs,gates=gates,scales=list(labels=labels,at=ats))
}
#'plot the statistics for a particular cell population of a group of samples
#'
#'plot the statistics for a particular cell population of a group of
#'samples,this method is usually called after \code{qaCheck} to visualize the
#'QA results.
#'
#'The method does the same thing as \code{qaCheck} in terms of parsing the
#'formula and selecting the gated population,statistics and subsetting the
#'samples. The difference is that it reads the outliers detection results saved
#'in database and hightlight them in the summary plots. Two kinds of lattice
#'plots are currently supported:xyplot and bwplot(boxplot),depends on the
#'\code{plotType} in \code{qaTask} object. When the output path is provided by
#'\code{dest}, the svg plot is generated.  In svg plot, each dot or box (or
#'only the one marked as outliers) is annotated by the tooltip or
#'hyperlink.which further points to the individual density plot of the gated
#'population.
#'
#'with \code{scatterPlot} and \code{subset} arguments, scatter plots can be
#'generated for the selected FCS files or sample groups,which allows users to
#'investigate the individual outlier groups or files.
#'
#'@name plot-methods
#'@aliases plot plot,qaTask-method plot,qaTask,ANY-method
#'@docType methods
#'@param x a \code{qaTask} object
#'@param y a \code{formula} describing the variables to be used for plotting.
#'see \code{\link{qaCheck}} for more details.
#'@param ...  arguments to control the output.
#'
#'pop:a character scalar indicating the population name.If provided,it
#'overwrites the pop slot in qaTask object.
#'
#'
#'subset:a logical expression as a filter. see \code{\link{qaCheck}} for more
#'details.
#'
#'width,height:size specification for the svg output.
#'
#'dest: a character specifying the output path. It is NULL by default, which
#'indicates using the regular R device as the output.  Otherwise it outputs to
#'a svg file.
#'
#'plotAll: a logical/character scalar indicating whether to plot the 1D/2D
#'density plot for all the individual FCS files together with the summary
#'plot(either xyplot or bwplot).  It is only valid when \code{dest} is
#'specified as non-null path.  It is FALSE by default,indicating that only the
#'FCS files that are marked as outliers by \code{qaCheck} are plotted.  If
#'TRUE, all FCS files are plotted ,which should be used cautously since it
#'could be time consuming for a large dataset.  When it is "none",no scatter
#'plot will be generated.
#'
#'scatterPlot: a logical scalar. When TRUE, the density(scatter) plot is
#'plotted instead of the summary plot(xyplot/bwplot)
#'
#' scatterPar: A list storing all the fliwViz arguments. see \link[flowViz:xyplot]{xyplot}
#' 
#'par:A list storing all the lattice arguments.If provided,it overwrites the
#'par slot of qaTask object.
#'
#' outerStrip: a \code{logical} indicating whether to enable \link[latticeExtra:useOuterStrips]{useOuterStrips}
#' 
#' strip.lines,strip.left.lines: arguments passed to \link[latticeExtra:useOuterStrips]{useOuterStrips}  
#'@author Mike Jiang,Greg Finak
#'
#'Maintainer: Mike Jiang <wjiang2@@fhcrc.org>
#'@seealso \code{\link{qaCheck}},\code{\link[QUALIFIER:qaReport]{qaReport}}
#'@keywords methods
#'@examples
#'
#'
#'\dontrun{
#'
#'data("ITNQASTUDY")
#'checkListFile<-file.path(system.file("data",package="QUALIFIER"),"qaCheckList.csv.gz")
#'qaTask.list<-read.qaTask(db,checkListFile)
#'
#'#using formula to summing up the percentage of boundary events of each channel
#'#using the cutoff function to detect the FCS files that has the higher percentage of boundary events
#'#than the upper threshold provided by uBound
#'#Note that the percentages of all channels for each fcs file ("name" here indicates the fcs file name) 
#'#are summed up through the formula  
#'qaCheck(qaTask.list[["BoundaryEvents"]]
#'		,sum(proportion) ~ RecdDt | name
#'		,outlierfunc=outlier.cutoff
#'		,uBound=0.0003
#'		)
#'
#'plot(qaTask.list[["BoundaryEvents"]],proportion ~ RecdDt | channel)
#'
#'
#'
#'#using Interquartile Range based outlier detection function
#'#to find the outliers that has significant variance of MNC cell population among aliquots
#'#here the formula is implicitly provided by qaTask object
#'
#'qaCheck(qaTask.list[["MNC"]],outlierfunc=qoutlier,alpha=1.5)
#'
#'plot(qaTask.list[["MNC"]])
#'}
#'
#' @importFrom stats4 plot
#' @export 
setMethod("plot", signature=c(x="qaTask"),
		function(x, y, ...){
			
          plot.qaTask(qaObj=x,y = y, ...)
            
		})
#' @importFrom latticeExtra useOuterStrips
plot.qaTask <- function(qaObj,y,subset,pop,width,height
						,scatterPar=list()
						,dest = NULL,rFunc = NULL,plotAll = FALSE
						,scatterPlot = FALSE,gsid = NULL
						,horizontal = FALSE
                        ,panel = NULL
                        ,highlight
                        , between = list(x = 0.2,y = 0.2)
                        , axis= axis.grid
                        , outerStrip = FALSE 
                        , strip.lines = 2
                        , strip.left.lines = 3
						, ...)
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

	formuRes<-flowWorkspace:::.formulaParser(formula1)
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
    if(is.call(subset))
      res<-.queryStats(db,statsType=statsType,substitute(subset),pop=pop, gsid=gsid, type = qaObj@type)
	else
      res<-.queryStats(db,statsType=statsType,pop=pop,gsid=gsid, type = qaObj@type)
		
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
	#append the outlier flag
	res[, outlier := res$sid%in%base::subset(db$outlierResult,qaID==qaID(qaObj))$sid]
	res[, gOutlier := res$sid%in%base::subset(db$GroupOutlierResult,qaID==qaID(qaObj))$sid]
#	browser()

	res <- rename(res,c("value"=statsType))
	if(missing(highlight)){
      highlight <- qaObj@highlight
    }
    if(!highlight%in%colnames(res))
      stop(paste(highlight,"not found in the data"))
#	browser()	
	if(!is.null(dest))
	{	
		if(!file.exists(dest))
			stop(paste("folder '",dest,"' does not exist!",sep=""))
		sfile<-tempfile(pattern=getName(qaObj),tmpdir=dest,fileext=".svg")
#		browser()
		RSVGTipsDevice::devSVGTips(sfile,width=width,height=height)
		isSvg<-TRUE
	}else
	{
		isSvg<-FALSE
	}
	
	
#	browser()
	plotObjs <- new.env()
	if(scatterPlot)
			thisCall <- qa.GroupPlot(db=db,df=res,statsType=statsType,par=scatterP)
	else
	{#otherwise, plot the summary plot (either xyplot or bwplot)
				
		if(plotType(qaObj)=="xyplot")
		{
#			browser()
            if(is.null(panel)){
              panel <- panel.xyplot.qa
            }
			thisCall<-quote(
							xyplot(x = formula1
									,data = res
									,groups = outlier
									,panel = panel
									,df = res
									,dest = dest
									,plotObjs = plotObjs
									,plotAll = plotAll
									,statsType = statsType
									,db = db
									,scatterPar = scatterP
									,highlight = highlight
									,rFunc = rFunc
                                    ,between = between
                                    ,axis = axis
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
                                    ,between = between
                                    ,axis = axis
									)
							)
			
		}
		#append the par list
		thisCall <- as.call(c(as.list(thisCall),par))
#		browser()
        
		thisCall <- eval(thisCall)
        
        if(outerStrip){
          thisCall <-  eval(quote(useOuterStrips(thisCall
                                                 , strip.lines = strip.lines
                                                 , strip.left.lines = strip.left.lines
                                                  )
                                   )
                             )
        }
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

