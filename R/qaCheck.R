#' @description 
#' code{clearCheck} function removes the outlier results detected by the previous \code{qaCheck} call on a particular gating set.
#' 
#' @export 
#' @rdname qaCheck-methods
clearCheck <- function(obj, gsid)
{
#	browser()
	if(missing(gsid))
		gsid <- max(db$gstbl$gsid)
	db <- getData(obj)
	ind <- db$outlierResult$qaID%in%qaID(obj)&db$outlierResult$gsid%in%gsid
	db$outlierResult <- db$outlierResult[!ind,]
	
}
#'Perform the quality assessment for the qaTask object
#' @description 
#' Perform the quality assessment for a particular QA Task based on the
#' information provided by \code{\link{qaTask}} object.
#'
#' @details 
#'\code{qaCheck} method parses the formula stored in qaTask or explicitly
#'provided by the argument and select the appropriate gated population,extract
#'the statistics that is pre-calculated by \code{\link{getQAStats}} and perform
#'the outlier detection within a certain sample groups specified by the
#'conditioning variables or x term in formula. Then the outliers detection
#'results are save in database and ready for query or plotting.
#'
#'
#'@name qaCheck-methods
#'@aliases qaCheck qaCheck-methods qaCheck,qaTask-method clearCheck
#'@docType methods
#'@param obj a \code{qaTask} object
#'@param gsid an \code{integer} that uniquely identifies a gating set object.
#'if missing, the latest added gating set is selected.
#'@param ...  formula: a \code{formula} describing the variables to be used for
#'QA. When it is omitted or NULL, the formula stored in \code{qaTask} is used.
#'It is generally of the form y ~ x | g1 * g2 * ... , y is the statistics to be
#'checked in this QA, It must be one of the four types:
#'
#'"MFI": Median Fluorescence Intensity of the cell population specified by
#'\code{\link{qaTask}},
#'
#'"proportion": the percentage of the cell population specified by
#'\code{qaTask} in the parent population,
#'
#'"count": the number of events of the cell population specified by
#'\code{qaTask},
#'
#'"spike": the variance of intensity over time of each channel ,which
#'indicating the stability of the fluorescence intensity.
#'
#'x is normally used to specify the variable plotted on x-axis in
#'\code{\link[QUALIFIER:plot]{plot}} method.  when \code{plotType} of the
#'\code{qaTask} is "bwplot", it is also taken as the conditioning variable that
#'divides the samples into subgroups within which the \code{outlierfunc} is
#'applied.
#'
#'g1,g2,.... are the conditioning variables, which are used to divide the
#'samples into subgroups and perform QA check whitin each individual
#'groups.They may also be omitted,in which case the outliers detection is
#'peformed in the entire sample set.
#'
#'subset: a logical expression used as a filter.It follows the same syntax as
#'the "subset" expression in \code{\link[base:subset]{subset}}.
#'
#'\emph{Usage:}
#'
#'subset=channel\%in\%c('FITC-A')
#'
#'subset=Tube=='CD8/CD25/CD4/CD3/CD62L'&channel\%in\%c('FITC-A')
#'
#'outlierfunc:a \code{function} to be used for outlier detection.  see
#'\code{\link{outlierFunctions}} for more details.
#'
#'gOutlierfunc:a \code{function} to be used for group outlier detection.  see
#'\code{\link{outlierFunctions}} for more details.  rFunc:a \code{function} for
#'fitting regression model within each individual subgroup.
#'
#'isTerminal:a logical scalar indicating whether the pop is at terminal node of
#'the gating path.
#'
#'fixed:a logical scalar indicating whether the pop name is matched as it is
#'.By default it is FALSE,which matches the gating path as the regular
#'expression
#'@author Mike Jiang,Greg Finak
#'
#'Maintainer: Mike Jiang <wjiang2@@fhcrc.org>
#'@seealso \code{\link[QUALIFIER:plot]{plot}},\code{\link{getQAStats}}
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
#'
#'#using t-distribution based outlier detection function  
#'#applied the linear regression on each group to detect the significant MFI change over time 
#'qaCheck(qaTask.list[["MFIOverTime"]]
#'		,outlierfunc=outlier.t
#'		,rFunc=rlm
#'		,alpha=0.05
#')
#'plot(qaTask.list[["MFIOverTime"]],y=MFI~RecdDt|stain
#'		,subset="channel%in%c('FITC-A')"
#'		,rFunc=rlm
#')
#'
#'
#'#detect the outliers that has lower percentage of RBC Lysis than the threshold provided by lBound
#'qaCheck(qaTask.list[["RBCLysis"]]
#'		,formula=proportion ~ RecdDt | Tube
#'		,outlierfunc=outlier.cutoff
#'		,lBound=0.8
#'		)
#'		
#'plot(qaTask.list[["RBCLysis"]])	
#'}
#'
#' @rdname qaCheck-methods
#' @export 
setMethod("qaCheck", signature=c(obj="qaTask"),
    function(obj, ...){
      .qaCheck.main(obj,...)
    })

.qaCheck.main <- function(obj,formula = NULL, subset
                  , outlierfunc = NULL
                  , gOutlierfunc = NULL                
                   , rFunc = NULL, ...){
#			browser()
            #set outlier detection function
            if(is.null(outlierfunc))
              outlierfunc <- list(func = obj@outlierFunc
                                  , args = obj@outlierFunc_args)
                 
            if(is.null(gOutlierfunc))
              gOutlierfunc <- list(func = obj@goutlierFunc
                                  , args = obj@goutlierFunc_args)
                            
			call.f <- match.call(expand.dots = F)

			#replace subset with Subset
			ind <- which(names(call.f) == "subset")
			if(length(ind) > 0)
			{
				names(call.f)[ind] <- "Subset"
			}


            argname <- names(outlierfunc$args)
             
#			browser()		
			##try to run the qa for each individual cutoff value if multiple values are provided 
			##through lbound or rbound arugments
			if(!is.null(argname))
			{
				if(length(argname)==1&&grepl("[UuLl][Bb][Oo][Uu][Nn][Dd]",argname))
				{
					cutoff<-outlierfunc$args[[1]]
					
					#convert to named vector
					if(class(cutoff)=="data.frame")
					{
						
						cutoff<-apply(cutoff,1,"[",1)
					}else
					{
						if(class(cutoff)=="list")
						{
							cutoff<-unlist(cutoff)
						}
					}
					#if the cutoff is a multipe-value data structure
					#then try each individual cutoff for each conditioning value
					if(length(cutoff)>1)
					{
						formuRes<-flowWorkspace:::.formulaParser(formula)
						for(curConVal in names(cutoff))
						{
#						browser()
                            cur.outlierfunc <- outlierfunc
                            cur.outlierfunc[["args"]][[argname]] <- unname(cutoff[curConVal])
							cur.call.f<-call.f
                            
                        
							cur.call.f[[1]]<-quote(.qaCheck)
							cur.call.f$formula<-formula
                            
							cur.call.f$outlierfunc<-cur.outlierfunc
							cur.call.f$gOutlierfunc<-gOutlierfunc
							cur.call.f$rFunc<-rFunc
#							browser()
							cur.call.f$Subset<-substitute(x==y,list(x=formuRes$groupBy,y=curConVal))
							cur.call.f$Subset[[2]]<-as.symbol(cur.call.f$Subset[[2]])
							if(!missing(subset))
								cur.call.f$Subset<-as.call(list(as.symbol("&"),cur.call.f$Subset,substitute(subset)))
							
							cur.call.f$Subset<-as.call(list(quote(substitute),cur.call.f$Subset))
#							cur.call.f$...<-NULL
							#replace the cutoff value
#							eval(substitute(cur.call.f$v<-unname(cutoff[curConVal]),list(v=argname)))
#						browser()
							
							eval(cur.call.f)
							
						}
#						browser()
						return("done!")
					}
				}
				
			}	
				
#			browser()
			#if single scalar, then call the qacheck function directly
			if(missing(subset))
				.qaCheck(obj,formula=formula,outlierfunc=outlierfunc,gOutlierfunc=gOutlierfunc,rFunc=rFunc, ...)
			else
				.qaCheck(obj,formula=formula,Subset=substitute(subset),outlierfunc=outlierfunc,gOutlierfunc=gOutlierfunc,rFunc=rFunc, ...)
			
			
		}
        
# @importFrom reshape rename
.qaCheck <- function(obj,formula=NULL,Subset
                    ,outlierfunc
                    ,gOutlierfunc
                    ,rFunc=NULL,gsid=NULL,...){
	

	qaID<-qaID(obj)
	db<-getData(obj)
	pop<-getPop(obj)

	
	
	if(is.null(rFunc))
		rFunc<-rFunc(obj)	

#browser()
	if(is.null(formula))
	{
		formula<-getFormula(obj)
		
	}
	formuRes<-flowWorkspace:::.formulaParser(formula)
	xTerm<-formuRes$xTerm
	groupBy<-formuRes$groupBy
	
	statsType <- matchStatType(db,formuRes)
	
    if(missing(Subset))
      Subset <- obj@subset
    
	##query db
	if(is.call(Subset))
	{		
        yy <- .queryStats(db,statsType=statsType,Subset,pop=getPop(obj), gsid=gsid, type = obj@type)		
	}else
	{

      yy <- .queryStats(db,statsType=statsType,pop=getPop(obj),gsid=gsid, type = obj@type)		
	}
		
	if(nrow(yy)==0)
	{
		return("empty subsets!")
	}
		

	yy <- rename(yy,c("value"=statsType))		

	##apply the function to xTerm and yTerm in each group
	if(!is.null(formuRes$xfunc))
		yy<-applyFunc(yy,as.character(formuRes$xTerm),formuRes$xfunc,formuRes$groupBy)
	if(!is.null(formuRes$yfunc))
		yy<-applyFunc(yy,as.character(formuRes$yTerm),formuRes$yfunc,formuRes$groupBy)
	

    # outlier detection within groups (for bwplot) 
    .funcOutlierGrp <- function(x){
              #do IQR within each group
              IQR.Tbl <- x[,IQR(.SD[[statsType]]), by = xTerm]
#              browser()
              #log transform for between groups outlier call
              IQRs <- IQR.Tbl[,V1]
              curGroupOutlier <- do.call(gOutlierfunc$func ,c(list(x = log(IQRs))
                                                                ,c(isLower=FALSE
                                                                  ,gOutlierfunc$args
                                                                  )
                                                              )
                                          )
              
              curOutGroupID <- as.character(IQR.Tbl[curGroupOutlier, xTerm])
              #match to the respective sids of the detected group outliers
              if(length(curOutGroupID) > 0){
                mid <- match(curOutGroupID,x[[eval(as.character(xTerm))]])
                
                curOutSids <- x[mid,sid]
                if(length(curOutSids)>0)
                  curOutSids        
              }
    }
#    browser()
    ##detect group outlier if boxplot
	groupOutSids <- NULL
	if(plotType(obj) == "bwplot")
	{
        #do the xterm_group outlier detection within each conditional group
        if(is.null(groupBy))
          groupOutSids <- .funcOutlierGrp(yy)
        else{
          groupOut <- yy[,.funcOutlierGrp(.SD), by = groupBy]
          if(nrow(groupOut) > 0)
            groupOutSids <- groupOut[,V1]
        }
          
		
	}
	
	
    # individual outlier detection 
    .funcOutlier <-function(x){
      
      
      if(is.null(rFunc))
      {
        inputVec <- as.numeric(x[[statsType]])
        
      }else
      {
        #using regression function to fit the data and 
        #calculate the outliers based on the residuals if applicable
        f1<-substitute(y~x,list(y=formuRes$yTerm,x=xTerm))
#                   browser()
        regResult<-try(rFunc(as.formula(f1),x),silent=TRUE)
        if(all(class(regResult)!="try-error"))
        {
          inputVec<-regResult$residuals
        }else
        {

          inputVec <- as.numeric(x[[statsType]])
        }
      }
#                       browser()               
      outlierVec <- do.call(outlierfunc$func,c(list(x= inputVec)
                                                ,outlierfunc$args
                                                )
                                              
                            )
      
      x[outlierVec,sid]
      
      
    }
#	browser()
	##bwplot indicates the aditional grouping level by aliquot for individual outlier detection
	if(plotType(obj)=="bwplot")
	{
		groupBy <- c(groupBy,as.character(xTerm))
        outlierfunc <- list(func = qoutlier,args = list())
	}
    if(is.null(groupBy))
      stats_list <- .funcOutlier(yy)
    else
      stats_list <- yy[,.funcOutlier(.SD), by = groupBy][,V1]
	 
			

#	browser()
	
	##clean the old results
	ind<-db$outlierResult$sid%in%yy$sid&qaID==db$outlierResult$qaID
	db$outlierResult<-db$outlierResult[!ind,]
	ind<-db$GroupOutlierResult$sid%in%yy$sid&qaID==db$GroupOutlierResult$qaID
	db$GroupOutlierResult<-db$GroupOutlierResult[!ind,]
#	browser()
	#append the new one
	if(length(stats_list)>0)
	{
		ret<-cbind(sid=stats_list,qaID=qaID)
		db$outlierResult<-rbind(db$outlierResult,ret)
		ret
	}
	if(length(groupOutSids)>0)
	{
		#append the new one
		db$GroupOutlierResult<-rbind(db$GroupOutlierResult,cbind(sid=groupOutSids,qaID=qaID))

	}
	
	
	}

	

