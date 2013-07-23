# TODO: Add comment
# 
# Author: mike
###############################################################################
clearCheck<-function(obj,gsid)
{
	db<-getData(obj)
	ind<-db$outlierResult$qaID%in%qaID(obj)&db$outlierResult$gsid%in%gsid
	db$outlierResult<-db$outlierResult[!ind,]
}
setMethod("qaCheck", signature=c(obj="qaTask"),
		function(obj,formula=NULL,subset
                  ,outlierfunc = list(func = outlier.norm,args = list())
                  ,gOutlierfunc=list(func = outlier.norm,args = list())                
                   ,rFunc=NULL, ...){
			
			call.f<-match.call(expand.dots = F)

			#replace subset with Subset
			ind<-which(names(call.f)=="subset")
			if(length(ind)>0)
			{
				names(call.f)[ind]<-"Subset"
			}

			argname<-names(list(...))
			##try to run the qa for each individual cutoff value if multiple values are provided 
			##through lbound or rbound arugments
			if(!is.null(argname))
			{
				if(length(argname)==1&&grepl("[UuLl][Bb][Oo][Uu][Nn][Dd]",argname))
				{
					cutoff<-list(...)[[1]]
					
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
						formuRes<-.formulaParser(formula)
						for(curConVal in names(cutoff))
						{
#						browser()
							cur.call.f<-call.f
							
							cur.call.f[[1]]<-quote(.qaCheck)
							cur.call.f$formula<-formula
							cur.call.f$outlierfunc<-outlierfunc
							cur.call.f$gOutlierfunc<-gOutlierfunc
							cur.call.f$rFunc<-rFunc
#							browser()
							cur.call.f$Subset<-substitute(x==y,list(x=formuRes$groupBy,y=curConVal))
							cur.call.f$Subset[[2]]<-as.symbol(cur.call.f$Subset[[2]])
							if(!missing(subset))
								cur.call.f$Subset<-as.call(list(as.symbol("&"),cur.call.f$Subset,substitute(subset)))
							
							cur.call.f$Subset<-as.call(list(quote(substitute),cur.call.f$Subset))
							cur.call.f$...<-NULL
							#replace the cutoff value
							eval(substitute(cur.call.f$v<-unname(cutoff[curConVal]),list(v=argname)))
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
			
			
		})
        

.qaCheck<-function(obj,formula=NULL,Subset
                    ,outlierfunc
                    ,gOutlierfunc
                    ,rFunc=NULL,gsid=NULL,...){
	

	qaID<-qaID(obj)
	db<-getData(obj)
	pop<-getPop(obj)

	if(is.null(outlierfunc))
	{
		if(plotType(obj)=="bwplot")
		{
			outlierfunc<-qoutlier
			message("qoutlier is used for outlier detection.")
		}else
		{
			outlierfunc<-outlier.norm
			message("outlier.norm is used for outlier detection.")
		}
	
	}
	
	if(is.null(rFunc))
		rFunc<-rFunc(obj)	

#browser()
	if(is.null(formula))
	{
		formula<-getFormula(obj)
		
	}
	formuRes<-.formulaParser(formula)
	xTerm<-formuRes$xTerm
	groupBy<-formuRes$groupBy
	
	statsType<-matchStatType(db,formuRes)
#	browser()
    if(missing(Subset))
      Subset <- obj@subset
    
	##query db
	if(length(Subset) == 0 || is.na(Subset))
	{		
		yy <- .queryStats(db,statsType=statsType,pop=getPop(obj),gsid=gsid, type = obj@type)
		
	}else
	{
		yy <- .queryStats(db,statsType=statsType,Subset,pop=getPop(obj), gsid=gsid, type = obj@type)
		
	}
		
	if(nrow(yy)==0)
	{
		return("empty subsets!")
	}
		

	yy<-reshape::rename(yy,c("value"=statsType))		

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
	groupOutSids<-NULL
	if(plotType(obj)=="bwplot")
	{
        #do the xterm_group outlier detection within each conditional group
        if(is.null(groupBy))
          groupOutSids <- .funcOutlierGrp(yy)
        else
          groupOutSids <- yy[,.funcOutlierGrp(.SD), by = groupBy][,V1]
		
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

	

