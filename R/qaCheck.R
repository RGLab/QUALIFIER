# TODO: Add comment
# 
# Author: mike
###############################################################################
clearCheck<-function(obj,gsid)
{
#	browser()
	if(missing(gsid))
		gsid<-max(db$gstbl$gsid)
	db<-getData(obj)
	ind<-db$outlierResult$qaID%in%qaID(obj)&db$outlierResult$gsid%in%gsid
	db$outlierResult<-db$outlierResult[!ind,]
	
}
setMethod("qaCheck", signature=c(obj="qaTask"),
		function(obj,formula=NULL,subset,outlierfunc=NULL,gOutlierfunc=NULL,rFunc=NULL,isTerminal=TRUE,fixed=FALSE,...){
			
			call.f<-match.call(expand.dots = F)

			#replace subset with Subset
			ind<-which(names(call.f)=="subset")
			if(length(ind)>0)
			{
				names(call.f)[ind]<-"Subset"
			}

			argname<-names(list(...))
#			browser()		
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
				.qaCheck(obj,formula=formula,outlierfunc=outlierfunc,gOutlierfunc=gOutlierfunc,rFunc=rFunc,...)
			else
				.qaCheck(obj,formula=formula,Subset=substitute(subset),outlierfunc=outlierfunc,gOutlierfunc=gOutlierfunc,rFunc=rFunc,...)
			
			
		})

.qaCheck<-function(obj,formula=NULL,Subset,outlierfunc=NULL,gOutlierfunc=NULL,rFunc=NULL,isTerminal=TRUE,fixed=FALSE,gsid=NULL,...){
#	browser()

	qaID<-qaID(obj)
	db<-getData(obj)
	pop<-getPop(obj)
#	browser()
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
	##query db
	if(missing(Subset))
	{		
		yy<-.queryStats(db,statsType=statsType,pop=getPop(obj),isTerminal=isTerminal,fixed=fixed,gsid=gsid)
		
	}else
	{
		yy<-.queryStats(db,statsType=statsType,Subset,pop=getPop(obj),isTerminal=isTerminal,fixed=fixed,gsid=gsid)
		
	}
#		browser()	
	if(nrow(yy)==0)
	{
		return("empty subsets!")
	}
		
#	yy<-cast(yy,...~stats)
	yy<-reshape::rename(yy,c("value"=statsType))		
#	browser()
	##apply the function to xTerm and yTerm in each group
	if(!is.null(formuRes$xfunc))
		yy<-applyFunc(yy,as.character(formuRes$xTerm),formuRes$xfunc,formuRes$groupBy)
	if(!is.null(formuRes$yfunc))
		yy<-applyFunc(yy,as.character(formuRes$yTerm),formuRes$yfunc,formuRes$groupBy)
	
	
	factors<-lapply(groupBy,function(x){
				eval(substitute(factor(yy$v),list(v=x)))
			})
	
	##detect group outlier if boxplot
	groupOutSids<-NULL
	if(plotType(obj)=="bwplot")
	{
		##merge multipe conditioning variable to one to make a simply factor
		
		if(is.null(gOutlierfunc))
		{
			gOutlierfunc<-outlier.norm
			message("outlier.norm is used for group outlier detection.")
		}
		
		groupOutSids<-by(yy,factors,function(x){
											
#					browser()								
					
					curFactor<-as.factor(eval(substitute(x$v,list(v=as.character(xTerm)))))

					IQRs<-tapply(x[,statsType],curFactor,IQR)
					
					#log transform for between groups outlier call
#					browser()
					curGroupOutlier<-gOutlierfunc(log(IQRs),isLower=FALSE,...)
					
					curOutGroupID<-names(curGroupOutlier[curGroupOutlier])
					curOutSids<-x[curFactor%in%curOutGroupID,]$sid
					if(length(curOutSids)>0)
						curOutSids
					})
		groupOutSids<-unlist(groupOutSids)
	}
	
	
#	browser()
	
	##bwplot indicates the aditional grouping level by aliquot for individual outlier detection
	if(plotType(obj)=="bwplot")
	{
		
		groupBy<-c(groupBy,as.character(xTerm))
		factors<-lapply(groupBy,function(x){
					eval(substitute(yy$v,list(v=x)))
				})
	}	
			
	stats_list<-by(yy,factors
			,function(x){
#				browser()
				
				if(is.null(rFunc))
				{
					inputVec<-x[,statsType]
					
				}else
				{
					#using regression function to fit the data and 
					#calculate the outliers based on the residuals if applicable
					f1<-substitute(y~x,list(y=formuRes$yTerm,x=xTerm))
#					browser()
					regResult<-try(rFunc(as.formula(f1),x),silent=TRUE)
					if(all(class(regResult)!="try-error"))
					{
						inputVec<-regResult$residuals
					}else
					{
#						browser()
						inputVec<-x[,statsType]
					}
				}
#				browser()
				outlierVec<-outlierfunc(inputVec,...)
				
				x[outlierVec,]$sid
				
				
			})
	stats_list<-unlist(stats_list,use.names = FALSE)
#	browser()
	

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

	

