# TODO: Add comment
# 
# Author: mike
###############################################################################

#setMethod("qaCheck", signature=c(x="qaTask"),
#		function(x,y,...){
#			if(missing(y))
#				y<-NULL
#			plot.qaTask(qaObj=x,formula=y,...)
#		})
#
#plot.qaTask<-function(qaObj,formula,subset=NULL,width=10,height=10,...)#,channel=NA,stain=NA,tube=NA
#{

setMethod("qaCheck", signature=c(obj="qaTask"),
		function(obj,formula=NULL,subset=NULL,outlierfunc,rFunc=NULL,...){
#	browser()

	qaID<-qaID(obj)
	db<-getData(obj)
	pop<-getPop(obj)
	
	if(missing(outlierfunc))
		stop("outlierfunc is missing!")
		

#browser()
	if(is.null(formula))
	{
		formula<-formula(obj)
		
	}
	formuRes<-.formulaParser(formula)
	xTerm<-formuRes$xTerm
	groupBy<-formuRes$groupBy
	
#	browser()
	##query db
	yy<-queryStats(db,formula,subset,pop)
		

	factors<-lapply(groupBy,function(x){
				eval(substitute(yy$v,list(v=x)))
			})
	
	##detect group outlier if boxplot
	groupOutSids<-NULL
	if(plotType(obj)=="bwplot")
	{
		##merge multipe conditioning variable to one to make a simply factor
		
#		factors<-as.factor(apply(yy[,groupBy],1,paste,collapse="_"))
		gOutlierfunc<-list(...)$gOutlierfunc
		if(is.null(gOutlierfunc))
			gOutlierfunc<-outlierfunc
		
		groupOutSids<-by(yy,factors,function(x){
											
					
					curFactor<-as.factor(eval(substitute(x$v,list(v=as.character(xTerm)))))

					IQRs<-tapply(x$value,curFactor,IQR)
#					browser()								
					
					#loop to detect outliers at each step by comparing the pvalue and the estimated variance (v>p)
#					ttt<-cochran.test(value~coresampleid,yy)
#					ttt
					
#		groupOutlier<-toutlier(log(IQRs),isLower=FALSE)
					#log transform for between groups outlier call
					curGroupOutlier<-gOutlierfunc(log(IQRs),isLower=FALSE)
					
					curOutGroupID<-names(curGroupOutlier[curGroupOutlier])
					curOutSids<-x[curFactor%in%curOutGroupID,]$sid
					if(length(curOutSids)>0)
						curOutSids
#		groupOutlier<-groupOutlier[which(groupOutlier)]	
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
					inputVec<-x$value
					
				}else
				{#using regression function to fit the data and calculate the outliers based on the residuals
					f1<-substitute(value~x,list(x=xTerm))
#					browser()
					regResult<-rFunc(as.formula(f1),x)
					inputVec<-regResult$residuals
				}
				
				outlierVec<-outlierfunc(inputVec,...)
				
				x[outlierVec,]$sid
				
				
			})
	stats_list<-unlist(stats_list,use.names = FALSE)
#	browser()
	
#	yy<-do.call("rbind",stats_list)
	
#	ret<-subset(yy,outlier==TRUE)
#	rownames(ret)<-NULL

	
	#remove old records
#	browser()
	
	##clean the old results
	db$outlierResult<-db$outlierResult[!db$outlierResult$qaID==qaID,]
	db$GroupOutlierResult<-db$GroupOutlierResult[!db$GroupOutlierResult$qaID==qaID,]
	
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
	
	
	})

	

