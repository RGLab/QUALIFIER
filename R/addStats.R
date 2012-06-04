#
################################################################################
##this method allow user using formula "definition" to add their own statsTypes 
##based on the four original stats(count/proportion/MFI/spike) and different conditioning/aggregation variables.
##and the QA check and plot can be done on this new stat type afterwards 
##unfinished:
################################################################################
#setMethod("addStats",signature(x="environment",definition="formula")
#					,function(x,definition,statName,subset,...){
#			browser()
#			
#			formuRes<-.formulaParser(definition)
#			statsType<-matchStatType(x,formuRes)
#			
#			
#			if(!missing(subset))
#				df<-queryStats(x,Subset=substitute(subset),statsType=statsType,...)
#			else
#				df<-queryStats(x,statsType=statsType,...)
#			
#			
#			
##			if(!is.null(formuRes$xfunc))
##				df<-applyFunc(df,as.character(formuRes$xTerm),formuRes$xfunc,formuRes$groupBy)
#			if(!is.null(formuRes$yfunc))
#			{
#				df1<-applyFunc2(df,as.character(formuRes$yTerm),formuRes$yfunc,formuRes$groupBy)
#			}
#				df<-df[,colnames(db$stats)]
#				
#			
#})
#
#
##this routine add new entries for the new statsType
##applyFunc2<-function(data,term,func,groupBy)
##{
##			browser()
##	factors<-lapply(groupBy,function(x){
##				
##				eval(substitute(data$v,list(v=x)))
##			})
##	#					browser()
##	
##	
##	res<-eval(
##			substitute(aggregate(value~x,data=data,FUN=f),list(f=func,x=as.symbol(groupBy)))
##	)	
##	data<-by(data,factors,function(x){
##				browser()
##				eval(substitute(f(x$stats),list(f=func,stats=term)))
##				
##			})
##	do.call("rbind",data)
##}