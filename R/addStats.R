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


#' this routine is currently use-case-specific for the marginal events
#' @param x a \code{qaTask} object
#' @param definition a \code{formula} that defines how the new stats is computed
#' @param pop a \code{character} that specifies the population name of the new stats
#' @param statName a \code{character} that specifies the name of new stats
addStats<-function(x,definition,pop,statName){
	formuRes<-.formulaParser(definition)
	db<-getData(x)
	statsType<-matchStatType(db,formuRes)
	
	
	df <- queryStats(x)
	
	if(!is.null(formuRes$yfunc))
	{
		groupBy <- paste0(formuRes$groupBy,collapse=",")
		df1 <- df[, eval(formuRes$yfunc)(value), by = groupBy]
        
	}else
		stop("no aggregation function provided!")
    
    df1[, value := V1]
    df1[, V1 := NULL]
	df1[,stats := statName]
	df1[,sid := 1:nrow(df1)+max(db$stats[,sid])]
	df1[, channel := as.character(NA)]
    df1[, stain := as.character(NA)]
	df1[, population := pop]
	df1[, node := as.character(NA)]
    
    
    setcolorder(df1,colnames(db$stats))
    
	db$stats <- rbindlist(list(db$stats, df1))
	
}


#
##this routine add new entries for the new statsType
#applyFunc2<-function(data,term,func,groupBy)
#{
#
#	factors<-lapply(groupBy,function(x){
#				
#				eval(substitute(data$v,list(v=x)))
#			})
#						
#			browser()	
#	by1<-paste(groupBy,collapse="+")
#	res<-eval(
#			substitute(aggregate(value~x,data=data,FUN=f),list(f=func,x=as.symbol(by1)))
#	)	
##	data<-by(data,factors,function(x){
###				browser()
##				eval(substitute(f(x$stats),list(f=func,stats=term)))
##				
##			})
##	do.call("rbind",data)
#	res
#}