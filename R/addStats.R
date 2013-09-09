#' this routine is currently use-case-specific for the marginal events
#' 
#' @param x a \code{qaTask} object
#' @param definition a \code{formula} that defines how the new stats is computed
#' @param pop a \code{character} that specifies the population name of the new stats
#' @param statName a \code{character} that specifies the name of new stats
addStats <- function(x, definition, pop, statName){
	
    # subset the data by qaTask
	DT <- queryStats(x)
    
    #parse the function from definition
    formuRes <- flowWorkspace:::.formulaParser(definition)
	if(!is.null(formuRes$yfunc))
	{
        
        groupBy <- paste0(formuRes$groupBy,collapse=",")
        
        db <- getData(x)
        
        #apply the function  
		DT1 <- DT[, list(value = eval(formuRes$yfunc)(value)), by = groupBy]
        
        #add other columns
        DT1[,stats := statName]
        DT1[,sid := 1:nrow(DT1)+max(db$stats[,sid])]
        DT1[, channel := as.character(NA)]
        DT1[, stain := as.character(NA)]
        DT1[, population := pop]
        DT1[, node := as.character(NA)]
        
        #reorder the columns to prepare for rbindlist
        colNames <- colnames(db$stats)        
        setcolorder(DT1, colNames)
        
        #append new rows to db table
        db$stats <- rbindlist(list(db$stats, DT1))    
	}else
		stop("no aggregation function provided!")
    
	
}

