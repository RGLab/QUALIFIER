############################################
#these are labkey db specific routines
#to be extended to more generic DB operations
############################################
loadStats<-function(db,...){

#	browser()
	db$stats<-labkey.selectRows(queryName="stats",...)
	db$stats$channel[db$stats$channel=="NA"]<-NA
	db$gstbl<-labkey.selectRows(queryName="gstbl",...)
				
}
loadDB<-function(db,...){
#	browser()
	db$qaTaskList<-labkey.selectRows(queryName="qaTaskList",...)	
	db$GroupOutlierResult<-labkey.selectRows(queryName="GroupOutlierResult",...)
	db$outlierResult<-labkey.selectRows(queryName="outlierResult",...)
	loadStats(db,...)	
					
}

writeTask<-function(db,...){
#	browser()
	sql<-"select max(qaID) as max_qaid from qatasklist"
	max_qaid <- labkey.executeSql(sql = sql,showHidden=TRUE,colNameOpt='caption',...)[1,]
	if(is.na(max_qaid))
		max_qaid<-0
	db$qaTaskTbl$qaID<-db$qaTaskTbl$qaID+max_qaid
	
	toInsert<-db$qaTaskTbl
	
	insertedRow <- labkey.insertRows(queryName="qatasklist",toInsert=toInsert,...)
}

writeGStbl<-function(db,...){
	##convert the sid to global one before append them to labkey db
	sql<-"select max(gsid) as max_gsid from gstbl"
	max_gsid <- labkey.executeSql(sql = sql,showHidden=TRUE,colNameOpt='caption',...)[1,]
	if(is.na(max_gsid))
		max_gsid<-0
	db$gstbl$sid<-db$gstbl$gsid+max_gsid
	
	toInsert<-db$gstbl
	toInsert$objlink="~/gatingSet"
	insertedRow <- labkey.insertRows(queryName="gstbl",toInsert=toInsert,...)
}

writeStats<-function(db,...){
	
	sql<-"select max(sid) as max_sid from stats"

	max_sid <- labkey.executeSql(sql = sql,showHidden=TRUE,colNameOpt='caption',...)[1,]
	if(is.na(max_sid))
		max_sid<-0
	
	db$stats$sid<-db$stats$sid+max_sid
	toInsert<-db$stats

	##insert new stats
	insertedRow <- labkey.insertRows(queryName="stats",toInsert=toInsert,...)
					
}

writeQAResults<-function(db,...){
	
#browser()
	##insert new QA results
	deletedRow<-labkey.deleteRows(queryName="outlierResult", toDelete=db$outlierResult,...)
	insertedRow <- labkey.insertRows(queryName="outlierResult", toInsert=db$outlierResult,...)
	deletedRow <- labkey.deleteRows(queryName="GroupOutlierResult", toDelete=db$GroupOutlierResult,...)
	insertedRow <- labkey.insertRows(queryName="GroupOutlierResult", toInsert=db$GroupOutlierResult,...)
}
writeProjections <- function(G,...){
	gh <- G[[1]];
	popNames <- getNodes( gh, isPath = T )
	nodeNames <- getNodes( gh )
	res <- lapply( 1:length(nodeNames), function(i) {
				curPop <- popNames[i]
				curpNode <- nodeNames[i]
				
				#store the children gate projections
				curChildrens <- getChildren(gh,curpNode)
				if( length( curChildrens ) > 0 )
				{
					prjlist <- lapply( curChildrens, function(curChildren) {
								g <- getGate( gh, curChildren )
								if ( class(g) == "BooleanGate" ){
									return( NULL );
								}else {
									param<-parameters(g)
									if(length(param)==1)
										param<-c(param,"SSC-A")
									return( param );
								}
							})
					prj <- do.call(rbind, prjlist)
					prj <- unique(prj)
					prj <- as.data.frame(prj)
					
					colnames(prj) <- c('x_axis', 'y_axis')
					cbind( name = curpNode, path = curPop, prj )
				} else {
					
				}
			})
	
	toInsert <- do.call( rbind, res )
	insertedRow <- labkey.insertRows( queryName = 'projections', toInsert = toInsert, ...)
}