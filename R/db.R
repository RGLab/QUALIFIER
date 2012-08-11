
loadDB<-function(s){
	
	scobj <- getSchema(s, "qualifier")
	db <- new.env()
	db$GroupOutlierResult<-getRows(s,scobj$groupoutlierresult)
	db$outlierResult<-getRows(s,scobj$outlierresult)
	db$qaTaskList<-getRows(s,scobj$qatasklist)
	db$stats<-getRows(s,scobj$stats,colSelect="f")
	db$gstbl<-getRows(s,scobj$gstbl)
	db				
}

writeStats<-function(db,s){

	##convert the sid to global one before append them to labkey db
	sql<-"select max(sid) as max_sid from stats"

	max_sid <- labkey.executeSql(
			baseUrl="http://dhcp157039.fhcrc.org:8080/labkey",
			folderPath=getFolderPath(s),
			schemaName="qualifier",
			sql = sql)
	
	db$stats$sid<-db$stats$sid+max_sid
	
	##insert new stats
	insertedRow <- labkey.insertRows("http://dhcp157039.fhcrc.org:8080/labkey"
									, folderPath=getFolderPath(s)
										,schemaName="qualifier"
										,queryName="stats"
										, toInsert=db$stats
									)
					
}

writeQAResults<-function(db,s){
	

	##insert new QA results
	insertedRow <- labkey.insertRows("http://dhcp157039.fhcrc.org:8080/labkey"
									, folderPath=getFolderPath(s)
									,schemaName="qualifier"
									,queryName="outlierResult"
									, toInsert=db$outlierResult
									)
	insertedRow <- labkey.insertRows("http://dhcp157039.fhcrc.org:8080/labkey"
									, folderPath=getFolderPath(s)
									,schemaName="qualifier"
									,queryName="GroupOutlierResult"
									, toInsert=db$GroupOutlierResult
							)
}

