############################################
#these are labkey db specific routines
#to be extended to more generic DB operations
############################################
loadStats <- function( db=.db, ... ){
	db$stats <- labkey.selectRows(
		  queryName 	= 'stats'
		, schemaName	= 'opencyto_quality_control'
		, ...
	);

	db$stats$channel[db$stats$channel=='NA'] <- NA;
};

loadDB <- function( db=.db, ... ){
	db$qaTaskList <- labkey.selectRows(
		  queryName 	= 'qaTaskList'
		, schemaName	= 'opencyto_quality_control'
		, ...
	);

	db$GroupOutlierResult <- labkey.selectRows(
		  queryName	= 'GroupOutlierResult'
		, schemaName	= 'opencyto_quality_control'
		, ...
	);

	db$outlierResult <- labkey.selectRows(
		  queryName	= 'outlierResult'
		, schemaName	= 'opencyto_quality_control'
		, ...
	);
	loadStats( db, ... );
};

writeTask <- function( db=.db, ... ){
	sql <- 'SELECT MAX(qaID) AS max_qaid FROM qatasklist';

	max_qaid <- labkey.executeSql(
		  sql 		= sql
		, showHidden	= T
		, colNameOpt	= 'caption'
		, schemaName	= 'opencyto_quality_control'
		, ...
	)[1,];

	if(is.na(max_qaid))
		max_qaid <- 0;

	db$qaTaskTbl$qaID <- db$qaTaskTbl$qaID + max_qaid;
	
	toInsert <- db$qaTaskTbl;
	
	insertedRow <- labkey.insertRows(
		  toInsert	= toInsert
		, queryName	= 'qaTaskList'
		, schemaName	= 'opencyto_quality_control'
		, ...
	);
};

writeStats <- function( db=.db, ... ){
	sql <- 'SELECT MAX(sid) AS max_sid FROM stats';

	max_sid <- labkey.executeSql(
		  sql 		= sql
		, showHidden 	= T
		, colNameOpt 	= 'caption'
		, schemaName	= 'opencyto_quality_control'
		, ...)[1,];

	if(is.na(max_sid))
		max_sid <- 0;
	
	db$stats$sid <- db$stats$sid + max_sid;
	toInsert <- db$stats;

	insertedRow <- labkey.insertRows(
		  toInsert	= toInsert
		, queryName 	= 'stats'
		, schemaName	= 'opencyto_quality_control'
		, ...
	);
};

writeQAResults <- function( db=.db, ... ){
	if ( nrow( db$outlierResult ) > 0 ){
		insertedRow	<- labkey.insertRows(
			  toInsert	= db$outlierResult
			, queryName	= 'outlierResult'
			, ...
		);
	}


	if ( nrow( db$GroupOutlierResult ) > 0 ){
		insertedRow	<- labkey.insertRows(
			  toInsert	= db$GroupOutlierResult
			, queryName	= 'GroupOutlierResult'
			, ...
		);
	}
};

