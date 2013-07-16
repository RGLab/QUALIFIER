## ---------------------------------------------------------------------------
## A container for the results of outliers test 
## ---------------------------------------------------------------------------
setClass("qaTask",
		representation(qaID="integer"
						,qaName="character"
						,description="character"
						,qaLevel="character"
						,pop="character"
						,formula="ANY"
                        ,type="character" # indicate how pop is matched
                        ,subset = "ANY"
						,plotType="character"
						,width="numeric"
						,height="numeric"
						,par="list"#arguments for lattice plot
						,scatterPar="list"#arguments for indivdiual plot
						,htmlReport="logical"#decide wether to plot even without outliers detected
						,rFunc="ANY"
						,highlight="character"#argument to determine level on which the dot will be higtlighted when hoverover in svg plot(like FCS,or sampleID ,should be the column name in the meta data) 
						,db="ANY"
						),
		prototype=list(qaID=integer(0)
						,qaName=character(0)
						,description=character(0)
						,qaLevel=character(0)
						,pop=character(0)
						,formula="ANY"
                        ,type = "popName"
                        ,subset = "ANY" 
						,plotType="xyplot"
						,width=10
						,height=10
						,par=list(horiz=FALSE
								,scales=list(format="%m/%d/%y")
								)
						,scatterPar=list(type="xyplot"
										,smooth=FALSE
										,stat=TRUE)
						,htmlReport=FALSE
						,rFunc=NULL
						,highlight="id"
						,db="ANY"
						)
		)

		

#TODO:to make constructor that record the qaTask into the db table
makeQaTask<-function(db=.db,qaName,description,qaLevel,pop,formula, type, plotType)
{
	qa<-new("qaTask"
						,qaID=max(db$qaChecklist)
						,qaName=qaName
						,description=description
						,qaLevel=qaLevel
						,pop=pop
						,formula=as.formula(formula)
                        ,type=type
						,plotType=plotType
						,db=db
				)
	
	qa
}

read.qaTask <-function(checkListFile, ...)
{
  df<-read.csv(checkListFile)
  .read.qaTask(df,...)
}

#' construct qatask from a data.frame
.read.qaTask<-function(df,db=.db)
{
	
	db$qaTaskTbl<-df
	qaTask.list<-apply(df,1,function(curRow,db){
#          browser()
          filter <- curRow["subset"]
          filter <- parse(text = filter)
				curQa<-new("qaTask"
						,qaID=as.integer(curRow["qaID"])
						,qaName=curRow["qaName"]
						,description=curRow["description"]
						,qaLevel=curRow["qaLevel"]
						,pop=curRow["pop"]
						,formula=as.formula(curRow["formula"])
                        ,type=curRow["type"]
                        ,subset = filter
						,plotType=curRow["plotType"]
						,db=db
				)
				curQa					
			},db)
	names(qaTask.list)<-df$qaName
	db$qaTaskList<-qaTask.list
	print(paste(nrow(df),"qaTask created ahd saved in db!"))
	qaTask.list
}
