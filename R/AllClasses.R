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
						,plotType="character"
						,width="numeric"
						,height="numeric"
						,par="list"
						,db="ANY"
						
						),
		prototype=list(qaID=integer(0)
						,qaName=character(0)
						,description=character(0)
						,qaLevel=character(0)
						,pop=character(0)
						,formula="ANY"
						,plotType="xyplot"
						,width=numeric(10)
						,height=numeric(10)
						,par=list(horiz=FALSE)
						,db="ANY"
						)
		)

		

#TODO:to make constructor that record the qaTask into the db table
makeQaTask<-function(db,qaName,description,qaLevel,pop,formula,plotType)
{
	qa<-new("qaTask"
						,qaID=max(db$qaChecklist)
						,qaName=curRow["qaName"]
						,description=curRow["description"]
						,qaLevel=curRow["qaLevel"]
						,pop=curRow["pop"]
						,formula=as.formula(curRow["formula"])
						,plotType=curRow["plotType"]
						,db=db
				)
									
			
	print(paste("one qaTask created ahd saved in db!"))
	qa
}

read.qaTask<-function(db,checkListFile)
{
	qaCheckList<-read.csv(checkListFile)
	
	qaTask.list<-apply(qaCheckList,1,function(curRow,db){
#browser()			
				curQa<-new("qaTask"
						,qaID=as.integer(curRow["qaID"])
						,qaName=curRow["qaName"]
						,description=curRow["description"]
						,qaLevel=curRow["qaLevel"]
						,pop=curRow["pop"]
						,formula=as.formula(curRow["formula"])
						,plotType=curRow["plotType"]
						,db=db
				)
				curQa					
			},db)
	names(qaTask.list)<-qaCheckList$qaName
	db$qaCheckList<-qaTask.list
	print(paste(nrow(qaChecklist),"qaTask created ahd saved in db!"))
	qaTask.list
}