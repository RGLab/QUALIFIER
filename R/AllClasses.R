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
						,par=list(horiz=FALSE)
						,db="ANY"
						)
		)

