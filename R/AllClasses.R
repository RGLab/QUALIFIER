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
						,par="list"#arguments for lattice plot
						,scatterPar="list"#arguments for indivdiual plot
						,htmlReport="logical"#decide wether to plot even witout outliers detected
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
						,scatterPar=list(type="xyplot"
										,formula=NULL)
						,htmlReport=FALSE
						,db="ANY"
						)
		)

