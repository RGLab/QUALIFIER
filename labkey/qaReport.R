library(QUALIFIER)

################################################################################  
#1.load db from labkey
#--------------------------------------------------------------------------------
#.db<-new.env()
#loadDB(.db,baseUrl="http://dhcp157039.fhcrc.org:8080/labkey", folderPath="/FlowGraph PROJECT",schemaName="qualifier")

################################################################################  
#qa report in html format 
#set plotAll=TRUE to generate the scatter plots for all the individual FCS files 
#otherwise only plots for outliers are generated.
#--------------------------------------------------------------------------------

##customerize some of the task before pass them to report method
htmlReport(qaTask.list[["MFIOverTime"]])<-TRUE
rFunc(qaTask.list[["MFIOverTime"]])<-rlm
qpar(qaTask.list[["MFIOverTime"]])<-list(horiz=FALSE,scales=list(x=NULL,y=NULL))
scatterPar(qaTask.list[["BoundaryEvents"]])<-list(type="xyplot",xlog=TRUE)
scatterPar(qaTask.list[["RedundantStain"]])<-list(type="xyplot",xlog=TRUE)
qpar(qaTask.list[["RedundantStain"]])<-list(horiz=FALSE
		,scales=list(x=list(relation="free"))
#											,layout=c(2,NA,1)
)
highlight(qaTask.list[["RBCLysis"]])<-"coresampleid"

outDir<-tempdir()
qaReport(qaTask.list[1],outDir=outDir,plotAll="none")

outDir