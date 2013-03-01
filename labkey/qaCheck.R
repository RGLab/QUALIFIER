library(QUALIFIER)

################################################################################  
#1.load db from labkey
#--------------------------------------------------------------------------------
#.db<-new.env()
#initDB(.db)
#loadStats(.db,baseUrl="http://dhcp157039.fhcrc.org:8080/labkey", folderPath="/FlowGraph PROJECT")

################################################################################  
#2.load QA check list
#--------------------------------------------------------------------------------

checkListFile<-file.path(system.file("data",package="QUALIFIER"),"qaCheckList.csv.gz")
qaTask.list<-read.qaTask(.db,checkListFile=checkListFile)

################################################################################  
#3.write QA check list to db
#--------------------------------------------------------------------------------

writeTask(.db,baseUrl="http://dhcp157039.fhcrc.org:8080/labkey", folderPath="/FlowGraph PROJECT")
################################################################################  
#4.outlier detection 
#--------------------------------------------------------------------------------


qaCheck(qaTask.list[["BoundaryEvents"]]
		,sum(proportion) ~ RecdDt | name
		,outlierfunc=outlier.cutoff
		,uBound=0.0003
)



#qaCheck(qaTask.list[["MFIOverTime"]]
##		,outlierfunc=outlier.norm
#		,rFunc=rlm
##		,Subset=channel%in%c('PE-Cy7-A')
#		,z.cutoff=10
#)


qaCheck(qaTask.list[["RBCLysis"]]
		,outlierfunc=outlier.cutoff
		,lBound=0.8
)


qaCheck(qaTask.list[["spike"]]
#		,outlierfunc=outlier.t
#		,z.cutoff=3
#		,alpha=0.001
)



qaCheck(qaTask.list[["MNC"]]
#		,Subset=coresampleid%in%c(11730,8780)
#		,z.cutoff=1
)





qaCheck(qaTask.list[["RedundantStain"]]
#			,gOutlierfunc=qoutlier
#			,outlierfunc=qoutlier
#			,alpha=1.5
#			,z.cutoff=2
)

##total number of events check
tubesEvents<-read.csv(file.path(system.file("data",package="QUALIFIER"),"tubesevents.csv.gz"),row.names=1)
tubesEventsOrig<-QUALIFIER:::.TubeNameMapping(db,tubesEvents=tubesEvents[,3,drop=F])
tubesEvents20090825<-QUALIFIER:::.TubeNameMapping(db,tubesEvents=tubesEvents[,2,drop=F])
tubesEvents20090622<-QUALIFIER:::.TubeNameMapping(db,tubesEvents=tubesEvents[,1,drop=F])


###80% of the pre-defined the value for each pannel
qaCheck(qaTask.list[["NumberOfEvents"]]
		,formula=count ~ RecdDt | Tube
		,outlierfunc=outlier.cutoff
		,lBound=0.8*tubesEvents20090825
		,subset=as.Date(RecdDt,"%m/%d/%y")>='2009-08-25'
)

qaCheck(qaTask.list[["NumberOfEvents"]]
		,formula=count ~ RecdDt | Tube
		,outlierfunc=outlier.cutoff
		,lBound=0.8*tubesEvents20090622
		,subset=as.Date(RecdDt,"%m/%d/%y")<'2009-08-25'&as.Date(RecdDt,"%m/%d/%y")>='2009-06-22'
)
qaCheck(qaTask.list[["NumberOfEvents"]]
		,formula=count ~ RecdDt | Tube
		,outlierfunc=outlier.cutoff
		,lBound=0.8*tubesEventsOrig
		,subset=as.Date(RecdDt,"%m/%d/%y")<'2009-06-22'
)

################################################################################  
#5.dump db to labkey
#--------------------------------------------------------------------------------
writeQAResults(.db,baseUrl="http://dhcp157039.fhcrc.org:8080/labkey", folderPath="/FlowGraph PROJECT")
