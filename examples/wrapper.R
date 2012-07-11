# preprocessing
###############################################################################
library(QUALIFIER)
library(flowWorkspace)

###############################################################################
#1.parse gating template
###############################################################################
ws<-openWorkspace("/loc/no-backup/mike/ITN029ST/QA_MFI_RBC_bounary_eventsV3.xml")
GT<-parseWorkspace(ws
		,name=2
		,execute=F
		,subset=1
		,useInternal=T
)
gh_template<-GT[[1]]					
###############################################################################
#2.apply gating template to new data
###############################################################################

datapath<-"/loc/no-backup/mike/ITN029ST/"
newSamples<-list.files(datapath)[1:500]
G<-GatingSet(gh_template
		,newSamples
		,path=datapath
		,isNcdf=FALSE
)


################################################################################  
#3.extract stats
###############################################################################
library(parallel)
db<-new.env()
initDB(db)
metaFile="~/rglab/workspace/QUALIFIER/misc/ITN029ST/FCS_File_mapping.csv"
#6 minutes for 500 samples
system.time(
qaPreprocess(db=db,gs=G
			,metaFile=metaFile
			,fcs.colname="FCS_Files"
			,date.colname=c("RecdDt","AnalysisDt")
			)
)
################################################################################  
#4.load QA check list
###############################################################################
setwd("QUALIFIER/output")

checkListFile<-file.path(system.file("data",package="QUALIFIER"),"qaCheckList.csv.gz")
qaTask.list<-read.qaTask(db,checkListFile=checkListFile)


################################################################################  
# 5 .QA check 
###############################################################################


qaCheck(qaTask.list[["BoundaryEvents"]]
		,sum(proportion) ~ RecdDt | name
		,outlierfunc=outlier.cutoff
		,uBound=0.0003
)



qaCheck(qaTask.list[["MFIOverTime"]]
#		,outlierfunc=outlier.norm
		,rFunc=rlm
#		,Subset=channel%in%c('PE-Cy7-A')
		,z.cutoff=10
)


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
#4.qa report in html format 
#set plotAll=TRUE to generate the scatter plots for all the individual FCS files 
#otherwise only plots for outliers are generated.
###############################################################################

##customerize some of the task before pass them to report method
htmlReport(qaTask.list[["MFIOverTime"]])<-TRUE
rFunc(qaTask.list[["MFIOverTime"]])<-rlm
scatterPar(qaTask.list[["BoundaryEvents"]])<-list(type="xyplot",xlog=TRUE)
scatterPar(qaTask.list[["RedundantStain"]])<-list(type="xyplot",xlog=TRUE)
qpar(qaTask.list[["RedundantStain"]])<-list(horiz=FALSE
		,scales=list(x=list(relation="free"))
#											,layout=c(2,NA,1)
)

undebug(QUALIFIER:::qaWrite.task)
qaReport(qaTask.list
		,outDir="~/rglab/workspace/QUALIFIER/output"
#		,plotAll="none"
)


