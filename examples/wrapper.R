# preprocessing
###############################################################################
library(QUALIFIER)

###############################################################################
#1.parse gating template
#--------------------------------------------------------------------------------
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
#--------------------------------------------------------------------------------

datapath<-"/loc/no-backup/mike/ITN029ST/"
newSamples<-list.files(datapath,pattern=".fcs")[1:1000]
G<-GatingSet(gh_template
		,newSamples
		,path=datapath
		,isNcdf=FALSE
)


################################################################################  
#3.extract stats
#--------------------------------------------------------------------------------
##meta info about FCS files such as pid,sampleID,visit number, staining panel(or Tube)
##not that pid is mandatory (to be displayed in tool tips of boxplot later (we may add
#extra argument to pass the colmns to tooltip instead of hardcoded the colnames)
metaFile="~/rglab/workspace/QUALIFIER/misc/ITN029ST/FCS_File_mapping.csv"


qaPreprocess(gs=G
			,metaFile=metaFile
			,fcs.colname="FCS_Files"
			,date.colname=c("RecdDt","AnalysisDt")
			)


################################################################################  
#4.load QA check list
#--------------------------------------------------------------------------------

checkListFile<-file.path(system.file("data",package="QUALIFIER"),"qaCheckList.csv.gz")
qaTask.list<-read.qaTask(checkListFile=checkListFile)


################################################################################  
# 5 .QA check 
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
#4.qa report in html format 
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


qaReport(qaTask.list
		,outDir="~/rglab/workspace/QUALIFIER/output"
		,plotAll=FALSE
)

################################################################################  
#5.interactive plot 
#--------------------------------------------------------------------------------

plot(qaTask.list[["NumberOfEvents"]]
#		,count ~ RecdDt
		,subset=Tube=='CD8/CD25/CD4/CD3/CD62L'
#,dest="image"
#		,scales=list(x=list(rot=45
#							,cex=0.5
#							))
#		,pch=19
)


plot(qaTask.list[["MFIOverTime"]]
		,y=MFI~RecdDt|stain
		,subset=channel%in%c('APC-A')
#				&stain=="CD123"
#				&id==806
		,rFunc=rlm
		,scales=list(format="%m/%d/%y")
#		,scatterPlot=TRUE
		,scatterPar=list(xlog=F)
#		,dest="image"
#		,plotAll="none"

)



plot(qaTask.list[["RBCLysis"]]
#		,subset=Tube=='CD8/CD25/CD4/CD3/CD62L'
#				&id%in%c(270)
#		, RecdDt~proportion | Tube
		,scales=list(format="%m/%d/%y")
		,ylab="percent"
#		,scatterPlot=T
		,scatterPar=list(stat=T
				,xbin=128)
#		,horiz=T
#		,dest="image"
		,highlight="coresampleid"
#		,plotAll="none"
		,width=27,height=13
)	




plot(qaTask.list[["spike"]]
		,y=spike~RecdDt|channel
		,subset=Tube=='CD11c/CD80/DUMP/HLADr/CD123'
#	,dest="image"
#	,plotAll=T
)

plot(qaTask.list[["spike"]],y=spike~RecdDt|channel
		,subset=channel=='FITC-A'
#					&id%in%c(245,119)
#		,scatterPlot=TRUE
		,scatterPar=list(ylog=T
				,xlim=c(0,100)
#						,xbin=128
		)
#		,dest="image"
#		,plotAll="none"
)


plot(qaTask.list[["MNC"]]
		,proportion~factor(coresampleid)
#		, factor(coresampleid)~proportion
#		,par=list(horiz=TRUE)
)

#scatter plot for a sample group	
plot(qaTask.list[["MNC"]]
		,proportion~factor(coresampleid)
#		,par=list(xlab="coresampleid")
#		, coresampleid ~proportion
#		,par=list(horiz=TRUE)
		,subset=coresampleid%in%c(
#									11730
				8780
		)
#		,scatterPlot=TRUE
		,scatterPar=list(xbin=128
				,stat=T)
		,dest="image"
		,plotAll=TRUE
)

#scatter for one sample
plot(qaTask.list[["MNC"]]
		,scatterPlot=TRUE
		,subset=coresampleid==11730&id==245)


##example of passing lattice arguments		
plot(qaTask.list[["RedundantStain"]]
		,subset=channel=='APC-A'
#				&stain%in%c('CD123','CD3')
#				&coresampleid==11730
		,y=proportion~factor(coresampleid)|channel:stain
#		,scatterPlot=T
		,scatterPar=list(xlog=F
				,stat=T
		)
		,scales=list(x=list(relation="free"))
#		,layout=c(2,NA,1)
#		,dest="image"
)

################################################################################  
#6.clear or query check results 
#--------------------------------------------------------------------------------
clearCheck(qaTask.list[["MNC"]])

clearCheck(qaTask.list[["MFIOverTime"]])

clearCheck(qaTask.list[["RBCLysis"]])

subset(
		queryStats(qaTask.list[["RBCLysis"]],subset=Tube=='CD8/CD25/CD4/CD3/CD62L')
		,outlier==TRUE
		)


################################################################################  
#7.add new aggregated stats 
#--------------------------------------------------------------------------------

QUALIFIER:::addStats(qaTask.list[["BoundaryEvents"]]
		,definition=sum(proportion)~RecdDt|id+gsid
		,pop="/root/MNC/margin"
		,statName="sum.prop")

head(subset(
				queryStats(qaTask.list[["BoundaryEvents"]]
						,y=sum.prop ~ RecdDt 
						,pop="margin"
						,subset=value>0
				)
				,outlier==TRUE)
)
#check on the new stats
qaCheck(qaTask.list[["BoundaryEvents"]]
		,sum.prop ~ RecdDt 
		,outlierfunc=outlier.cutoff
		,uBound=0.0003
)

plot(qaTask.list[["BoundaryEvents"]]
		,sum.prop ~ RecdDt 
#		,subset=channel=="PE-A"
#				&id==806
		,ylab="sum(percent)"
#		,scatterPlot=T
#		,scatterPar=list(
#						xlog=T
#						,stat=T
#						)
		,scales=list(format="%m/%d/%y")
#		,plotAll="none"
#		,dest="image"
)

