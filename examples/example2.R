
library(QUALIFIER)
library(flowWorkspace)
unloadNamespace("QUALIFIER")
unloadNamespace("flowWorkspace")

localDir<-"~/rglab"
outDir<-file.path(localDir,"workspace/flowQA/output/ITN029_339")
dest<-file.path(outDir,"trellis_plot/")

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
getPopStats(gh_template)[,2:3]
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
getPopStats(G[[1]])[,2:3]

################################################################################  
#3.extract stats
###############################################################################
library(parallel)
qaPreprocess(gs=G,metaFile=metaFile,fcs.colname="FCS_Files",nslave=0)
################################################################################  
#4.load QA check list
###############################################################################
metaFile="~/rglab/workspace/QUALIFIER/misc/ITN029ST/FCS_File_mapping.csv"
checkListFile<-file.path(system.file("data",package="QUALIFIER"),"qaCheckList.csv.gz")
qaTask.list<-read.qaTask(checkListFile=checkListFile)

Rprof()
getQAStats(db$gs[[1]][1])
Rprof(NULL)
summaryRprof()

initDB()


##TODO: to change other parts adapting the change of schema of gs and stats table
## fix the other issues that ITN raises.

#read pre-determined events number for tubes from csv file
##pannel name should be in place of tube name since the entire package is using pannel name 
##to represent the tube

tubesEvents<-read.csv(file.path(system.file("data",package="QUALIFIER"),"tubesevents.csv.gz"),row.names=1)
tubesEventsOrig<-.TubeNameMapping(tubesEvents=tubesEvents[,3,drop=F])
tubesEvents20090825<-.TubeNameMapping(tubesEvents=tubesEvents[,2,drop=F])
tubesEvents20090622<-.TubeNameMapping(tubesEvents=tubesEvents[,1,drop=F])


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



plot(qaTask.list[["NumberOfEvents"]]
#		,Subset=Tube=='CD8/CD25/CD4/CD3/CD62L'
#,dest="image"
)

plot(qaTask.list[["NumberOfEvents"]]
		,subset=id=='245'
		,scatterPlot=TRUE
)
clearCheck(qaTask.list[["NumberOfEvents"]])


#addStats(db,definition=sum(proportion)~RecdDt|name
#			,statName="sum.prop"
#			,pop="margin"
##			,subset=population=="margin"
#			)

qaCheck(qaTask.list[["BoundaryEvents"]]
		,sum(proportion) ~ RecdDt | name
		,outlierfunc=outlier.cutoff
		,uBound=0.0003
)



plot(qaTask.list[["BoundaryEvents"]]
		,proportion ~ RecdDt |channel
#		,dest="image"
#		,subset=
#				channel=="PE-A"
#					&value>0
#					id==119
#		,par=list(ylab="percent")
#		,scatterPlot=T
#		,scatterPar=list(type="densityplot"
#						,scales=list(x=list(log=T))
#						)
##		,plotAll=F
)



## creating and showing the summary


qaCheck(qaTask.list[["MFIOverTime"]]
#		,outlierfunc=outlier.norm
		,rFunc=rlm
#		,Subset=channel%in%c('PE-Cy7-A')
		,z.cutoff=10
)
plot(qaTask.list[["MFIOverTime"]]
		,y=MFI~RecdDt|stain
		,subset=channel%in%c('PE-Cy7-A')
		,rFunc=rlm
#		,dest="image"

)
clearCheck(qaTask.list[["MFIOverTime"]])


qaCheck(qaTask.list[["RBCLysis"]]
		,outlierfunc=outlier.cutoff
		,lBound=0.9 #0.8
)
plot(qaTask.list[["RBCLysis"]]
#		,subset=Tube=='CD8/CD25/CD4/CD3/CD62L'
#		, RecdDt~proportion | Tube
#		,par=list(ylab="percent")
#		,horiz=T
		,dest="image"
		,highlight="coresampleid"
#	,plotAll="none"
)	


qaCheck(qaTask.list[["spike"]]
#		,outlierfunc=outlier.t
#		,z.cutoff=3
#		,alpha=0.001
)
plot(qaTask.list[["spike"]]
		,y=spike~RecdDt|channel
#		,subset=Tube=='CD11c/CD80/DUMP/HLADr/CD123'
#	,dest="image"
#	,plotAll=T
)

plot(qaTask.list[["spike"]],y=spike~RecdDt|channel
		,subset=channel=='FITC-A'
					&id%in%c(245,119)
		,scatterPlot=TRUE
#		,dest="image"
#		,plotAll="none"
)


qaCheck(qaTask.list[["MNC"]]
#		,Subset=coresampleid%in%c(11730,8780)
#		,z.cutoff=1
)

plot(qaTask.list[["MNC"]]
		,proportion~factor(coresampleid)|gsid
#		, factor(coresampleid)~proportion
#		,par=list(horiz=TRUE)

)

#scatter plot for a sample group	
plot(qaTask.list[["MNC"]]
		,proportion~factor(coresampleid)
#		,par=list(xlab="coresampleid")
#		, coresampleid ~proportion
#		,par=list(horiz=TRUE)
		,subset=coresampleid%in%c(11730
									,8780
		)
		,scatterPlot=TRUE
#		,dest="image"
#		,plotAll="none"
)
#scatter for one sample
plot(qaTask.list[["MNC"]]
		,scatterPlot=TRUE
		,subset=coresampleid==11730&id==245)

qaCheck(qaTask.list[["RedundantStain"]]
#			,gOutlierfunc=qoutlier
#			,outlierfunc=qoutlier
#			,alpha=1.5
#			,z.cutoff=2
)

##example of passing lattice arguments		
plot(qaTask.list[["RedundantStain"]]
		,subset=channel=='APC-A'&stain%in%c('CD123')
				&coresampleid==11730
		,y=proportion~factor(coresampleid)|channel:stain
		,scatterPlot=T
#		,dest="image"
)
################################################################################  
#4.qa report in html format 
#set plotAll=TRUE to generate the scatter plots for all the individual FCS files 
#otherwise only plots for outliers are generated.
###############################################################################

##customerize some of the task before pass them to report method
htmlReport(qaTask.list[["MFIOverTime"]])<-TRUE
rFunc(qaTask.list[["MFIOverTime"]])<-rlm
scatterPar(qaTask.list[["BoundaryEvents"]])<-list(type="densityplot",scales=list(x=list(log=TRUE)))
QUALIFIER:::scatterPar(qaTask.list[["RedundantStain"]])<-list(type="densityplot",scales=list(x=list(log=TRUE)))


qaReport(qaTask.list[[5]],outDir="~/rglab/workspace/QUALIFIER/output",plotAll="none")


