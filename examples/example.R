
library(QUALIFIER)
#library(flowWorkspace)
#unloadNamespace("QUALIFIER")
#unloadNamespace("flowWorkspace")
#lapply(list.files("/home/wjiang2/rglab/workspace/QUALIFIER/R",pattern=".R",full=T),source)

#outDir<-file.path(localDir,"workspace/flowQA/output/ITN029_339")
#dest<-file.path(outDir,"trellis_plot/")

###############################################################################
#1.parse gating template
###############################################################################
ws<-openWorkspace("/shared/silo_researcher/Gottardo_R/mike_working/ITN029ST/QA_template.xml")
GT<-parseWorkspace(ws
					,name=2
					,execute=F
					,subset=1
					)
gh_template<-GT[[1]]					
getPopStats(gh_template)[,2:3]

#test the matchNode

###############################################################################
#2.apply gating template to new data
###############################################################################
			
datapath<-"/shared/silo_researcher/Gottardo_R/mike_working/ITN029ST/"
#newSamples<-getSample(gh_template)
newSamples<-list.files(datapath)[1:200]
length(newSamples)
G<-GatingSet(gh_template
			,newSamples[1:30]
			,path=datapath
			,isNcdf=T
#			,dMode=4
			)
getPopStats(G[[1]])[,2:3]
plotGate(G[[1]],merge=F)
################################################################################  
#3.extract stats (6 min for 500 samples, 12 min for 1k samples)
###############################################################################
#library(parallel)
db<-new.env()
initDB(db)
metaFile="/shared/silo_researcher/Gottardo_R/mike_working/ITN029ST/FCS_File_mapping.csv"

qaPreprocess(db=db,gs=G
		,metaFile=metaFile
		,fcs.colname="FCS_Files"
		,date.colname=c("RecdDt","AnalysisDt")
		,nslave=1
#		,type="SOCK"
#		,isMFI=T,isSpike=T
)
#colnames(pData(db$gs[[1]]))
#pData(G)
#head(db$stats)
#getQAStats(G[1])

#saveToDB(db=db,gs=G
#		,metaFile=metaFile
#		,fcs.colname="FCS_Files"
#		,date.colname=c("RecdDt","AnalysisDt")
#	)
################################################################################  
#4.load QA check list
###############################################################################
checkListFile<-file.path(system.file("data",package="QUALIFIER"),"qaCheckList.csv.gz")
qaTask.list <- read.qaTask(db,checkListFile=checkListFile)


pData(db$gs[[1]])$RecdDt<-as.Date(pData(db$gs[[1]])$RecdDt)
QUALIFIER:::.parseTubeID(db)#parse TubeID from FCS filenames

#read pre-determined events number for tubes from csv file
##pannel name should be in place of tube name since the entire package is using pannel name 
##to represent the tube

tubesEvents<-read.csv(file.path(system.file("data",package="QUALIFIER"),"tubesevents.csv.gz"),row.names=1)
tubesEventsOrig<-QUALIFIER:::.TubeNameMapping(db,tubesEvents=tubesEvents[,3,drop=F])
tubesEvents20090825<-QUALIFIER:::.TubeNameMapping(db,tubesEvents=tubesEvents[,2,drop=F])
tubesEvents20090622<-QUALIFIER:::.TubeNameMapping(db,tubesEvents=tubesEvents[,1,drop=F])


###80% of the pre-defined the value for each pannel
qaCheck(qaTask.list[["NumberOfEvents"]]
		,formula=count ~ RecdDt | Tube
		,outlierfunc=list(func=outlier.cutoff,args=list(lBound=0.8*tubesEvents20090825))
		,subset=as.Date(RecdDt,"%m/%d/%y")>='2009-08-25'
)

qaCheck(qaTask.list[["NumberOfEvents"]]
		,formula=count ~ RecdDt | Tube
		,outlierfunc=list(func=outlier.cutoff,args=list(lBound=0.8*tubesEvents20090622))
		,subset=as.Date(RecdDt,"%m/%d/%y")<'2009-08-25'&as.Date(RecdDt,"%m/%d/%y")>='2009-06-22'
)
qaCheck(qaTask.list[["NumberOfEvents"]]
		,formula=count ~ RecdDt | Tube
		,outlierfunc=list(func=outlier.cutoff,args=list(lBound=0.8*tubesEventsOrig))
		,subset=as.Date(RecdDt,"%m/%d/%y")<'2009-06-22'
)

CairoX11()

plot(qaTask.list[["NumberOfEvents"]]
#		,count ~ RecdDt
		,subset=Tube=='CD8/CD25/CD4/CD3/CD62L'
#,dest="image"
#		,scales=list(x=list(rot=45
#							,cex=0.5
#							))
#		,pch=19
)

#qaTask.list[["NumberOfEvents"]]@formula<-count ~ `RecdDt` | `t/Tube`:`t/PTID`

plot(qaTask.list[["NumberOfEvents"]]
#		,subset=id=='245'
#		,scatterPlot=TRUE
#		,scatterPar=list(stat=T)
)
clearCheck(qaTask.list[["NumberOfEvents"]])


##add new aggregated stats
.addStats(qaTask.list[["BoundaryEvents"]]
		,definition=sum(proportion)~RecdDt|fileid+gsid
		,pop="/root/MNC/margin"
		,statName="sum.prop"
        )

head(subset(
				queryStats(qaTask.list[["BoundaryEvents"]]
							,y=sum.prop ~ RecdDt 
							,pop="margin"
#							,subset=value>0&id==806
							)
				,outlier==TRUE)
)
#check on the new stats
qaCheck(qaTask.list[["BoundaryEvents"]]
		,sum.prop ~ RecdDt 
		,outlierfunc= list(func = outlier.cutoff,args=list(uBound=0.0003))
		)

plot(qaTask.list[["BoundaryEvents"]]
		,sum.prop ~ RecdDt 
#		,subset=channel=="PE-A"
#				&id==806
#		,ylab="sum(percent)"
#		,scatterPlot=T
#		,scatterPar=list(
#						xlog=T
#						,stat=T
#						)
		,scales=list(format="%m/%d/%y")
#		,plotAll="none"
#		,dest="image"
)

#plot(qaTask.list[["BoundaryEvents"]]
#		,proportion ~ RecdDt|channel 
##		,subset=channel=="PE-A"
##				&id==806
#		,ylab="percent"
##		,scatterPlot=T
##		,scatterPar=list(
##						xlog=T
##						,stat=T
##						)
#		,scales=list(format="%m/%d/%y")
###		,plotAll=F
##		,dest="image"
#)

clearCheck(qaTask.list[["BoundaryEvents"]])
		
		
		
		
qaCheck(qaTask.list[["MFIOverTime"]]
#		,outlierfunc=outlier.norm
		,rFunc=rlm
#		,Subset=channel%in%c('PE-Cy7-A')
#		,z.cutoff=10
)

plot(qaTask.list[["MFIOverTime"]]
		,y=MFI~RecdDt|stain
		,subset=channel%in%c('APC-A')
#				&stain=="Va24"
#				&id==806
		,rFunc=rlm
		,scales=list(format="%m/%d/%y")
#		,scatterPlot=TRUE
		,scatterPar=list(xlog=F)
#		,dest="image"

)

clearCheck(qaTask.list[["MFIOverTime"]])


qaCheck(qaTask.list[["RBCLysis"]]
		,outlierfunc=list(func=outlier.cutoff,args=list(lBound=0.9))
)

subset(
		queryStats(qaTask.list[["RBCLysis"]]
					,subset=Tube=='CD8/CD25/CD4/CD3/CD62L')
		,outlier==TRUE)

plot(qaTask.list[["RBCLysis"]]
#		,subset=Tube=='CD8/CD25/CD4/CD3/CD62L'
#				&id%in%c(270)
#		, RecdDt~proportion | Tube
		,scales=list(format="%m/%d/%y")
		,ylab="percent"
#		,scatterPlot=T
#		,scatterPar=list(stat=T
#						,xbin=128)
#		,horiz=T
#		,dest="image"
		,highlight="coresampleid"
#		,plotAll="none"
		,width=27,height=13
)	

clearCheck(qaTask.list[["RBCLysis"]])


qaCheck(qaTask.list[["spike"]]
#		,outlierfunc=outlier.t
#		,z.cutoff=3
#		,alpha=0.001
#		,isLower=FALSE
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


qaCheck(qaTask.list[["MNC"]]
#		,Subset=coresampleid%in%c(11730,8780)
#		,z.cutoff=0.1
)

plot(qaTask.list[["MNC"]]
		,proportion~`coresampleid`
#		, factor(coresampleid)~proportion
#		,par=list(horiz=TRUE)
)

#scatter plot for a sample group	
plot(qaTask.list[["MNC"]]
		,proportion~factor(coresampleid)
#		,par=list(xlab="coresampleid")
#		, coresampleid ~proportion
#		,par=list(horiz=TRUE)
#		,subset=coresampleid%in%c(
##									11730
#									8780
#		)
#		,scatterPlot=TRUE
		,scatterPar=list(xbin=128
						,stat=T)
		,dest="~/rglab/workspace/QUALIFIER/output/image"
		,plotAll=F
)
#scatter for one sample
plot(qaTask.list[["MNC"]]
		,scatterPlot=TRUE
		,subset=coresampleid==11730&id==245)

clearCheck(qaTask.list[["MNC"]])

qaCheck(qaTask.list[["RedundantStain"]]
#			,gOutlierfunc=qoutlier
#			,outlierfunc=qoutlier
#			,alpha=1.5
#			,z.cutoff=2
)

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
#4.qa report in html format 
#set plotAll=TRUE to generate the scatter plots for all the individual FCS files 
#otherwise only plots for outliers are generated.
###############################################################################

##customerize some of the task before pass them to report method
htmlReport(qaTask.list[["MFIOverTime"]])<-TRUE
rFunc(qaTask.list[["MFIOverTime"]])<-rlm

highlight(qaTask.list[["BoundaryEvents"]])<-"coresampleid"
#scatterPar(qaTask.list[["MFIOverTime"]])<-list(xlog=TRUE)
#scatterPar(qaTask.list[["BoundaryEvents"]])<-list(xlog=TRUE)
#scatterPar(qaTask.list[["RedundantStain"]])<-list(xlog=TRUE)
qpar(qaTask.list[["RedundantStain"]])<-list(scales=list(x=list(relation="free")))




library(QUALIFIER)

#save_db(db1, path = "/home/wjiang2/rglab/workspace/QUALIFIER/output/preprocessedData", overwrite = T)
db <- load_db(path = "/home/wjiang2/rglab/workspace/QUALIFIER/output/preprocessedData")
qaTask.list <- db$qaTaskList


#modify functions within package namespace
funcToinsert <- "panel.xyplotEx" 
funcSym <- as.symbol(funcToinsert)
eval(substitute(environment(ff) <- getNamespace("QUALIFIER"), list(ff = funcSym)))
assignInNamespace(funcToinsert, eval(funcSym), ns = "QUALIFIER")


plot(qaTask.list[["RBCLysis"]]
    ,subset=Tube=='CD8/CD25/CD4/CD3/CD62L'
#				&id%in%c(270)
#		, RecdDt~proportion | Tube
    ,scales=list(format="%m/%d/%y")
    ,ylab="percent"
#		,scatterPlot=T
#		,scatterPar=list(stat=T
#						,xbin=128)
#		,horiz=T
    ,dest="~/rglab/workspace/QUALIFIER/output/image"
    ,highlight="coresampleid"
#		,plotAll="none"
#    ,width=27,height=13
)	


qaReport(qaTask.list["RBCLysis"]
		,outDir="~/rglab/workspace/QUALIFIER/output"
		,plotAll="none"
#		,subset=as.POSIXlt(RecdDt)$year==(2007-1900)
		)


