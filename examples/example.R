# TODO: Add comment
# 
# Author: mike
###############################################################################
#unloadNamespace("flowQA")
library(QUALIFIER)
library(Rmpi)
library(multicore)

library(ncdfFlow)
library(flowWorkspace)
#unloadNamespace("flowWorkspace")
#unloadNamespace("ncdfFlow")

localDir<-"~/rglab"
outDir<-file.path(localDir,"workspace/flowQA/output/ITN029_339")
dest<-file.path(outDir,"trellis_plot/")

###read annotation data
metaFile="~/rglab/workspace/QUALIFIER/misc/ITN029ST/FCS_File_mapping.csv"
anno<-read.csv(metaFile)


################################################################################  
#1.parse QA flowJo workspace into R
#Note that this step is most time consuming especially for large datasets
#it is convienient to save the gatingset once it is done 
#so that it be loaded directly from disk later on for the further processing 
###############################################################################
ws<-openWorkspace("~/rglab/workspace/QUALIFIER/misc/ITN029ST/QA_MFI_RBC_bounary_eventsV3.xml")


#ncfs<-read.ncdfFlowSet(files =file.path(ws@path
#												,c(flowWorkspace:::getFileNames(ws)[1:10]
##													,"20110125240_F06_I025.fcs"
#													)
#												)
#						,isWriteSlice = F)
#addFrame(ncfs
#		,read.FCS("/home/wjiang2/rglab/workspace/flowQA/misc/ITN029ST/20110125240_F06_I025.fcs")
#		,"01107121_F01_I010.fcs")
#flowCore:::read.FCSheader("/home/wjiang2/rglab/workspace/flowQA/misc/ITN029ST/20110125240_F06_I025.fcs")
#ncfs[["01107121_F01_I010.fcs"]]

time1<-Sys.time()
##filter samples by anno data 
subsetID<-flowWorkspace::getFJWSubsetIndices(ws,key="$FIL",value=as.character(anno$FCS_Files),group=2)
##parse the workspace with the subset
G<-parseWorkspace(ws,execute=T,isNcdf=T,name=1,nslaves=2,subset=subsetID)
Sys.time()-time1
#save the gating results
saveNcdf("G","gatingHierarchy")
save(G,file="gatingHierarchy/GS.Rda")


################################################################################  
#2.load metadata for QA and extract cell counts,percentage and MFI
###############################################################################
load(file="gatingHierarchy/GS.Rda")
db<-new.env()##using environment to mimic a database connection
saveToDB(db,G,anno)##append the annotation  and Gating set to db 
time1<-Sys.time()
getQAStats(db,nslaves=10)
Sys.time()-time1
#
save(db,file="data/ITN029_all.rda")#save stats

#read pre-determined events number for tubes from csv file
##pannel name should be in place of tube name since the entire package is using pannel name 
##to represent the tube


tubesEvents<-read.csv(file.path(system.file("data",package="QUALIFIER"),"tubesevents.csv"),row.names=1)

tubesEvents2009<-QUALIFIER:::.TubeNameMapping(db,tubesEvents[,1,drop=F])
tubesEvents2007<-QUALIFIER:::.TubeNameMapping(db,tubesEvents[,2,drop=F])




################################################################################  
#3. perform different QA checks
#interactive lattice plot for each individual qatask
#a)customized formula can be supplied to overwrite the default one
#b)by default dest=NULL, which plots on the regular R device,otherwise it is a
#character indicating the path to save the svg plot
###############################################################################
#load("gatingHierarchy/GS.Rda")#load gatinghierarchy from disk
data("ITN029")#load stats from disk
#db$G<-G
checkListFile<-file.path(system.file("data",package="QUALIFIER"),"qaCheckList.csv")
qaTask.list<-makeQaTask(db,checkListFile)

CairoX11()#for faster rendering plot

###80% of the pre-defined the value for each pannel
qaCheck(qaTask.list[["NumberOfEvents"]]
		,formula=count ~ RecdDt | Tube
		,outlierfunc=outlier.cutoff
		,lBound=0.8*tubesEvents2009
		,subset="RecdDt>='2009-08-01'"
)
qaCheck(qaTask.list[["NumberOfEvents"]]
		,formula=count ~ RecdDt | Tube
		,outlierfunc=outlier.cutoff
		,lBound=0.8*tubesEvents2007
		,subset="RecdDt<'2009-08-01'"
)


plot(qaTask.list[["NumberOfEvents"]]
#		,subset="Tube=='CD8/CD25/CD4/CD3/CD62L'"
#,dest="image"
)

plot(qaTask.list[["NumberOfEvents"]]
		,subset="id=='366'"
		,scatterPlot=TRUE
)



qaCheck(qaTask.list[["BoundaryEvents"]]
		,sum(percent) ~ RecdDt | name
		,outlierfunc=outlier.cutoff
		,uBound=0.0003
)
plot(qaTask.list[["BoundaryEvents"]]
		,percent ~ RecdDt | channel
#		,dest="image"
)



qaCheck(qaTask.list[["MFIOverTime"]]
#		,outlierfunc=outlier.norm
		,rFunc=rlm
#		,subset="channel%in%c('PE-Cy7-A')"
		,z.cutoff=3
)
plot(qaTask.list[["MFIOverTime"]]
		,y=MFI~RecdDt|stain
		,subset="channel%in%c('PE-Cy7-A')"
		,rFunc=rlm
		,scales=list(y=c(relation="free"))

)


qaCheck(qaTask.list[["RBCLysis"]]
		,formula=percent ~ RecdDt | Tube
		,outlierfunc=outlier.cutoff
		,lBound=0.8
)
plot(qaTask.list[["RBCLysis"]]
#		,subset="Tube=='CD8/CD25/CD4/CD3/CD62L'"
#		,dest="image"
#	,plotAll="none"
)	


qaCheck(qaTask.list[["spike"]]
		,outlierfunc=outlier.norm#outlier.t
		,z.cutoff=6
#		,alpha=0.00001
)
plot(qaTask.list[["spike"]],y=spike~RecdDt|channel
		,subset="Tube=='CD11c/CD80/DUMP/HLADr/CD123'"
#	,dest="image"
#	,plotAll=T
)

plot(qaTask.list[["spike"]],y=spike~RecdDt|channel
		,subset="id=='366'&channel=='FITC-A'"
		,scatterPlot=TRUE
)


qaCheck(qaTask.list[["MNC"]]
		,formula=percent ~ coresampleid
#		,outlierfunc=qoutlier
#		,alpha=1.5
		,z.cutoff=3)
plot(qaTask.list[["MNC"]],percent ~ coresampleid
#	,dest="image"
	)

#scatter plot for a sample group	
plot(qaTask.list[["MNC"]]
		,scatterPlot=TRUE
		,subset="coresampleid==11730")
#scatter okit fore one sample
plot(qaTask.list[["MNC"]]
		,scatterPlot=TRUE
		,subset="coresampleid==8780&id==49")

qaCheck(qaTask.list[["RedundantStain"]]
#			,gOutlierfunc=qoutlier
#			,outlierfunc=qoutlier
#			,alpha=1.5
#			,z.cutoff=2
		)
		
##example of passing lattice arguments		
plot(qaTask.list[["RedundantStain"]]
		,subset="channel=='APC-A'&stain%in%c('CD123','Auto')"
		,y=percent~coresampleid|channel:stain
		,ylab="test",
		,scales=list(x=c(draw=F)
							)
		,layout=c(1,2)
				
)
################################################################################  
#4.qa report in html format 
#set plotAll=TRUE to generate the scatter plots for all the individual FCS files 
#otherwise only plots for outliers are generated.
###############################################################################
qaReport(qaTask.list,outDir="~/rglab/workspace/QUALIFIER/output",plotAll="none")



















