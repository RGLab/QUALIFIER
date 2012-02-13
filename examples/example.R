# TODO: Add comment
# 
# Author: mike
###############################################################################
library(flowQA)
library(Rmpi)
library(multicore)
library(snow)
#unloadNamespace("flowQA")
library(ncdfFlow)
#library(flowWorkspace)
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
ws<-openWorkspace("~/rglab/workspace/flowQA/misc/ITN029ST/QA_MFI_RBC_bounary_eventsV3.xml")


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
subsetID<-getFJWSubsetIndices(ws,key="$FIL",value=as.character(anno$FCS_Files),group=2)
##parse the workspace with the subset
G<-parseWorkspace(ws,execute=T,isNcdf=T,name=2,nslaves=6,subset=subsetID)
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
tubesEvents<-read.csv("~/rglab/workspace/QUALIFIER/misc/tubesevents.csv",row.names=1)
tubesEvents<-.TubeNameMapping(db,tubesEvents)




################################################################################  
#3. perform different QA checks
###############################################################################
#load("gatingHierarchy/GS.Rda")#load gatinghierarchy from disk
data("ITN029_all")#load stats from disk
#db$G<-G
checkListFile<-file.path(system.file("data",package="flowQA"),"qaCheckList.csv")
qaTask.list<-makeQaTask(db,checkListFile)


###80% of the pre-defined the value for each pannel
qaCheck(qaTask.list[["NumberOfEvents"]]
		,formula=count ~ RecdDt | Tube
		,outlierfunc=outlier.cutoff
		,lBound=0.8*tubesEvents
#		,subset="Tube=='CD8/CD25/CD4/CD3/CD62L'"
)

qaCheck(qaTask.list[["BoundaryEvents"]]
		,sum(percent) ~ RecdDt | name
		,outlierfunc=outlier.cutoff
		,uBound=0.0003


)

qaCheck(qaTask.list[["MFIOverTime"]]
		,outlierfunc=outlier.norm
		,rFunc=rlm
		,z.cutoff=3
)

qaCheck(qaTask.list[["RBCLysis"]]
		,formula=percent ~ RecdDt | Tube
		,outlierfunc=outlier.cutoff
		,lBound=0.8
)

qaCheck(qaTask.list[["spike"]],outlierfunc=outlier.t,alpha=0.00001)

qaCheck(qaTask.list[["MNC"]],formula=percent ~ coresampleid,outlierfunc=qoutlier,alpha=1.5)

qaCheck(qaTask.list[["RedundantStain"]],outlierfunc=qoutlier,alpha=1.5)

################################################################################  
#4.qa report in html format 
#set plotAll=TRUE to generate the scatter plots for all the individual FCS files 
#otherwise only plots for outliers are generated.
###############################################################################
qa.report(db,outDir="~/rglab/workspace/QUALIFIER/output",plotAll=F)



################################################################################  
#5.interactive lattice plot for each individual qatask
#a)customized formula can be supplied to overwrite the default one
#b)by default dest=NULL, which plots on the regular R device,otherwise it is a
#character indicating the path to save the svg plot
###############################################################################


plot(qaTask.list[["NumberOfEvents"]]
#		,subset="Tube=='CD8/CD25/CD4/CD3/CD62L'"
#,dest="image"
)
#
plot(qaTask.list[["BoundaryEvents"]]
		,percent ~ RecdDt | channel
#		,dest="image"
)

plot(qaTask.list[["MFIOverTime"]],y=MFI~RecdDt|stain
		,subset="channel%in%c('FITC-A')"
		,rFunc=rlm
		,relation="free"
#		,dest="image"
#		,plotAll=T

)

plot(qaTask.list[["RBCLysis"]],subset="Tube=='CD8/CD25/CD4/CD3/CD62L'"
		#,dest="image"
)	

plot(qaTask.list[["spike"]],y=spike~RecdDt|channel
		,subset="Tube=='CD8/CD25/CD4/CD3/CD62L'&channel%in%c('FITC-A')"
#	,dest="image"
#	,plotAll=T
)

plot(qaTask.list[["MNC"]],percent ~ coresampleid
#		,dest="image"
)	

plot(qaTask.list[["RedundantStain"]]
		,subset="stain%in%c('CD3','CD4')"
		,y=percent~coresampleid|channel:stain
#		,dest="image"
#		,plotAll=T
)







