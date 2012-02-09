################################################################################  
# load FCS and parse flowJo workspace for QA
###############################################################################
library(flowQA)
#library(RSQLite)
library(Rmpi)
library(multicore)

localDir<-"~/rglab"
sourceDir<-file.path(localDir,"workspace/flowQ/misc/ITN029ST")
annoData<-read.csv(file.path(sourceDir,"FCS_File_mapping.csv"), as.is=TRUE)
sampleIds<-unique(annoData$coresampleid)
annoData<-subset(annoData,coresampleid%in%sampleIds[1:30])

ws<-openWorkspace(file.path(sourceDir,"/QA_MFI_RBC_bounary_eventsV3.xml"))

tempfolder<-tempdir()
for(curFcs in annoData$FCS_Files)
	system(paste("ln -s",file.path(ws@path,curFcs),file.path(tempfolder,curFcs),sep=" "))

time1<-Sys.time()
G<-parseWorkspace(
		ws,execute=TRUE,path=tempfolder,isNcdf=TRUE,cleanup=FALSE,keep.indices=TRUE,name=1
#		,subset=c(1:500)
#		,nslaves=14
)

Sys.time()-time1
saveNcdf("G","gatingHierarchy")
save(G,file="gatingHierarchy/GS.Rda")
