# TODO: Add comment
# 
# Author: mike
###############################################################################
library(flowQA)
#library(Rmpi)
#library(multicore)
#unloadNamespace("flowQA")
#library(ncdfFlow)
#library(flowWorkspace)
#unloadNamespace("flowWorkspace")
#unloadNamespace("ncdfFlow")

###compile vignette
setwd("inst/doc")
library(tools)
Sweave("flowQA.Rnw")
texi2dvi("flowQA.tex",pdf = TRUE)

localDir<-"~/rglab"
outDir<-file.path(localDir,"workspace/flowQA/output/ITN029_339")
dest<-file.path(outDir,"trellis_plot/")

################################################################################  
#1.parse QA flowJo workspace into R
#Note that this step is most time consuming especially for large datasets
#it is convienient to save the gatingset once it is done 
#so that it be loaded directly from disk later on for the further processing 
###############################################################################
ws<-openWorkspace("~/rglab/workspace/flowQA/misc/ITN029ST/QA_MFI_RBC_bounary_eventsV3.xml")
G<-parseWorkspace(ws,execute=TRUE,isNcdf=TRUE)
saveNcdf("G","gatingHierarchy")
save(G,file="gatingHierarchy/GS.Rda")

################################################################################  
#2.load metadata for QA and extract cell counts,percentage and MFI
###############################################################################
load(file="gatingHierarchy/GS.Rda")
time1<-Sys.time()
db<-extractStats(G,metaFile="~/rglab/workspace/flowQA/misc/ITN029ST/FCS_File_mapping.csv")
Sys.time()-time1

save(db,file="ITN029.rda")#save stats

#unique(db$anno$Tube)
#unique(db$statsOfGS$channel)

##select a smaller subset for demo
#sampleCount<-table(db$anno$coresampleid[db$anno$name%in%t1$fcsFile[1:50]])
#sampleSelected<-names(sampleCount[sampleCount>2])
#db$anno<-db$anno[db$anno$coresampleid%in%sampleSelected,]

fcsSelected<-sample(db$anno$name,20)
db$anno<-db$anno[db$anno$name%in%fcsSelected,]

db$statsOfGS<-db$statsOfGS[db$statsOfGS$id%in%db$anno$id,]


nrow(db$statsOfGS)
################################################################################  
#3. perform different QA checks
###############################################################################
#load("gatingHierarchy/GS.Rda")#load gatinghierarchy from disk
data("ITN029")#load stats from disk
#db$G<-G
checkListFile<-file.path(system.file("data",package="flowQA"),"qaCheckList.csv")
qaTask.list<-makeQaTask(db,checkListFile)


###80% of the pre-defined the value for each pannel
qaCheck(qaTask.list[["NumberOfEvents"]]
		,outlierfunc=outlier.cutoff
		,lBound=0.8
)

qaCheck(qaTask.list[["BoundaryEvents"]]
		,sum(percent) ~ RecdDt | name
		,outlierfunc=outlier.cutoff
		,uBound=0.0003


)

qaCheck(qaTask.list[["MFIOverTime"]]
		,outlierfunc=outlier.t
		,rFunc=lm
		,alpha=0.05
)

qaCheck(qaTask.list[["RBCLysis"]]
		,formula=percent ~ RecdDt | Tube
		,outlierfunc=outlier.cutoff
		,lBound=0.8
		)

qaCheck(qaTask.list[["spike"]],outlierfunc=outlier.t,alpha=0.00001)

qaCheck(qaTask.list[["MNC"]],outlierfunc=qoutlier,alpha=1.5)

qaCheck(qaTask.list[["RedundantStain"]],outlierfunc=qoutlier,alpha=1.5)

################################################################################  
#4.qa report in html format 
#set plotAll=TRUE to generate the scatter plots for all the individual FCS files 
#otherwise only plots for outliers are generated.
###############################################################################
qa.report(db,outDir="~/rglab/workspace/flowQA/output",plotAll=F)



################################################################################  
#5.interactive lattice plot for each individual qatask
#a)customized formula can be supplied to overwrite the default one
#b)by default dest=NULL, which plots on the regular R device,otherwise it is a
#character indicating the path to save the svg plot
###############################################################################


plot(qaTask.list[["NumberOfEvents"]]
		,subset="Tube=='CD8/CD25/CD4/CD3/CD62L'"
#,dest="image"
)
#
plot(qaTask.list[["BoundaryEvents"]]
		,percent ~ RecdDt | channel
#		,dest="image"
)

plot(qaTask.list[["MFIOverTime"]],y=MFI~RecdDt|stain
		,subset="channel%in%c('FITC-A')"
		,rFunc=lm
#		,dest="image"
#		,plotAll=T

)

plot(qaTask.list[["RBCLysis"]]
#		,dest="image"
)	

plot(qaTask.list[["spike"]],y=spike~RecdDt|channel
		,subset="Tube=='CD8/CD25/CD4/CD3/CD62L'&channel%in%c('FITC-A')"
#	,dest="image"
#	,plotAll=T
)

plot(qaTask.list[["MNC"]]
		,dest="image"
)	

plot(qaTask.list[["RedundantStain"]]
		,subset="stain%in%c('CD8')"
#		,dest="image"
#		,plotAll=T
)



################################################################################  
#5.qa report in html
###############################################################################
qa.report(db,outDir="~/rglab/workspace/flowQA/output")
	

