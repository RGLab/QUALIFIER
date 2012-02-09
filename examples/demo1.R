# TODO: Add comment
# 
# Author: mike
###############################################################################


library(flowQA)

################################################################################  
#1.load stats from disk
###############################################################################


data("ITN029")

###make sure to set the working dir to where the gating hierarchy folder is saved
setwd("output")

################################################################################  
#2.construct qatask from the checklist(specified in csv)
###############################################################################

checkListFile<-file.path(system.file("data",package="flowQA"),"qaCheckList.csv")
qaTask.list<-makeQaTask(db,checkListFile)


################################################################################  
#3.perform each individual qa check by  
#a)selecting the appropriate outlier detection algorithm
#b)adjusting the threshold 
#c)changing the aggregation level by the formula 
###############################################################################

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
	,dest="image"
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
		,dest="image"
		,plotAll=T
)



