# TODO: Add comment
# 
# Author: wjiang2
###############################################################################
unloadNamespace("QUALIFIER")
library(QUALIFIER)
library(latticeExtra)
lapply(list.files("/home/wjiang2/rglab/workspace/QUALIFIER/R",pattern=".R",full=T),source)

library(parallel)
G <- load_gs("~/rglab/workspace/QUALIFIER/misc/HVTNsubset")

db<-new.env()
initDB(db)
metaFile="~/rglab/workspace/QUALIFIER/misc/ITN029ST/FCS_File_mapping.csv"
qaPreprocess(db=db,gs=G
		,isMFI=F,isSpike=F
		,nslave=2
		,type="SOCK"
)
pData(db$gs[[1]])
#pData(G)
#getQAStats(G[[1]],isMFI=F,isSpike=F)


checkListFile<-"~/rglab/workspace/QUALIFIER/misc/qaTasksListHVTN.csv"
qaTask.list<-read.qaTask(db,checkListFile=checkListFile)
qaTask.list[[2]]
plot(qaTask.list[[2]],proportion ~ factor(`Sample/VISITNO`) | `Sample/Stim`)
