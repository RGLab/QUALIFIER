# TODO: Add comment
# 
# Author: wjiang2
###############################################################################

setMethod("getQAStats",signature("environment"),function(obj,isFlowCore=TRUE,nslaves=NULL,...){
			
			statsOfGS<-getQAStats(obj$G,isFlowCore,nslaves)
#						browser()
			statsOfGS<-lapply(names(statsOfGS),function(curID){
						curStats<-statsOfGS[[curID]]
						curStats$id<-as.integer(curID)
						curStats
					})

			db$statsOfGS<-do.call("rbind",statsOfGS)
			obj$statsOfGS$sid<-1:nrow(obj$statsOfGS)
			print("stats saved!")
			
		})


setMethod("getQAStats",signature("GatingSet"),function(obj,isFlowCore=TRUE,nslaves=NULL,...){
			
			
			print("extracting stats...")
			
			glist<-obj@set
			IDs<-pData(obj)[getSamples(obj),"id"]
			if(is.null(IDs)||length(IDs)!=length(glist))
			{
				stop("Not all IDs for the current sample set are found in meta data of this GatingSet!")	
			}
			
			names(glist)<-IDs
			
			if(length(grep("parallel",loadedNamespaces()))==1)
			{
#				cores<-getOption("cores")
				if(is.null(nslaves))
				{
					nslaves<-parallel::detectCores()
				}
				
				
			}
			if(!is.null(nslaves)&&nslaves>1)
			{
				message("Using the parallel mode with ",nslaves," cores")
				cl<-parallel::makeCluster(nslaves,type="SOCK")
				statsOfGS<-parallel::parLapply(cl,glist,function(gh){
					library(QUALIFIER)
					getQAStats(gh)
					})
			parallel::stopCluster(cl)
			}else
			{
				message("It is currently running in serial mode and the parallel mode is recommend for faster processing.")
				
				statsOfGS<-lapply(glist,getQAStats)
			}
			
			statsOfGS
			
		})


##extract stats from a gating hierarchy\\
setMethod("getQAStats",signature("GatingHierarchy"),function(obj,isFlowCore...){
			
			statsOfGh<-NULL#data.frame(id=as.integer(),node=as.character(),population=as.character(),stats=as.character(),value=as.numeric())
	#		params<-colnames(getData(gh))#TODO:it is wierd that colnames methods does not work after enter this function
			params<-parameters(getData(obj))$name
			statsPop<-getPopStats(obj)
			nodes<-getNodes(obj)
#			browser()
			for(i in 1:length(nodes))
			{
				curNode<-nodes[i]
#				print(curNode)
				curData<-getData(obj,curNode)
				curGate<-getGate(obj,curNode)
				
				
				##extract pop name
				if(QUALIFIER:::.isRoot(obj,curNode))
				{
					curPopName<-"Total"
				}else
				{
					curNodeName<-strsplit(curNode,"\\.")[[1]]
					if(length(curNodeName)>1)
						curNodeName<-curNodeName[2]
					if(grepl("MFI",curNodeName)||grepl("margin",curNodeName))
					{
						curPopName<-strsplit(curNodeName," ")[[1]][2]
					}else
					{
						curPopName<-curNodeName
					}
				}
				
				##get count and proportion
				statsOfNode<-subset(statsPop,node==curNode)
#				if(curPopName%in%c("margin","MFI"))
				if(!QUALIFIER:::.isRoot(obj,curNode))#&&!is.na(curGate)
				{
					chnl<-parameters(curGate)
					#only 1D gate needs to save channel info
					if(length(chnl)>1)
						chnl<-NA
				}else
				{
					chnl<-NA
				}
				statsOfNode<-data.frame(channel=chnl,stats=c("proportion","count"),value=c(statsOfNode$flowCore.freq,statsOfNode$flowCore.count),row.names=NULL)
				
				#get spikes meatures for each channel at root level
				if(QUALIFIER:::.isRoot(obj,curNode))
				{

#					browser()
					spikes<-unlist(lapply(params[-8],.timelineplot,x=getData(obj), binSize=50))
					
					statsOfNode<-rbind(statsOfNode,data.frame(channel=params[-8],stats="spike",value=spikes))
					chnls<-params[-8] #select channel at root level
					
				}
#				else
#				{
#					#only select channels defined by the gate when colecct MFI from non-root node
#					chnls<-parameters(curGate)
#				}

				#get MIF meatures
				if(!is.na(chnl))
				{
					MFI<-colMeans(exprs(curData)[,chnl,drop=FALSE])
					if(all(!is.na(MFI)))
						statsOfNode<-rbind(statsOfNode,data.frame(channel=names(MFI),stats="MFI",value=MFI))
				}
				##append the rows
				
				statsOfGh<-rbind(statsOfGh,cbind(statsOfNode,node=curNode,population=curPopName,row.names=NULL))
			}
		
			statsOfGh
			
			
		})

##copy from timelineplot of flowViz, add the plot flag
.timelineplot <- function(x, channel, binSize, varCut=1, ...)#add plot flag to enable spike score computing without plots
{
#	browser()
	## Sanity checking up front
	if(!length(channel)==1)
		stop("'channel' must be character scalar")
	if(!channel %in% flowCore:::colnames(x))
		stop(channel, " is not a valid channel in this flowSet.")
	if(tolower(channel) == "time")
		stop("Argument 'channel' can not be the time channel")
	
	## Bin the data and compute local variances and locations
	time <- flowCore:::findTimeChannel(xx= exprs(x))
	timeData <- flowCore:::prepareSet(x, parm=channel, time=time,
			binSize=binSize )
#	browser()	
	
	## Standardize to compute meaningful scale-free QA scores
	med <- median(timeData$smooth[,2], na.rm=TRUE)
	vars <- mean(timeData$variance)
	stand <-  abs(timeData$smooth[,2]-med)/(vars*varCut)
	
#    browser()
	## the QA score and correlation coefficient
	
	qaScore <-sum(stand[stand>0])/varCut
	
	
	## The return value with attributes attached.
	attr(qaScore, "binSize") <- binSize
	return(qaScore)
}
