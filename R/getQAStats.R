# TODO: Add comment
# 
# Author: wjiang2
###############################################################################
##TODO:to append the stats to the current table
setMethod("getQAStats",signature=c("environment"),function(obj,gsid,isFlowCore=TRUE,nslaves=NULL,...){
			if(missing(gsid))
				stop("missing gsid!")
			
			
			gs<-obj$gs[[gsid]][1:20]
			
#			browser()
			
			statsOfGS<-getQAStats(gs,isFlowCore,nslaves)
			
			
			
			statsOfGS<-lapply(names(statsOfGS),function(curID){
												curStats<-statsOfGS[[curID]]
												curStats$id<-as.integer(curID)
												curStats
											})
			statsOfGS<-do.call("rbind",statsOfGS)
			

			##append sid and gsid
			if(nrow(obj$stats)==0)
				msid<-0
			else
				msid<-max(obj$stats$sid)
			statsOfGS$sid<-1:nrow(statsOfGS)+msid
			statsOfGS$gsid<-gsid
			

			#save it db (remove the old records with the same gsid
#			browser()
			ind<-obj$stats$gsid==gsid
			obj$stats<-rbind(obj$stats[!ind,],statsOfGS[,colnames(obj$stats)])
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
					getQAStats(gh,isFlowCore=isFlowCore)
					})
			parallel::stopCluster(cl)
			}else
			{
				message("It is currently running in serial mode and the parallel mode is recommend for faster processing.")

#				browser()
#				time1<-Sys.time()
				statsOfGS<-lapply(glist,getQAStats,isFlowCore=isFlowCore)
#				Sys.time()-time1
			}
			
			statsOfGS
			
		})
#TODO:GateingHierarchy already has fjName slot, this is method should directly get this slot
#setMethod("getPath",signature("GatingHierarchy"),function(x,y,...){
##			browser()
#				path_detail<-sp.between(x@tree,getNodes(x)[1],y)[[1]]$path_detail
#				path_detail[1]<-".root"
#				path<-paste(unlist(lapply(path_detail,function(x)strsplit(x,"\\.")[[1]][2]))
#					,collapse="/")
#				paste("/",path,sep="")
#				})
#setMethod("getPath",signature("GatingHierarchyInternal"),function(x,y,...){
##			browser()
#			ind<-which(getNodes(x)%in%y)
#			getNodes(x,isPath=T)[ind]
#			
#			
#		})
##extract stats from a gating hierarchy\\
setMethod("getQAStats",signature("GatingHierarchy"),function(obj,isFlowCore=TRUE,...){
			
			#check if data is gated
			params<-try(parameters(getData(obj))$name,silent=TRUE)
			if(inherits(params,"try-error"))
				params<-NULL
			
			statsPop<-getPopStats(obj)
			nodes<-getNodes(obj)
			nodePaths<-getNodes(obj,isPath=T)
#			browser()
			nParam<-length(params)-1 #minus time channel
			nNodes<-length(nodes)
#			nStats<-nParam+2 #root stats(spikes+count+%)
#					+(nNodes-1)*2##count,%
#					
#			statsOfGh<-data.frame(sid=integer(nNodes) #statesID:unique for each stat entry
#								,id=integer(nNodes)#fileID:unique for each FCS
#								,gsid=integer(nNodes)#gatignSetID:unique fore each gatingSet
#								,population=character(nNodes)
#								,stats=character(nNodes)
#								,node=character(nNodes)
#								,channel=character(nNodes)
#								,value=numeric(nNodes)
#								)
			statsOfGh<-NULL
#			browser()
			fdata<-getData(obj)
			for(i in 1:nNodes)
			{
				curPopName<-nodePaths[i]
				curNode<-nodes[i]
#				print(curNode)
				if(!is.null(params))
				{
					curData<-getData(obj,curNode)
					curGate<-getGate(obj,curNode)
				}
				
#				browser()
				##extract pop name
#				curPopName<-getPath(obj,curNode)
#				browser()
		
				##get count and proportion
				statsOfNode<-subset(statsPop,node==curNode)
#				if(curPopName%in%c("margin","MFI"))
				if(!is.null(params)&&!QUALIFIER:::.isRoot(obj,curNode))#&&!is.na(curGate)
				{
					chnl<-parameters(curGate)
					#only 1D gate needs to save channel info
					if(length(chnl)>1)
						chnl<-NA
				}else
				{
					chnl<-NA
				}
				if(isFlowCore)
				{
					stats_prop<-statsOfNode$flowCore.freq
					stats_count<-statsOfNode$flowCore.count	
				}else
				{
					stats_prop<-statsOfNode$flowJo.freq
					stats_count<-statsOfNode$flowJo.count
				}
				
				statsOfNode<-data.frame(channel=chnl,stats=c("proportion","count")
										,value=c(stats_prop,stats_count)
										,row.names=NULL)
				
				#get spikes meatures for each channel at root level
				if(!is.null(params)&&QUALIFIER:::.isRoot(obj,curNode))
				{

#					browser()
					expr <- exprs(fdata)
					
					time <- flowCore:::findTimeChannel(expr)
					if(!(time %in% colnames(expr)))
						stop("Invalid name of variable (", time, ") recording the ",
								"\ntime domain specified as 'time' argument.", call.=FALSE)
					
					spikes<-unlist(lapply(params[!params%in%time],QUALIFIER:::.timelineplot,x=fdata, binSize=50))
					
					statsOfNode<-rbind(statsOfNode,data.frame(channel=params[!params%in%time],stats="spike",value=spikes))
					chnls<-params[!params%in%time] #select channel at root level
					
				}


				#get MIF meatures
				if(!is.na(chnl))
				{
#					browser()
					mat<-exprs(curData)[,chnl,drop=FALSE]
					chnames<-colnames(mat)
					MFI<-rowMedians(t(mat))#using rowMedian to speed up
#					MFI<-colMeans(exprs(curData)[,chnl,drop=FALSE])
					if(all(!is.na(MFI)))
						statsOfNode<-rbind(statsOfNode,data.frame(channel=chnames,stats="MFI",value=MFI))
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
