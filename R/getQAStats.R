# TODO: Add comment
# 
# Author: wjiang2
###############################################################################
##TODO:to append the stats to the current table
setMethod("getQAStats",signature=c("environment"),function(obj,gsid,...){
			if(missing(gsid))
				stop("missing gsid!")
			
			
			gs<-obj$gs[[gsid]]
			
#			browser()
			
			statsOfGS<-getQAStats(gs,...)
			
			
			
			statsOfGS<-lapply(names(statsOfGS),function(curID){
									curStats<-statsOfGS[[curID]]
									curStats$fileid<-as.integer(curID)
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


setMethod("getQAStats",signature("GatingSet"),function(obj,nslaves=NULL,type="PSOCK",...){
			
			
			print("extracting stats...")
			
			glist<-obj@set
			IDs<-pData(obj)[getSamples(obj),'fileid']
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
			##parallel mode is not available for gating set of internal structure  
			##due to the undistributable pointer
#			if(!is.null(nslaves)&&nslaves>1)
#			{
#							
#				message("Using the parallel mode with ",nslaves," cores")
#				
#				cl<-parallel::makeCluster(nslaves,type)
#				statsOfGS<-parallel::parLapply(cl,glist,function(gh){
#												library(QUALIFIER)
#												getQAStats(gh,...)
#												},...)
#				parallel::stopCluster(cl)
#			}else
#			{
#				message("It is currently running in serial mode.")


#				time1<-Sys.time()
				statsOfGS<-lapply(glist,getQAStats,...)
#				Sys.time()-time1
#			}
			
			statsOfGS
			
		})
##extract stats from a gating hierarchy\\
setMethod("getQAStats",signature("GatingHierarchy"),function(obj,isFlowCore=TRUE,isMFI=TRUE,isSpike=TRUE,pops = NULL,...){
			
			message("reading GatingHierarchy:",getSample(obj))
#			browser()
			#check if data is gated
			params<-try(parameters(getData(obj))$name,silent=TRUE)
			if(inherits(params,"try-error"))
				params<-NULL
			
			statsPop<-getPopStats(obj)
			nodes<-getNodes(obj)
            #subset the nodes
            if(!is.null(pops)){
              if(is.numeric(pops)){
                nodes <- nodes[pops]  
              }else{
                nodes <- nodes[match(pops,nodes)]
              }
              
            }
			nodePaths<-getNodes(obj,isPath=T)
			#convert to QUALIFIER's path
			nodePaths[1]<-paste("/",nodePaths[1],sep="")
			nodePaths[-1]<-paste("/root",nodePaths[-1],sep="")
			nParam<-length(params)-1 #minus time channel
			nNodes<-length(nodes)
#					
			statsOfGh<-NULL
#			browser()
			fdata<-getData(obj)
			pd<-pData(parameters(fdata))
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
				
		
				##get count and proportion
				statsOfNode<-subset(statsPop,node==curNode)
#				if(curPopName%in%c("margin","MFI"))
				if(!is.null(params)&&!.isRoot(obj,curNode)&&class(curGate)!="BooleanGate")#&&!is.na(curGate)
				{
#					browser()
					chnl<-parameters(curGate)
#					
#					if(length(chnl)>1)
#						chnl<-NA
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

				if(is.na(chnl))
					stain<-NA
				else
				{
					stain<-unname(pd[match(chnl,pd[,"name"]),"desc"])
				}
				statsOfNode<-data.frame(channel=chnl,stain=stain,stats=c("proportion","count")
										,value=c(stats_prop,stats_count)
										,row.names=NULL)
#				browser()
				#get spikes meatures for each channel at root level
				if(!is.null(params)&&.isRoot(obj,curNode)&&isSpike)
				{

#					browser()
					expr <- exprs(fdata)
					
					time <- flowCore:::findTimeChannel(expr)
					if(!(time %in% colnames(expr)))
						stop("Invalid name of variable (", time, ") recording the ",
								"\ntime domain specified as 'time' argument.", call.=FALSE)
					nonTimeChnls<-params[!params%in%time]
					stain<-unname(pd[match(nonTimeChnls,pd[,"name"]),"desc"])
					
					spikes<-unlist(lapply(nonTimeChnls,.timelineplot,x=fdata, binSize=50))
					
					statsOfNode<-rbind(statsOfNode,data.frame(channel=nonTimeChnls,stain=stain,stats="spike",value=spikes))
					
				}


				#get MIF meatures
				if(!is.na(chnl)&&isMFI)
				{
#					browser()
					mat<-exprs(curData)[,chnl,drop=FALSE]
					chnames<-colnames(mat)
					MFI<-rowMedians(t(mat))#using rowMedian to speed up
#					MFI<-colMeans(exprs(curData)[,chnl,drop=FALSE])
					if(all(!is.na(MFI)))
						statsOfNode<-rbind(statsOfNode,data.frame(channel=chnames,stain=stain,stats="MFI",value=MFI))
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
	if(!channel %in% flowCore::colnames(x))
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
