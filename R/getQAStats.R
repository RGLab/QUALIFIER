# TODO: Add comment
# 
# Author: wjiang2
###############################################################################
##TODO:to append the stats to the current table

.getQAStats.env <- function(obj,gsid,...){
			if(missing(gsid))
				stop("missing gsid!")
			
			
			gs<-obj$gs[[gsid]]
			
#			browser()
			
			statsOfGS<-getQAStats(gs,...)
			
			for(curID in names(statsOfGS)){
					
                    curID <- as.integer(curID)
                    statsOfGS[[curID]][,fileid:= curID]
				}
			statsOfGS<-rbindlist(statsOfGS)
			

			##append sid and gsid
			if(nrow(obj$stats)==0)
				msid<-0
			else
				msid<-max(obj$stats$sid)
			statsOfGS[,sid := 1:nrow(statsOfGS)+msid]
			statsOfGS[,gsid := gsid]
			

			#save it db (remove the old records with the same gsid
			
			ind <- obj$stats$gsid == gsid
			obj$stats<-rbind(obj$stats[!ind,],statsOfGS[,colnames(obj$stats),with=FALSE])
			print("stats saved!")
			
		}
 setMethod("getQAStats",signature=c("environment"),function(obj,gsid,...){
       .getQAStats.env(obj,gsid,...)
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
setMethod("getQAStats",signature("GatingHierarchy"),function(obj, ...){
      .getQAStats.gh(obj, ...)    
    })

.getQAStats.gh <- function(obj,isFlowCore=TRUE,isMFI = FALSE,isSpike = FALSE,pops = NULL, ...){
			
			message("reading GatingHierarchy:",getSample(obj))
			
			#check if data is gated
			params<-try(parameters(getData(obj))$name,silent=TRUE)
			if(inherits(params,"try-error"))
				params<-NULL
#			browser()
            
#			statsPop<-getPopStats(obj)
            #convert to data.table
#            rn <- rownames(statsPop)
#            statsPop <- as.data.table(statsPop)
#            statsPop[,path:=eval(rn)]
#            setkey(statsPop,node)
            
			nodes<-getNodes(obj)
            nodePaths<-getNodes(obj,isPath=T)
            #convert to QUALIFIER's path
            nodePaths[1]<-paste("/",nodePaths[1],sep="")
            nodePaths[-1]<-paste("/root",nodePaths[-1],sep="")
            
            #subset the nodes
      
            if(!is.null(pops)){
              if(is.numeric(pops)){
                nodes <- nodes[pops]
                nodePaths <- nodePaths[pops]
              }else{
                nodes <- nodes[match(pops,nodes)]
                nodePaths <- nodePaths[match(pops,nodes)]
              }
              
            }
            
            
			nParam<-length(params)-1 #minus time channel
			nNodes<-length(nodes)
#					
			statsOfGh <- data.table()
            
			fdata<-getData(obj, use.exprs = isSpike||isMFI)
			pd<-pData(parameters(fdata))
                      
			statslist <- lapply(1:nNodes,function(i){

  				curPopName<-nodePaths[i]
  				curNode<-nodes[i]
                

  				if(!is.null(params))
  				{
  					
  					curGate<-getGate(obj,curNode)
  				}
  				
#                browser()
                
                ##############################################
                #extract channel and stain info
                #it is mainly used for QA on channel/stain-specific 1d gate
                #e.g. marginal events for each channel (defined by 1d gate on that channel)
                # or MFI stability for each stain+channel (defined by 1d gate on each channel)
                # or redudant staining across aliquots (1d gate on each stain+channel)
                # or Spike on each channel
                # Thus not really needed for 2-d gates unless we want to do QA on MFI
                #######################################################################
  				if(is.null(params)||.isRoot(obj,curNode)||class(curGate)=="booleanFilter")
  				{
                  #set channel to NA for root node or boolean gate
                  chnl <- as.character(NA) #convert from logical NA to character NA for rbindlist to behave appropriately
  				}else
  				{
                  chnl <- parameters(curGate)
                  #set channel to NA for 2d gate when isMFI == FALSE
                  if(!isMFI&&length(chnl)>1)
                    chnl <- as.character(NA)  
  				}
  				
  
  				if(all(is.na(chnl)))
  					stain<-as.character(NA)
  				else
  				{
  					stain<-unname(pd[match(chnl,pd[,"name"]),"desc"])
  				}
                ##get count and proportion
                statsOfNode <- flowWorkspace:::.getPopStat(obj,curNode)
                
                if(isFlowCore)
                {
                  statsOfNode <- statsOfNode$flowCore
                }else
                {
                  statsOfNode<-statsOfNode$flowJo
                }
  				statsOfNode <- data.table(channel=chnl
                                          ,stain=stain
                                           ,stats= names(statsOfNode)
    										,value = statsOfNode
  										)
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
  					
  					spikes <- unlist(lapply(nonTimeChnls,.timelineplot,x=fdata, binSize=50))
  					
  					statsOfNode <- rbindlist(list(statsOfNode
                                                    ,data.table(channel=nonTimeChnls
                                                                ,stain=stain
                                                                ,stats="spike"
                                                                 ,value=spikes)
                                                    )
                                              )
  					
  				}
  
  
  				#get MIF meatures
  				if(!is.na(chnl)&&isMFI)
  				{
                      curData<-getData(obj,curNode)
  #					browser()
  					mat<-exprs(curData)[,chnl,drop=FALSE]
  					chnames<-colnames(mat)
  					MFI<-rowMedians(t(mat))#using rowMedian to speed up
  
  					if(all(!is.na(MFI)))
  						statsOfNode <- rbindlist(list(statsOfNode
                                                        ,data.table(channel=chnames
                                                                    ,stain=stain
                                                                    ,stats="MFI"
                                                                     ,value=MFI)
                                                         )
                                                     )
  				}
                  #append two columns
                  statsOfNode[, node := curNode]
                  statsOfNode[, population := curPopName]
                  statsOfNode
  			})
            
            ##merge to one table
            statsOfGh<-rbindlist(statslist)
            
			statsOfGh
			
			
}
setMethod("getQAStats",signature("GatingSetList"),function(obj,...){
      res <- lapply(obj,function(gs){
            getQAStats(gs,...)
          })
      
      unlist(res,recursive = FALSE)
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
