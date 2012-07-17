# TODO: Add comment
# 
# Author: mike
###############################################################################
#.postProcessBoxPlotSVG<-function(sfile)
#{
#	doc = xmlParse(sfile)
#	top<-xmlRoot(doc)
#	nodes<-xmlChildren(top)
#	toMove<-nodes[[length(nodes)-1]]
#	removeChildren(top,list(length(nodes)-1))##remove the node from back
#	toMove<-toMove[[1]]##remove link
#	removeChildren(toMove,list(1))##remove title
#	addChildren(top,kids=list(toMove),at=5)##add it to top
#	saveXML(top,sfile )
#	
#}

initDB<-function(db=.db){
	createDbSchema(db)
}
#the convienient wrapper that does saveToDB,getQAStats,makeQaTask 3 steps in one call
qaPreprocess<-function(db=.db,gs,gs.name="default gatingSet",metaFile,fcs.colname="name",date.colname=NULL,...)
{
	
	##associate the anno with gating set and save them in db
	gsid<-saveToDB(db,gs,gs.name,metaFile,fcs.colname,date.colname)
		
	#extract stats from gating set named as "G" that was stored in db
#	browser()
	
	getQAStats(db,gsid,...)
	
	
	ls(db)
}
.postProcessSVG<-function(sfile)
{

#	browser()
	
	srcFile<-list.files(system.file("javascript",package="QUALIFIER"),pattern="highlight.js",full.names=TRUE)
	srcFile<-file(srcFile, "r")
	srcCode<-readLines(srcFile)
	
	close(srcFile)
	
	doc = xmlParse(sfile)
	
	top<-xmlRoot(doc)
	
	
	oldNode<-top[["script"]]
	newNode<-xmlNode("script",attrs=c(type="text/ecmascript"))
	newNode<-addChildren(newNode,xmlCDataNode(paste(srcCode,collapse="\n")))
	
	replaceNodes(oldNode,newNode)
		
	saveXML(top,sfile)
	
}

matchStatType<-function(db,formuRes)
{
#	browser()
	statsType<-NULL
	for(CurTerm in c("xTerm","yTerm"))
	{
		strTerm<-as.character(formuRes[[CurTerm]])
		if(!is.na(match(strTerm,levels(db$stats$stats))))
		{
			statsType=strTerm
			break
		}
	}
	if(is.null(statsType))
		stop("formula does not contain valid stats type!")
	return(statsType)
}
#TODO:refere to latticeParseFormula for more generic parser

.formulaParser<-function(formula)
{
#	browser()
	
	#parse the b term
	bTerm<-formula[[3]]
	cond<-NULL
	if(length(bTerm)>2)
	{
		xTerm<-bTerm[[2]]
		cond<-bTerm[[3]]
	}else
	{
		xTerm<-bTerm
	}
#	browser()
	##parse the conditional variable
	if(!is.null(cond))
	{
		if(length(cond)>1)
			groupBy<-as.character(cond)[-1]
		else
			groupBy<-as.character(cond)	
		
	}else
	{
		
		groupBy<-NULL
	}
	
	#parse the xterm
	xfunc<-NULL
	if(length(xTerm)==2)
	{
		xfunc<-xTerm[[1]]
		xTerm<-xTerm[[2]]
	}else
	{
		if(length(xTerm)>=3)
			stop("not supported formula!")
	}
	
	
	yTerm<-formula[[2]]
	yfunc<-NULL
	if(length(yTerm)==2)
	{
		yfunc<-yTerm[[1]]
		yTerm<-yTerm[[2]]
	}else
	{
		if(length(yTerm)>=3)
			stop("not supported formula!")
	}
	
	list(xTerm=xTerm,yTerm=yTerm,xfunc=xfunc,yfunc=yfunc,groupBy=groupBy)
}
.isRoot<-function(gh,node)
{
	return(ifelse(length(getParent(gh,node))==0,TRUE,FALSE))
}



#cell number(first node in gating hierachy) marginal events and MFI are also based on sub-populations defined by manual gates
#which are extracted during the batch process of storing % and MFI

saveToDB<-function(db=.db,gs,gs.name="default gatingSet",metaFile,fcs.colname="name",date.colname=NULL)
{
	

	#####load sample info from xls
	if(missing(metaFile))
		annoData<-data.frame(name=getSamples(gs))
	else
		annoData<-read.csv(metaFile)
	
	annoData$id<-1:nrow(annoData)
#		browser()
	if(!fcs.colname%in%colnames(annoData))
		stop("column that specify FCS file names is missing in annotation data!")
	#rename the fcs filename column so that it can be fit into flowSet pData slot
	colnames(annoData)[which(colnames(annoData)==fcs.colname)]<-"name"

	#format date columns
#	browser()
	if(!is.null(date.colname))
	{
		if(!all(date.colname%in%colnames(annoData)))
			warning("date column not found in annotation data!")
		else
			annoData[,date.colname]<-sapply(annoData[,date.colname,drop=F],function(x){
#																			browser()
																			as.Date(as.character(x),"%m/%d/%y")
																		}
											,simplify=FALSE)
					
	}
	
	
	#do some filtering for annoData
	annoData<-subset(annoData,name%in%getSamples(gs))
	
		
	##fit it into GatingSet(or flowSet)
	rownames(annoData)<-annoData$name
	
	gs<-gs[which(getSamples(gs)%in%annoData$name)]
	
	annoData<-annoData[getSamples(gs),]	#sort by sample order in gh

	##extract tubeID from filename by stripping the first two prefix (presummably date and fileid on each tube)
	annoData$tubeID<-unlist(lapply(annoData$name,function(x){
#			browser()
						strsplit(
								paste(strsplit(as.character(x),"_")[[1]][c(-1,-2)],collapse="_")
								,"\\.")[[1]][[1]]
					}))


	pData(gs)<-annoData
	#do the filtering for Gating set
	
	
	###append the data to db
	result<-try(colnames(getData(gs[[1]])),silent=TRUE)
	if(!inherits(result,"try-error")){
		db$params<-result
	}
	

	if(nrow(db$gstbl)==0)
		gsid<-1
	else
		gsid<-max(db$gstbl$gsid)+1
	db$gstbl<-rbind(db$gstbl,data.frame(gsid=gsid,gsname=gs.name))
#	browser()	
	db$gs[[gsid]]<-gs
	gsid
}





matchNode<-function(pattern,nodePath,isTerminal=FALSE,fixed=FALSE)
{
#	browser()
	#when pattern starts as slash, then assume it is a full path match instead of the substring match
	if(substr(pattern,1,1)=="/")
		return(pattern==nodePath)
#	browser()
#get the positions of the parttern matched in the gate path
	if(isTerminal)
		grepl(pattern,basename(as.character(nodePath)),fixed=fixed)
#		posList<-gregexpr(pattern,nodePath,fixed=fixed)
	else
		grepl(pattern,nodePath,fixed=fixed)
	
#	unlist(lapply(1:length(posList),function(i){
#				pos<-posList[[i]]
#				curNode<-as.character(nodePath[[i]])
#				if(length(pos)==1&&pos==-1)
#					return(FALSE)
#				else
#				{
#					if(isTerminal)#if matched as a terminal node,do the further check on the slash
#					{
#						res<-unlist(lapply(pos,function(x){
#											#check the existence of slash after the pattern
#											toMatch<-substring(curNode,x+1,nchar(curNode))
#											!grepl("/",toMatch)
#										}))
#						return(any(res))#return true if any matched instance satifsfy the terminal check
#					}else
#					{
#						return(TRUE) #if mathced as non-terminal node, then return true once it is matched anywhere in the path
#					}
#					
#				}
#			}))
	
	
	
	
}

##API to query stats entries from db by qaTask object and formula
setMethod("queryStats", signature=c(x="qaTask"),
		function(x,y,subset,pop,isTerminal=TRUE,fixed=FALSE,gsid=NULL,...){
			
			if(missing(y))
				y<-getFormula(x)
			db<-getData(x)
			formuRes<-.formulaParser(y)
			#determine the statsType(currently only one of the terms can be statType,we want to extend both in the future)
			statsType<-matchStatType(db,formuRes)
			if(missing(pop))
				pop<-getPop(x)
#			browser()
			if(missing(subset))
				res<-.queryStats(db,statsType=statsType,pop=pop,isTerminal=isTerminal,fixed=fixed,gsid=gsid)
			else
				res<-.queryStats(db,statsType=statsType,substitute(subset),pop=pop,isTerminal=isTerminal,fixed=fixed,gsid=gsid)
			
			if(nrow(res)!=0)
			{
				
				#append the outlier flag
				res$outlier<-res$sid%in%base::subset(db$outlierResult,qaID==qaID(x))$sid
				res$gOutlier<-res$sid%in%base::subset(db$GroupOutlierResult,qaID==qaID(x))$sid	
				
			}
			
			
			res
		})
#queryStats<-function(db,formula,Subset,pop=character(0),isReshape=FALSE)
.queryStats<-function(db,Subset,statsType=NULL,pop=character(0),isTerminal=FALSE,fixed=FALSE,gsid)
{
#	browser()

	if(is.null(gsid))
	{
		ret_anno<-lapply(1:length(db$gs),function(i){
									
									meta<-pData(db$gs[[i]])
									meta$gsid=i
									meta
							})
		ret_anno<-do.call(rbind,ret_anno)
					
	}else	
	{
		ret_anno<-pData(db$gs[[gsid]])
		ret_anno$gsid=gsid
	}
	ret_stats<-db$stats
	
#	browser()
	#filter by subset ,use eval instead of subset since subset is now a filtering argument instead of the function 
	if(length(pop)!=0)
	{
		r<-matchNode(pop,ret_stats$population,isTerminal,fixed)
		ret_stats <-ret_stats[r,]
	}
#	browser()
	if(!is.null(statsType))
		ret_stats<-subset(ret_stats,stats%in%statsType)
	
	ret<-merge(ret_stats,ret_anno,by.x=c("gsid","id"),by.y=c("gsid","id"))
	
	##add stain column from tube and channel
	ret$stain<-apply(ret,1,function(x){
				curChannel<-as.character(x["channel"])
#									browser()
				
				if(is.na(curChannel)||curChannel%in%db$params[1:2])
				{	
					curStain<-NA
				}else
				{
					chnlInd<-which(db$params[3:7]==curChannel)
					curStain<-strsplit(x["Tube"],"\\/")[[1]][chnlInd]
				}
				curStain
			})

#	browser()
	#filter by subset 

	if(!missing(Subset))
	{
		r <- eval(Subset, ret)
		if(!is.logical(r)) stop("'Subset' must evaluate to logical")
		ret <- ret[r,]
	}
#		browser()
	
	
	ret
	
	
}

#this routine keeps the original schema by replacing the stats value with aggregated value
applyFunc<-function(data,term,func,groupBy)
{
#			browser()
	factors<-lapply(groupBy,function(x){
				
				eval(substitute(data$v,list(v=x)))
			})
	#					browser()		
	data<-by(data,factors,function(x){
				x[,term]<-eval(substitute(f(x$stats),list(f=func,stats=term)))
				x
			})
	do.call("rbind",data)
}


