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
#the convienient wrapper that does saveToDB,getQAStats,makeQaTask 3 steps in one call
qaPreprocess<-function(db,G,metaFile,checkListFile,fcs.colname="name")
{
	anno<-read.csv(metaFile)
#	browser()
	##associate the anno with gating set and save them in db
	saveToDB(db,G,anno,fcs.colname)
	#extract stats from gating set named as "G" that was stored in db
	getQAStats(db)
	
	qaTask.list<-makeQaTask(db,checkListFile)
	#return a task list
	qaTask.list
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
		if(!is.na(match(strTerm,levels(db$statsOfGS$stats))))
		{
			statsType=strTerm
			break
		}
	}
	
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
<<<<<<< HEAD
#TODO:to save multipe gating set and try to associate them to the stats table
saveToDB<-function(G,annoData)
{
	#####load sample info from xls

	
=======

saveToDB<-function(db,G,annoData,fcs.colname="name")
{
	#####load sample info from xls
	if(missing(annoData))
		annoData<-data.frame(name=getSamples(G))
>>>>>>> master
	annoData$id<-1:nrow(annoData)
	if(!fcs.colname%in%colnames(annoData))
		stop("column that specify FCS file names is missing in annotation data!")
	#rename the fcs filename column so that it can be fit into flowSet pData slot
	colnames(annoData)[which(colnames(annoData)==fcs.colname)]<-"name"

	#do some filtering for annoData
	annoData<-subset(annoData,name%in%getSamples(G))
	
		
	##fit it into GatingSet(or flowSet)
	rownames(annoData)<-annoData$name
	
	G<-G[which(getSamples(G)%in%annoData$name)]
	
	annoData<-annoData[getSamples(G),]	#sort by sample order in gh

	##extract tubeID from filename by stripping the first two prefix (presummably date and fileid on each tube)
	annoData$tubeID<-unlist(lapply(annoData$name,function(x){
#			browser()
						strsplit(
								paste(strsplit(as.character(x),"_")[[1]][c(-1,-2)],collapse="_")
								,"\\.")[[1]][[1]]
					}))


	pData(G)<-annoData
	#do the filtering for Gating set
	
	
	###append the data to db
	result<-try(colnames(getData(G[[1]])),silent=TRUE)
	if(!inherits(result,"try-error")){
		db$params<-result
	}
	
	db$G<-G
#	db$GroupOutlierResult<-db$outlierResult<-data.frame(sid=integer(),qaID=integer(),stringsAsFactors=F)
	db
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

#queryStats<-function(db,formula,Subset,pop=character(0),isReshape=FALSE)
queryStats<-function(db,Subset,statsType=NULL,pop=character(0),isTerminal=FALSE,fixed=FALSE)
{
#	browser()

	
	ret_anno<-pData(db$G)
	
	ret_stats<-db$statsOfGS
	
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
	
	ret<-merge(ret_stats,ret_anno,by.x="id",by.y="id")
	
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
	
	##apply the function to value in each group
#	if(!is.null(func))
#	{
#		factors<-lapply(groupBy,function(x){
#
#					eval(substitute(ret$v,list(v=x)))
#				})
##					browser()		
#		ret<-by(ret,factors,function(x){
##							browser()
#					x$value<-eval(substitute(f(x$value),list(f=func)))
#					x
#				})
#		ret<-do.call("rbind",ret)
#	}
	
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


