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
		if(!is.na(match(strTerm,unique(db$stats$stats))))
		{
			statsType=strTerm
			break
		}
	}
	if(is.null(statsType))
		stop("formula does not contain valid stats type!")
	return(statsType)
}

##recursively parsing conditional variables
.parseCond<-function(cond){
#			browser()
	groupBy<-NULL
	if(length(cond)==1)
		groupBy<-as.character(cond)
	else
	{
		for(i in 1:length(cond))
		{
			curCond<-cond[[i]]
#				browser()
			if(length(curCond)==3)
			{
				res<-.parseCond(curCond)
				groupBy<-c(res,groupBy)
			}else
			{
				curCond<-as.character(curCond)
				if(!curCond%in%c(":","*","+"))
					groupBy<-c(groupBy,curCond)	
			}
			
		}	
	}
	
	groupBy
}

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
		groupBy<-.parseCond(cond)
		
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
#	return(ifelse(length(getParent(gh,node))==0,TRUE,FALSE))
	node=="root"
}



#cell number(first node in gating hierachy) marginal events and MFI are also based on sub-populations defined by manual gates
#which are extracted during the batch process of storing % and MFI

saveToDB<-function(db=.db,gs,gs.name="default gatingSet",metaFile,fcs.colname="name",date.colname=NULL)
{
	
	
	
	annoData<-pData(gs)
	if(is.na(match("name",colnames(annoData))))
		stop("'name' column is missing from pData of GatingSet!")
		
	if(!missing(metaFile))
	{
		annoData_csv<-read.csv(metaFile)
		annoData<-merge(annoData,annoData_csv,by.x="name",by.y=fcs.colname)
	}
		
	
	annoData$id<-1:nrow(annoData)
#		browser()
#	if(!fcs.colname%in%colnames(annoData))
#		stop("column that specify FCS file names is missing in annotation data!")
#	#rename the fcs filename column so that it can be fit into flowSet pData slot
#	colnames(annoData)[which(colnames(annoData)==fcs.colname)]<-"name"

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
	annoData<-droplevels(annoData)
		
	##fit it into GatingSet(or flowSet)
	rownames(annoData)<-annoData$name
	
	gs<-gs[which(getSamples(gs)%in%annoData$name)]
	
	annoData<-annoData[getSamples(gs),]	#sort by sample order in gh

	
	

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


#' @param type character specifing how the pattern is matched
#' "regExpr", passes it as a regular expression to grepl (fixed = FALSE), it is flexible enough for the advance users to define any type of qa tasks. (e.g. "/(4|8)\+$" for "4+" and "8+", but not "CD154+" )
#' for the users who don't know about regular expressions, type can be set to one of the following three options
#' "popName" interprets the pattern as the exact population name character and do the strict matching with terminal node, (e.g. "L" for lymph populations but not live/dead "Lv")
#' "subPath" will do the partial path match (e.g. "4+ for "4+" and all its downstream children: "4+/TNFa+", "4+/IL2+" etc... )
#' "fullPath" will do the full path match (e.g. "/S/Lv/L/3+/Excl/4+" will only be matched to "4+")
matchNode<-function(pattern, nodePath, type = c("regExpr", "fullPath", "subPath", "popName"))
{
#browser()
    type <- match.arg(type,c("regExpr", "fullPath", "subPath", "popName"))
    nodePath <- as.character(nodePath)
#       browser()
	#when pattern starts as slash, then assume it is a full path match instead of the substring match
	if(type == "fullPath")
    {
      nodesToMatch <- nodePath
      nodesToMatch%in%pattern
    }else if(type == "subPath"){
      nodesToMatch <- nodePath
      grepl(pattern,nodesToMatch, fixed = TRUE)
    }else if(type == "popName"){
      nodesToMatch <- basename(nodePath)
      nodesToMatch%in%pattern
    }else if (type == "regExpr"){
        nodesToMatch <- nodePath
        grepl(pattern,nodesToMatch)  
     } 
      

  	
}

##API to query stats entries from db by qaTask object and formula
setMethod("queryStats", signature=c(x="qaTask"),
		function(x,y,subset,pop,gsid=NULL,...){
			
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
				res<-.queryStats(db,statsType=statsType,pop=pop,gsid=gsid, ...)
			else
				res<-.queryStats(db,statsType=statsType,substitute(subset),pop=pop,gsid=gsid, ... )
			
			if(nrow(res)!=0)
			{
				
				#append the outlier flag
				res$outlier<-res$sid%in%base::subset(db$outlierResult,qaID==qaID(x))$sid
				res$gOutlier<-res$sid%in%base::subset(db$GroupOutlierResult,qaID==qaID(x))$sid	
				
			}
			
			
			res
		})

.queryStats<-function(db,Subset,statsType=NULL,pop=character(0),gsid, ...)
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
		r<-matchNode(pop,ret_stats$population, ...)
		ret_stats <-ret_stats[r,]
	}
#	browser()
	if(!is.null(statsType))
		ret_stats<-subset(ret_stats,stats%in%statsType)
	
	ret<-merge(ret_stats,ret_anno,by.x=c("gsid","id"),by.y=c("gsid","id"))
	
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


