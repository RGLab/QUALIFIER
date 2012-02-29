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

.postProcessSVG<-function(sfile)
{

#	browser()
	
	srcFile<-list.files(system.file("javascript",package="QUALIFIER"),pattern="highlight.js",full=T)
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
#TODO:refere to latticeParseFormula for more generic parser

.formulaParser<-function(formula)
{
#	browser()
	
	
	bTerm<-formula[[3]]
	cond<-""
	if(length(bTerm)>1)
	{
		xTerm<-bTerm[[2]]
		cond<-bTerm[[3]]
	}else
	{
		xTerm<-bTerm
	}
	
	if(length(cond)>1)
	{
		groupBy<-as.character(cond)[-1]
	}else
	{
		if(length(bTerm)>1)
		{
			groupBy<-as.character(cond)	
		}else
		{
			groupBy<-NULL
		}
		
	}
	
	yTerm<-formula[[2]]
	func<-NULL
	if(length(yTerm)>1)
	{
		func<-yTerm[[1]]
		yTerm<-yTerm[[2]]
	}
	
	list(xTerm=xTerm,yTerm=yTerm,func=func,groupBy=groupBy)
}
.isRoot<-function(gh,node)
{
	return(ifelse(length(getParent(gh,node))==0,TRUE,FALSE))
}



#cell number(first node in gating hierachy) marginal events and MFI are also based on sub-populations defined by manual gates
#which are extracted during the batch process of storing % and MFI

saveToDB<-function(db,G,annoData)
{
	#####load sample info from xls
#	annoData<-read.csv(metaFile, as.is=TRUE)
	
	###import global ID for each fcs from labkey
#	ncfs<-ncFlowSet(G)
#	objID.table<-ncfs@phenoData@data
#	objID.table$id<-1:nrow(objID.table)
#	annoData<-merge(annoData,objID.table,by.x="FCS_Files",by.y="name")
	
	annoData$id<-1:nrow(annoData)
	if(!"name"%in%colnames(annoData))
		stop("'name' column that stores FCS file names is missing in annotation data!")
#	browser()
	#do some filtering for annoData
#	annoData<-subset(annoData,!annoData$Tube%in%c("EMA/EMA/EMA/EMA/EMA","VD1/VD2/GD/BLK/CD3"))
	annoData<-subset(annoData,name%in%getSamples(G))
	
	#do some format converting
#	annoData$RecdDt<-as.Date(annoData$RecdDt,"%m/%d/%y")
#	annoData$AnalysisDt<-as.Date(annoData$AnalysisDt,"%m/%d/%y")

		
	##fit it into GatingSet(or flowSet)
#	colnames(annoData)[which(colnames(annoData)=="FCS_Files")]<-"name"
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

	db$params<-colnames(getData(G[[1]]))
	db$G<-G
	db$GroupOutlierResult<-db$outlierResult<-data.frame(sid=integer(),qaID=integer(),stringsAsFactors=F)
	db
}



makeQaTask<-function(db,checkListFile)
{
#	db$qaCheckList<-read.csv(checkListFile)
	
	qaTask.list<-apply(db$qaCheckList,1,function(curRow,db){
#browser()			
				curQa<-new("qaTask"
						,qaID=as.integer(curRow["qaID"])
						,qaName=curRow["qaName"]
						,description=curRow["description"]
						,qaLevel=curRow["qaLevel"]
						,pop=curRow["pop"]
						,formula=as.formula(curRow["formula"])
						,plotType=curRow["plotType"]
						,db=db
				)
				curQa					
			},db)
	names(qaTask.list)<-db$qaCheckList$qaName
	qaTask.list
}

queryStats<-function(db,formula,Subset,pop=character(0))
{
	
	formuRes<-.formulaParser(formula)
	
	yTerm<-formuRes$yTerm
	func<-formuRes$func
	groupBy<-formuRes$groupBy

	
	statsType<-as.character(yTerm)
	
	if(tolower(statsType)=="percent")##take percent as the same as proportion
		statsType="proportion"
	ret_anno<-pData(db$G)
	
	ret_stats<-db$statsOfGS
	
	
	#filter by subset ,use eval instead of subset since subset is now a filtering argument instead of the function 
	if(length(pop)!=0)
	{
		ret_stats <-subset(ret_stats,grepl(pop,population))
	}
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
#	if(!is.null(subset))
#	{
#		ret<-subset(ret,eval(parse(text=subset)))	
#	}
	if(!missing(Subset))
	{
		r <- eval(Subset, ret)
		if(!is.logical(r)) stop("'Subset' must evaluate to logical")
		ret <- ret[r,]
	}
	##apply the function to value in each group
	if(!is.null(func))
	{
		factors<-lapply(groupBy,function(x){

					eval(substitute(ret$v,list(v=x)))
				})
#					browser()		
		ret<-by(ret,factors,function(x){
#							browser()
					x$value<-eval(substitute(f(x$value),list(f=func)))
					x
				})
		ret<-do.call("rbind",ret)
	}
	
	ret
	
	
}





