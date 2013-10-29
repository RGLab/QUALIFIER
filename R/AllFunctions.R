
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

#' Initializes the data environment
#' 
#' Initializes and prepares the data environment for storing the QA data
#' 
#' 
#' @param db An \code{environment} storing all the QA data. By default it is an hidden global environment \code{.db}.
#' 
#' @examples 
#' db <- new.env()
#' initDB(db)
#' @export
initDB <- function(db=.db){
	createDbSchema(db)
}

#' save/load the data environment to/from disk
#' 
#' save and load the data environment that contains both statistics and GatingSets.
#' 
#' 
#' @param db An \code{environment} storing all the QA data. By default it is an hidden global environment \code{.db}.
#' @param path \code{character} data path that stores the db.
#' @param cdf \code{character} the option to control cdf file operation. see \link{save_gs} for more details.
#' @param ... other arguments passed to \link{save_gs}
#' @examples
#' \dontrun{
#' save_db(db, path = "./PreprocessedData")
#' db <- load_db(path = "./PreprocessedData")
#' } 
#' @export
#' @aliases load_db save_db
#' @rdname save_db
save_db <- function(db = .db, path, overwrite = FALSE, cdf = "link",...){
  if (file.exists(path)) {
    path <- normalizePath(path, mustWork = TRUE)
    if (!overwrite) {
      stop(path, "' already exists!try to use overwrite = TRUE to overwrite it.")
    }
  }
  else {
    dir.create(path = path)
    path <- normalizePath(path, mustWork = TRUE)
  }
  
  message("saving db ...")
  saveRDS(db, file = file.path(path, "db.rds"))
#  browser()
  gsids <- db$gstbl[, "gsid"]
  nGS <- length(gsids)
  if(!setequal(1:nGS, gsids))
    stop("Can't save the corrupted db!")
  
  l_ply(gsids, function(gsid){
        message("saving gs ", gsid)
        suppressMessages( 
          save_gs(db$gs[[gsid]], file.path(path, gsid), cdf = cdf, overwrite = overwrite, ...)
        )
      })
  
  message("Done\nTo reload it, use 'load_db' function\n")
  
}
#' @export
#' @rdname save_db
load_db <- function(path){
  path <- normalizePath(path, mustWork = TRUE)
  if (!file.exists(path)) 
    stop(path, "' not found!")
  files <- list.files(path)
  message("loading db ...")
  db <- readRDS(file.path(path, "db.rds"))
  gsids <- db$gstbl[, "gsid"]
  l_ply(gsids, function(gsid){
        message("loading gs ", gsid)
        suppressMessages( 
            db$gs[[gsid]] <- load_gs(file.path(path, gsid))
        )
      })
  message("Done\n")
  db
}

#' Preprocessing for QA check
#' 
#' A convenient wrapper that does \link{saveToDB}, \link{getQAStats} in one call
#' 
#' @inheritParams initDB
#' @param gs A \code{\link[=GatingSet-class]{GatingSet}} containing multiple
#'  \code{gating hierarchies}
#' @param gs.name A character scalar giving the name of the GatingSet.
#' @param metaFile A character scalar giving the file path of the sample
#'  annotation data, which is a csv spreadsheet contains the meta information.
#'  Each row corresponds to one FCS file and The QUALIFIER package looks for the
#'  FCS filename from "name" column of the spreadsheet.
#' @param fcs.colname A character scalar indicating column name that specify FCS
#'  file names in annotation data.
#' @param date.colname A character scalar indicating column names that contains
#'  date information which are automatically formatted to "\%m/\%d/\%y".
#' @param ... other arguments passed to \link{getQAStats}
#' 
#' @return a list of elements stored in the data environment.
#'
#'@examples
#'
#'\dontrun{
#'#prepare the data environment
#'db<-new.env()
#'initDB(db)
#'
#'qaPreprocess(db=db,gs=G
#'			,metaFile=metaFile
#'			,fcs.colname="FCS_Files"
#'			,date.colname=c("RecdDt","AnalysisDt")
#'			)
#' 
#'}
#' @export 
qaPreprocess <- function(db=.db,gs,gs.name="default gatingSet",metaFile,fcs.colname="name",date.colname=NULL,...)
{
	
	##associate the anno with gating set and save them in db
	gsid <- saveToDB(db,gs,gs.name,metaFile,fcs.colname,date.colname)
		
	#extract stats from gating set named as "G" that was stored in db
#	browser()
	
	getQAStats(db,gsid,...)
	
	
	ls(db)
}

#' insert javascript into svg to enable interactity (e.g.tooltips, highlight and links)
#' @importFrom XML xmlTreeParse xmlRoot xmlNode xmlCDataNode addChildren saveXML
.postProcessSVG <- function(sfile)
{


	
	srcFile <- list.files(system.file("javascript",package="QUALIFIER")
                          , pattern="highlight.js"
                          , full.names=TRUE )
	srcFile <- file(srcFile, "r")
	srcCode <- readLines(srcFile)
	
	close(srcFile)

	doc <- xmlTreeParse(sfile, useInternalNodes = FALSE)
	
	top <- xmlRoot(doc)

	newNode <- xmlNode("script",attrs=c(type="text/ecmascript"))
	newNode <- addChildren(newNode, xmlCDataNode(paste(srcCode,collapse="\n")))
    top[["script"]] <- newNode		
	
    saveXML(top, sfile)

    
}

matchStatType <- function(db,formuRes)
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




.isRoot<-function(gh,node)
{
#	return(ifelse(length(getParent(gh,node))==0,TRUE,FALSE))
	node=="root"
}



#cell number(first node in gating hierachy) marginal events and MFI are also based on sub-populations defined by manual gates
#which are extracted during the batch process of storing % and MFI

#' Save the gating set and annotation data into the data environment.
#' 
#' Save the gating set and annotation data into the data environment.
#'
#' @inheritParams qaPreprocess
#' 
#' @return An unique id for \code{GatingSet} that is generated incrementally.
#'@examples
#'
#'\dontrun{
#'#prepare the data environment
#'db<-new.env()
#'initDB(db)
#'
#'metaFile="~/rglab/workspace/QUALIFIER/misc/ITN029ST/FCS_File_mapping.csv"
#'##append the annotation  and Gating set to db
#'metaFile<-"FCS_File_mapping.csv"
#'saveToDB(db=db,gs=G
#'		,metaFile=metaFile
#'		,fcs.colname="FCS_Files"
#'		,date.colname=c("RecdDt","AnalysisDt")
#'	) 
#'
#'}

#' @export 
saveToDB<-function(db=.db,gs,gs.name="default gatingSet",metaFile,fcs.colname="name",date.colname=NULL)
{
	
	
    idColName <-qa.par.get("idCol")
    
	annoData<-pData(gs)
	if(is.na(match("name",colnames(annoData))))
		stop("'name' column is missing from pData of GatingSet!")
		
	if(!missing(metaFile))
	{
		dt <- fread(metaFile)
        annoData_csv <- as.data.frame(dt)
		annoData<-merge(annoData,annoData_csv,by.x="name",by.y=fcs.colname)
	}
#browser()
    #generate id column if not present
	if(!idColName%in%colnames(annoData))
	  annoData[,idColName] <- 1:nrow(annoData)
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
	annoData<-subset(annoData,name%in%sampleNames(gs))
	annoData<-droplevels(annoData)
		
	##fit it into GatingSet(or flowSet)
	rownames(annoData)<-annoData$name
	
	gs<-gs[which(sampleNames(gs)%in%annoData$name)]
	
	annoData<-annoData[sampleNames(gs),]	#sort by sample order in gh

	
	

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

#' match the population by a cerntain criteria
#' 
#' @param pattern character population pattern to match, can be one of the four \code{type}s
#' @param nodePath character a vector of population nodes to match with 
#' @param type character specifing how the pattern is matched
#' \itemize{
#'  \item regExpr: passes it as a regular expression to grepl (fixed = FALSE), it is flexible enough for the advance users to define any type of qa tasks. (e.g. "/(4|8)\+$" for "4+" and "8+", but not "CD154+" )
#'                  for the users who don't know about regular expressions, type can be set to one of the following three options
#'  \item popName: interprets the pattern as the exact population name character and do the strict matching with terminal node, (e.g. "L" for lymph populations but not live/dead "Lv")
#'  \item subPath: will do the partial path match (e.g. "4+ for "4+" and all its downstream children: "4+/TNFa+", "4+/IL2+" etc... )
#'  \item fullPath: will do the full path match (e.g. "/S/Lv/L/3+/Excl/4+" will only be matched to "4+")
#' }
#' @return \code{logical} vector as the matching result
#' @examples 
#' \dontrun{
#'  nodes <- getNodes(gh, isPath = TRUE) #fetch all the population (with path) from gating hierarchy
#'  nodes
#'    
#'  # exact match by population name (terminal/base name in the path)
#'  nodes[.matchNode("root", nodes, type ="popName")]
#'  nodes[.matchNode("Lv", nodes, type ="popName")] 
#'  nodes[.matchNode("MNC", nodes, type ="popName")]
#'  nodes[.matchNode("WBC_perct", nodes, type ="popName")]
#'  
#'  #partial match to the path
#'  nodes[.matchNode("MFI", nodes, type ="subPath")]
#'  nodes[.matchNode("margin", nodes, type ="subPath")]
#' 
#'  nodes[.matchNode("4+/TNFa+", nodes, type ="subPath")]
#'  nodes[.matchNode("8+", nodes, type ="subPath")]
#'  
#'  #regular expression match
#'  nodes[.matchNode("/(4|8)\\+$", nodes, type ="reg")]
#'  nodes[.matchNode("4\\+/(IFNg|IL2|IL4|IL17a|TNFa)\\+$", nodes, type ="reg")]
#'  nodes[.matchNode("/S/Lv/L/3+/Excl/4+/TNFa+", nodes, type ="fullPath")]
#'  
#'  }
.matchNode <- function(pattern, nodePath, type = c("regExpr", "fullPath", "subPath", "popName"))
{
#browser()
    type <- match.arg(type)
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

#' \code{queryStats} method queries stats entries from db by qaTask object and formula
#' @export 
#' @rdname qaCheck-methods
#' @aliases queryStats,qaTask-method
setMethod("queryStats", signature=c(x="qaTask"),
		function(x,y,subset,pop,gsid=NULL,...){
			
			if(missing(y))
				y<-getFormula(x)
			db<-getData(x)
			formuRes<-flowWorkspace:::.formulaParser(y)
			#determine the statsType(currently only one of the terms can be statType,we want to extend both in the future)
			statsType<-matchStatType(db,formuRes)
			if(missing(pop))
				pop<-getPop(x)
			
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
		r <- .matchNode(pop,ret_stats$population, ...)
		ret_stats <- ret_stats[r,]
	}
#	browser()
	if(!is.null(statsType))
		ret_stats <- subset(ret_stats,stats%in%statsType)
	
	ret <- merge(ret_stats,ret_anno,by=c("gsid",qa.par.get("idCol")))
    
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

# this routine keeps the original schema by replacing the stats value with aggregated value
applyFunc <- function(data,term,func,groupBy)
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


