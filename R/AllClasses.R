#'QUALIFIER: A package that provides automated flow data quality assessment
#'based on gated cell populations
#'
#'The package provide two important methods:\code{qaCheck} and \code{plot},
#'which allows users to use \code{formula} as a general and flexible way to
#'specify the tubes,channels,statistics and gated populations to perform
#'differnt QA tasks.
#'
#'\tabular{ll}{ Package: \tab QUALIFIER\cr Version: \tab 0.99.1\cr Date:\tab
#'2011-12-02\cr Depends: \tab R (>= 2.14), flowCore\cr License: \tab
#'Artistic-2.0\cr
#'
#'}
#'
#'@name QUALIFIER-package
#'@aliases QUALIFIER-package QUALIFIER
#'@docType package
#'@author Mike Jiang,Greg Finak
#'
#'Maintainer: Mike Jiang <wjiang2@@fhcrc.org>
#'@references \url{http://www.rglab.org/}
#'@keywords package
NULL


#' a class for storing important information of flow cytometry data quality assessment task
#'
#' This class stores the meta information, the name of gated cell population
#' associated with the QA task and the formula describing how the QA task is
#' performed.
#'
#' \describe{
#'    \item{\code{qaID}:}{A integer for the id of qaTask object}
#'    \item{\code{qaName}:}{A character containing the QA task name }
#'    \item{\code{description}:}{OA character containing the description of QA task.}
#'    \item{\code{qaLevel}:}{A character vector containing QA task level, which is displayed in the html report.}
#'    \item{\code{pop}:}{A character containing the name of the cell population ,which is also equivalent to the name of the node in the gating hierarchy tree.
#'	                            It basically tells qaCheck or plot methods the particular gated cell population from which statistics are extracted. }
#'    \item{\code{formula}:}{a formula describing how the QA task is performed. 
#'    						see \code{\link[=qaCheck,qaTask-method]{qaCheck}} for more details. } 
#'    \item{\code{type}:}{a character indicate how pop is matched.}
#'    \item{\code{subset}:}{an expression specifying a subset of data to be applied QA task.}       
#'    \item{\code{plotType}:}{A character indicating how the QA result is plotted. Currently only "xyplot" and "bwplot" are supported.}
#'    \item{\code{width}:}{A numeric scalar indicating the  width of svg figure .}
#'    \item{\code{height}:}{A numeric scalar indicating the  height of svg figure .}
#'    \item{\code{par}:}{A list storing all the lattice arguments to control the appearance of the plot. See \code{\link{xyplot}} for details.}
#'    \item{\code{scatterPar}:}{A list storing the arguments to control the appearance of 
#'    							the individual plot.
#'    							Example scatterPar value:
#'    							list(type="densityplot",scales=list(x=list(log=TRUE)))
#'    							this will set the log scale for x axis of the 1D-densityplot
#'    							}
#'    \item{\code{htmlReport}:}{A logical scalar indicating whether to plot the task when \code{\link{qaReport}} is called. 
#'							    By default, when there are no outliers detected for the qaTask,it is not plotted in the report.
#'							    But sometime it is helpful to still look at the plot with the trend of the data like MFI stability over time.
#'							    So by setting this flag to TRUE, we force this task to be plotted anyway. }
#'   \item{\code{highlight}:}{A character scalar indicating the level on which the dots in svg output should be highlighted when they are hovered over by mouse.
#'   							It is one of the visualization feature provided by svg format that helps to identify a couple of dots that share the same information (like FCS name,or sample ID).
#'   							It should be a valid column name in the metaFile (see \code{\link{getQAStats} for more details about the meta file}.
#'   							}
#'   \item{\code{rFunc}:}{A regression function passed to some qa tasks to monitor the long term trend.}
#'   \item{\code{outlierFunc}:}{An outlier detection function.}
#'   \item{\code{outlierFunc_args}:}{named arguments passed to the outlier detection function.}
#'   \item{\code{goutlierFunc}:}{A group outlier detection function.} 
#'   \item{\code{goutlierFunc_args}:}{named arguments passed to the group outlier detection function.}
#'    \item{\code{db}:}{An environment containing the database connection, which stores the gating hierarchy,QA task list,sample information and outliers detection results. .}

#'  }
#' 
#'@aliases 
#' qaTask-class
#' qaTask 
#' qaID,qaTask 
#' getName,qaTask 
#' description,qaTask 
#' qaLevel,qaTask 
#' getPop,qaTask 
#' getData,qaTask 
#' formula,qaTask
#' plotType,qaTask 
#' qpar,qaTask-method 
#' qpar<-,qaTask,list-method 
#' qpar 
#' qpar<-
#' scatterPar 
#' scatterPar<- 
#' scatterPar,qaTask-method
#' scatterPar<-,qaTask,list-method
#' htmlReport 
#' htmlReport<-
#' htmlReport,qaTask-method 
#' htmlReport<-,qaTask,logical-method 
#' highlight
#' highlight<- 
#' highlight,qaTask-method 
#' highlight<-,qaTask,character-method
#' rFunc,qaTask-method 
#' rFunc 
#' rFunc<- 
#' rFunc<-,qaTask,ANY-method
#' rFunc<-,qaTask-method 
#' show,qaTask 
#'@docType class
#'
#'@section Objects from the Class: Objects can be created by using constructor:
#'read.qaTask(db=.db,checkListFile)
#'@author Mike Jiang,Greg Finak
#'
#'Maintainer: Mike Jiang <wjiang2@@fhcrc.org>
#'@seealso \code{\link{qaCheck}},\code{\link{plot}},\code{\link{qaReport}}
#'@keywords classes
#' @exportClass qaTask
#' @rdname qaTask-class
setClass("qaTask",
		representation(qaID="integer"
						,qaName="character"
						,description="character"
						,qaLevel="character"
						,pop="character"
						,formula="ANY"
                        ,type="character" # indicate how pop is matched
                        ,subset = "ANY"
						,plotType="character"
						,width="numeric"
						,height="numeric"
						,par="list"#arguments for lattice plot
						,scatterPar="list"#arguments for indivdiual plot
						,htmlReport="logical"#decide wether to plot even without outliers detected
						,rFunc="ANY"
                        ,outlierFunc="ANY"
                        ,goutlierFunc="ANY"
                        ,outlierFunc_args="ANY"
                        ,goutlierFunc_args="ANY"
						,highlight="character"#argument to determine level on which the dot will be higtlighted when hoverover in svg plot(like FCS,or sampleID ,should be the column name in the meta data) 
						,db="ANY"
						),
		prototype=list(qaID=integer(0)
						,qaName=character(0)
						,description=character(0)
						,qaLevel=character(0)
						,pop=character(0)
						,formula="ANY"
                        ,type = "popName"
                        ,subset = "ANY" 
						,plotType="xyplot"
						,width=10
						,height=10
						,par=list(horiz=FALSE
								,scales=list(format="%m/%d/%y")
								)
						,scatterPar=list(type="xyplot"
										,smooth=FALSE
										,stat=TRUE)
						,htmlReport = FALSE
						,rFunc = NULL
						,highlight = NULL
                        ,outlierFunc = NULL
                        ,goutlierFunc = NULL
                        ,outlierFunc_args = NULL
                        ,goutlierFunc_args = NULL
						,db="ANY"
						)
		)

		

# constructor that stores the qaTask into the db table
makeQaTask <- function(db=.db,qaName,description,qaLevel,pop,formula, type, plotType)
{
	qa<-new("qaTask"
						,qaID=max(db$qaChecklist)
						,qaName=qaName
						,description=description
						,qaLevel=qaLevel
						,pop=pop
						,formula=as.formula(formula)
                        ,type=type
						,plotType=plotType
						,db=db
				)
	
	qa
}

#' Loading \code{qaTask}s from a csv file
#' 
#' The csv file contains the definition of one \code{qaTask}s
#' @param checkListFile A character scalar giving the file path, which is a csv
#'                        spreadsheet contains the detailed information of each QA task. It should have
#'                        the columns: 'qaID', 'qaName', 'description',	'qaLevel','pop', 'type', 'formula', 'subset', 'plotType'. 
#'                          See the slots of \code{\link{qaTask-class}} for more details.
#' @param ... other arguments
#' \itemize{
#'  \item db \code{environment}  See the slots of \code{\link{qaTask-class}} for more details.
#' } 
#' 
#' @return a list of \code{qaTask} objects
#' @aliases read.qaTask
#' @export
#' @examples 
#' \dontrun{
#'  checkListFile <- file.path(system.file("data", package = "QUALIFIER"), "qaCheckList.csv.gz")
#'  qaTask.list <- read.qaTask(db, checkListFile)
#'  qaTask.list[[1]]
#' }
read.qaTask <-function(checkListFile, ...)
{
  df<-read.csv(checkListFile)
  .read.qaTask(df,...)
}

#' construct qatask from a data.frame
.read.qaTask<-function(df,db=.db)
{
	
	db$qaTaskTbl<-df
	qaTask.list<-apply(df,1,function(curRow,db){
                          
                          plotType <- curRow["plotType"]
                        
                          filter <- gsub("^\\s*|\\s*$", "", curRow["subset"]) #trim the leading and trailing space
                          if(!is.na(filter))
                          {
                            if(nchar(filter) > 0)
                              filter <- parse(text = filter)
                            else
                              filter  <- NULL
                          }else
                            filter  <- NULL
                          #parse outlier functions                          
                          outlierFunc_args <- goutlierFunc_args <- list()
                          
                          outlierFunc <- curRow["outlierFunc"]
                          outlierFunc <- eval(parse(text = outlierFunc))
                          if(is.function(outlierFunc))
                          {
                            
                            outlierFunc_args <- curRow["outlierFunc_args"]
                            
                            if(!is.na(outlierFunc_args))
                              outlierFunc_args <- openCyto:::.argParser(outlierFunc_args)
                            
                          }else{
                            message("Outlier function is not specified!")
                            if(plotType=="bwplot")
                            {
                              outlierFunc <- qoutlier
                              message("'qoutlier' will be used as default outlier function.")
                            }else
                            {
                              outlierFunc <- outlier.norm
                              message("'outlier.norm' will be used as default outlier function.")
                            }
                            
                          }
                          
                          
                          goutlierFunc <- curRow["goutlierFunc"]
                          goutlierFunc <- eval(parse(text = goutlierFunc))
                          if(is.function(goutlierFunc))
                          {
                            goutlierFunc_args <- curRow["goutlierFunc_args"]
                            if(!is.na(aoutlierFunc_args))
                              outlierFunc_args <- openCyto:::.argParser(outlierFunc_args)
                             
                          }else{
                            goutlierFunc <- outlier.norm
                          }
                          
                          
                          
                    		curQa<-new("qaTask"
                        				,qaID = as.integer(curRow["qaID"])
                        				,qaName = curRow["qaName"]
                        				,description = curRow["description"]
                        				,qaLevel = curRow["qaLevel"]
                        				,pop = curRow["pop"]
                        				,formula = as.formula(curRow["formula"])
                                        ,type = curRow["type"]
                                        ,subset = filter
                        				,plotType = plotType
                                        ,highlight = qa.par.get("idCol")
                                        ,outlierFunc = outlierFunc
                                        ,goutlierFunc = goutlierFunc
                                        ,outlierFunc_args = outlierFunc_args
                                        ,goutlierFunc_args = goutlierFunc_args
                        				,db = db
                        		    )
                		  curQa					
			            } ,db)
	names(qaTask.list)<-df$qaName
	db$qaTaskList<-qaTask.list
	print(paste(nrow(df),"qaTask created ahd saved in db!"))
	qaTask.list
}
