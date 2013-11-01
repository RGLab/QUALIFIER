library(QUALIFIER)
#+ setup, include=FALSE
library(knitr)
opts_chunk$set(warning = FALSE, message = FALSE, include = TRUE)


#' # Data preprocessing
#' ## Parse the template workspace
#+ eval = FALSE
datapath <- "~/rglab/workspace/QUALIFIER/misc/ITN029ST/"
ws<-openWorkspace(file.path(datapath, "QA_template.xml"))
GT<-parseWorkspace(ws ,name=2 ,execute=F ,subset=1)
#' ## extract the gating hiearchy
#+ eval = FALSE
gh_template <- GT[[1]]					

#' ## Apply gating template to new data
#+ eval = FALSE

newSamples <- list.files(datapath, pattern = ".fcs")[1:30]
G <- GatingSet(gh_template ,newSamples ,path = datapath ,isNcdf=T
            #, ncdfFile = "mypath/myfile.nc" 
            )
  
#' ## Extract stats
#+ eval = FALSE
db<-new.env()
initDB(db)
metaFile="~/rglab/workspace/QUALIFIER/misc/ITN029ST/FCS_File_mapping.csv"
qaPreprocess(db=db,gs=G ,metaFile=metaFile 
              , fcs.colname="FCS_Files" 
              , date.colname=c("RecdDt","AnalysisDt")
              , date.format = "%Y-%m-%d"
            ,isMFI=T,isSpike=T, isChannel = T
        )


 
#' ## Load QA check list
#+ eval = FALSE
checkListFile <- file.path(system.file("data",package="QUALIFIER"),"qaCheckList.csv.gz")
qaTask.list <- read.qaTask(db,checkListFile=checkListFile)

#' ## Archive the preprocessed data
#+ eval = FALSE
save_db(db, path = "~/rglab/workspace/QUALIFIER/output/preprocessedData", overwrite = T
        , cdf = "link" #change it to "copy" on cross-device error
        )

#' # Start QA        
#' ## Load the archived preprocessed data        
#+ eval = T
db <- load_db(path = "~/rglab/workspace/QUALIFIER/output/preprocessedData")
qaTask.list <- db$qaTaskList

#' ## Convert date format if necessary
pData(db$gs[[1]])$RecdDt <- as.Date(pData(db$gs[[1]])$RecdDt)

#' ## qaTask1 : Total Number Of Events of each tube
#' ### parse TubeID from FCS filenames
QUALIFIER:::.parseTubeID(db)

#' ### read pre-determined events number for tubes from csv file
#' pannel name should be in place of tube name since the entire package is using pannel name 
#' to represent the tube
tubesEvents <- read.csv(file.path(system.file("data",package="QUALIFIER"),"tubesevents.csv.gz"),row.names=1)
tubesEvents
#' ### Each tube may have different thresholds for different time periods
tubesEventsOrig <- QUALIFIER:::.TubeNameMapping(db,tubesEvents=tubesEvents[,3,drop=F])
tubesEventsOrig
tubesEvents20090825 <- QUALIFIER:::.TubeNameMapping(db,tubesEvents=tubesEvents[,2,drop=F])
tubesEvents20090825
tubesEvents20090622 <- QUALIFIER:::.TubeNameMapping(db,tubesEvents=tubesEvents[,1,drop=F])
tubesEvents20090622

#' qaCheck on diffrent time periods seperately
#' use 80% of the pre-defined the value for each pannel as thresholds
qaCheck(qaTask.list[["NumberOfEvents"]]
    ,formula=count ~ RecdDt | Tube
    ,outlierfunc=list(func=outlier.cutoff,args=list(lBound=0.8*tubesEvents20090825))
    ,subset=as.Date(RecdDt,"%m/%d/%y")>='2009-08-25'
)

qaCheck(qaTask.list[["NumberOfEvents"]]
    ,formula=count ~ RecdDt | Tube
    ,outlierfunc=list(func=outlier.cutoff,args=list(lBound=0.8*tubesEvents20090622))
    ,subset=as.Date(RecdDt,"%m/%d/%y")<'2009-08-25'&as.Date(RecdDt,"%m/%d/%y")>='2009-06-22'
)

qaCheck(qaTask.list[["NumberOfEvents"]]
    ,formula=count ~ RecdDt | Tube
    ,outlierfunc=list(func=outlier.cutoff,args=list(lBound=0.8*tubesEventsOrig))
    ,subset=as.Date(RecdDt,"%m/%d/%y")<'2009-06-22'
)


#' ### quickly visualize the qa results for each tube
plot(qaTask.list[["NumberOfEvents"]]
      , subset=Tube=='CD8/CD25/CD4/CD3/CD62L'
    )
#' ### to inspect each individual outlier
#' #### enable svg output by setting `dest` folder 
#+ eval = FALSE
plot(qaTask.list[["NumberOfEvents"]]
    , subset=Tube=='CD8/CD25/CD4/CD3/CD62L'
      , dest = "./output/image"   
      , plotAll = FALSE 
)
#' #### or plot the scatter plot directly by setting `scatterPlot` to TRUE
plot(qaTask.list[["NumberOfEvents"]]
		,subset = fileid == 1
		,scatterPlot = TRUE
		,scatterPar = list(xbin = 32)
)

#' #### How to query stats table to get the `id` info of the outlier sample
#+ eval = FALSE
stats_of_thisTask <- queryStats(qaTask.list[["NumberOfEvents"]])
subset(stats_of_thisTask ,outlier==TRUE && Tube=='CD8/CD25/CD4/CD3/CD62L')

#' #### To clear qa check results
#+ eval = FALSE
clearCheck(qaTask.list[["NumberOfEvents"]])

#' ## qaTask 2 : Total Boundary Events (aggregated by sample)
#' ###Addd new aggregated stats
QUALIFIER:::.addStats(qaTask.list[["BoundaryEvents"]]
                      ,definition=sum(proportion)~RecdDt|fileid+gsid
                      ,pop="/root/MNC/margin"
                      ,statName="sum.prop"
                  )
#' ###query the aggregated stats
subset(
        queryStats(qaTask.list[["BoundaryEvents"]]
                    ,y=sum.prop ~ RecdDt 
                    ,pop="margin"
					,subset=value>0&fileid==68
                  )
        ,outlier==TRUE)

#' ### QA check on the new stats
qaCheck(qaTask.list[["BoundaryEvents"]]
        ,sum.prop ~ RecdDt 
        ,outlierfunc= list(func = outlier.cutoff
                          ,args=list(uBound=0.0003)
                          )
      )
#' ###  visualize the qa results
plot(qaTask.list[["BoundaryEvents"]]
    ,sum.prop ~ RecdDt 
    )




#' ## qaTask 3 : MFIOverTime
qaCheck(qaTask.list[["MFIOverTime"]]
        ,rFunc=rlm
        )

plot(qaTask.list[["MFIOverTime"]]
    ,subset=channel%in%c('APC-A')
        &stain=="CD86 APC-A"
		,rFunc=rlm
)
#' ### Investigate the individual outlier
queryStats(qaTask.list[["MFIOverTime"]]
          ,subset=channel%in%c('APC-A')
                  &stain=="CD86 APC-A"
                  &value > 800
          )
plot(qaTask.list[["MFIOverTime"]]
    ,subset=channel%in%c('APC-A')
        &stain=="CD86 APC-A"
        & fileid%in%c(25)
    ,scatterPlot = T
    ,scatterPar=list(xbin=64
                    ,xlog = TRUE)
)
#' ## qaTask 4 : RBCLysis
qaCheck(qaTask.list[["RBCLysis"]]
        ,outlierfunc=list(func=outlier.cutoff
                    ,args=list(lBound=0.9)
                    )
)

subset(
    queryStats(qaTask.list[["RBCLysis"]]
        ,subset=Tube=='CD8/CD25/CD4/CD3/CD62L')
    ,outlier==TRUE)

plot(qaTask.list[["RBCLysis"]])

plot(qaTask.list[["RBCLysis"]]
		,subset=Tube=='CD8/CD25/CD4/CD3/CD62L'
				& fileid%in%c(45)
		,scatterPlot=T
		,scatterPar=list(stat=T
						,xbin=64)
)	


#' ## qaTask 5 : spike
qaCheck(qaTask.list[["spike"]]
          ,outlierfunc = list(func = outlier.t
                              ,args = list(alpha=0.001
                              ,isLower=FALSE)
                              )
      )
plot(qaTask.list[["spike"]]
    ,y=spike~RecdDt|channel
    ,subset=Tube=='CD11c/CD80/DUMP/HLADr/CD123'
    )
subset(
    queryStats(qaTask.list[["spike"]]
        ,subset=Tube=='CD11c/CD80/DUMP/HLADr/CD123')
    ,outlier==TRUE)

plot(qaTask.list[["spike"]]
    ,subset=channel=='SSC-A'&fileid%in%c(21)
		,scatterPlot=TRUE
        ,scatterPar=list(xbin=32)
  )

#' ## qaTask 5 : MNC
qaCheck(qaTask.list[["MNC"]]
        , gOutlierfunc=list(func=outlier.norm
                            ,args=list(alpha = 0.4
#                                       , isLower = FALSE
                                       )
                            )
)

plot(qaTask.list[["MNC"]]
    ,proportion~factor(`coresampleid`)
)

#' ### try to rotate the boxplot
plot(qaTask.list[["MNC"]]
    , factor(`coresampleid`) ~ proportion
    , horiz = TRUE
)

plot(qaTask.list[["MNC"]]
	, subset=coresampleid%in%c(10908)
	, scatterPlot=TRUE
    , scatterPar=list(xbin = 32)
      )

#' ## qaTask 5 : RedundantStain
qaCheck(qaTask.list[["RedundantStain"]])

            
plot(qaTask.list[["RedundantStain"]]
    , proportion ~ factor(coresampleid)
    ,subset=channel=='APC-A'
				&stain%in%c('CD123 APC-A')
    )
		
plot(qaTask.list[["RedundantStain"]]
    ,subset = channel == 'APC-A'
			  & coresampleid == 11730
    ,scatterPlot=T
    ,scatterPar=list(xlog = T
                    , xbin = 32
                    )
)
  
#' ## qa report in html format
#' * set plotAll = "none" to avoid plotting scatterplots for FCS ( for quick test) 
#' * set plotAll = FALSE to only generate plots for the outliers
#' * set plotAll = TRUE to generate the scatter plots for all FCS files (may take longer) 

#' ### customerize some of the task before pass them to report method
htmlReport(qaTask.list[["MFIOverTime"]]) <- TRUE #set it as TRUE to display it even when no outlier is called
rFunc(qaTask.list[["MFIOverTime"]]) <- rlm

highlight(qaTask.list[["BoundaryEvents"]]) <- "coresampleid"
scatterPar(qaTask.list[["MFIOverTime"]]) <- list(xlog=TRUE)
scatterPar(qaTask.list[["BoundaryEvents"]]) <- list(xlog=TRUE)
scatterPar(qaTask.list[["RedundantStain"]]) <- list(xlog=TRUE)
qpar(qaTask.list[["RedundantStain"]]) <- list(scales=list(x=list(relation="free")))

#+ eval = FALSE
qaReport(qaTask.list, outDir = "~/rglab/workspace/QUALIFIER/output/"
                    , plotAll = FALSE
                    , subset = coresampleid == 11730
        )


