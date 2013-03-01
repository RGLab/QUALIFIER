library(QUALIFIER)

###############################################################################
#1.parse gating template
#--------------------------------------------------------------------------------
ws<-openWorkspace("/loc/no-backup/mike/ITN029ST/QA_template.xml")
GT<-parseWorkspace(ws
		,name=2
		,execute=F
		,subset=1
		,useInternal=T
)
gh_template<-GT[[1]]					
###############################################################################
#2.apply gating template to new data
#--------------------------------------------------------------------------------

datapath<-"/loc/no-backup/mike/ITN029ST/"
newSamples<-list.files(datapath,pattern=".fcs")[1:500]
G<-GatingSet(gh_template
		,newSamples
		,path=datapath
		,isNcdf=FALSE
)


################################################################################  
#3.extract stats
#--------------------------------------------------------------------------------
##meta info about FCS files such as pid,sampleID,visit number, staining panel(or Tube)
##not that pid is mandatory (to be displayed in tool tips of boxplot later (we may add
#extra argument to pass the colmns to tooltip instead of hardcoded the colnames)
metaFile="~/rglab/workspace/QUALIFIER/misc/ITN029ST/FCS_File_mapping.csv"
.db<-new.env()
initDB(.db)
pd<-pData(.db$gs[[1]])
participantid<-sort(unique(pd$participantid))
pid_map<-data.frame(pid=1:length(participantid),participantid=participantid)
pd<-merge(pd,pid_map,by.x="participantid",by.y="participantid")
pd$participantid<-NULL
pData(.db$gs[[1]])<-pd


qaPreprocess(gs=G
		,metaFile=metaFile
		,fcs.colname="FCS_Files"
		,date.colname=c("RecdDt","AnalysisDt")
)

################################################################################  
#4.dump db to labkey
#--------------------------------------------------------------------------------

##############################################################
# write gating set info to its table (called from a WebPart) #
# need to have the following parameter variables available,
# passed from the UI front-end:
# analysisName, gatingSetPath, analysisDescription, xmlPath, and sampleGroupName

            sql <- 'SELECT MAX(gsid) AS max_gsid FROM gstbl';

            max_gsid <- labkey.executeSql(
                  sql           = sql
                , showHidden    = TRUE
                , colNameOpt    = 'caption'
                , baseUrl       = labkey.url.base
                , folderPath    = labkey.url.path
                , schemaName    = 'opencyto_preprocessing'
            )[1,];

            if ( is.na(max_gsid) ){
                max_gsid <- 1;
            } else {
                max_gsid <- max_gsid + 1;
            }

            toInsert <- data.frame(
                  gsid            = max_gsid
                , gsname          = analysisName
                , objlink         = gatingSetPath
                , gsdescription   = analysisDescription
                , xmlpath         = xmlPath
                , samplegroup     = sampleGroupName
            );

            insertedRow <- labkey.insertRows(
                  queryName     = 'gstbl'
                , toInsert      = toInsert
                , baseUrl       = labkey.url.base
                , folderPath    = labkey.url.path
                , schemaName    = 'opencyto_preprocessing'
            );
##############################################################

writeStats(.db,baseUrl="http://dhcp157039.fhcrc.org:8080/labkey", folderPath="/FlowGraph PROJECT")

