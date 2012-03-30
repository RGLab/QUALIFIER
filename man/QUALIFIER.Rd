\name{QUALIFIER}
\alias{QUALIFIER}

\title{
a wrapper function 
}
\description{
a convenient way that does saveToDB,getQAStats,makeQaTask in one call
}
\usage{
QUALIFIER(db,G,metaFile,checkListFile,fcs.colname)
}
\arguments{
  
  \item{db}{ An environment storing all the QA data.}
  
  \item{G}{A \code{\link[=GatingSet-class]{GatingSet}} containing multiple \code{gating hierarchies}}
  
  \item{metaFile}{A character scalar giving the file path of the sample annotation data, which is a csv spreadsheet contains 
					the meta information. Each row corresponds to one FCS file and 
					The QUALIFIER package looks for the FCS filename from "name" column of the spreadsheet.
  				}
  \item{checkListFile}{A character scalar giving the file path, which is a csv spreadsheet contains the 
  						detailed information of each QA task. It should have the columns:
  						qaID,qaName,description,qaLevel,pop,formula,plotType. See the slots of \code{\link{qaTask}}
  						for more details. }
  \item{fcs.colname}{A character scalar indicating column name that specify FCS file names in annotation data.}
  
}

\details{
      This is a convenient way to construct associate Gating set with sample meta data,extract the statistics from 
      the gating set and create multiple \code{qaTask}s by the QA task specification file.
}

\value{
  
   a list of \code{qaTask} objects 
  
}

\author{
Mike Jiang,Greg Finak


Maintainer: Mike Jiang <wjiang2@fhcrc.org>
}

\seealso{
\code{\link{qaTask}},\code{\link{saveToDB}},\code{\link{getQAStats}},\code{\link{makeQaTask}}
}

\examples{
\dontrun{
metaFile="FCS_File_mapping.csv"
checkListFile<-file.path(system.file("data",package="QUALIFIER"),"qaCheckList.csv.gz")
QUALIFIER(db,G,metaFile,checkListFile)
}
}
\keyword{functions}

