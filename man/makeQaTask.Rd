\name{makeQaTask}
\alias{makeQaTask}
\alias{qaCheckList}
\alias{tubesevents}
\alias{db}
\title{
Create a list of \code{qaTask} objects 
}
\description{
Create a list of qaTask objects by reading external csv spreadsheet containing descriptions of each QA task.

}
\usage{
makeQaTask(db,checkListFile) 
}
\arguments{
  
  \item{db}{ An environment storing all the QA data.}
  
  \item{checkListFile}{A character scalar giving the file path, which is a csv spreadsheet contains the 
  						detailed information of each QA task. It should have the columns:
  						qaID,qaName,description,qaLevel,pop,formula,plotType. See the slots of \code{\link{qaTask}}
  						for more details. }
  
}

\details{
      This is a convenient way to construct multiple \code{qaTask}s. Users can also create the individual \code{qaTask} by
      using \code{new} method.
}

\value{
  
   a list of \code{qaTask} objects 
  
}

\author{
Mike Jiang,Greg Finak


Maintainer: Mike Jiang <wjiang2@fhcrc.org>
}

\seealso{
\code{\link{qaTask}},\code{\link{qaReport}}
}

\examples{
\dontrun{
checkListFile<-file.path(system.file("data",package="QUALIFIER"),"qaCheckList.csv.gz")
qaTask.list<-makeQaTask(db,checkListFile)
}
}
\keyword{functions}

