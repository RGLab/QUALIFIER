\name{qaCheck-methods}
\docType{methods}
\alias{qaCheck}
\alias{qaCheck-methods}
\alias{qaCheck,qaTask-method}
\alias{rlm}

\title{Perform the quality assessment for the qaTask object}
\description{
Perform the quality assessment for a particular QA Task based on the information provided by \code{\link{qaTask}} object. 
}
\usage{
qaCheck(obj,...)}
\arguments{

	\item{obj}{a \code{qaTask} object}
	
 	 \item{...}{
 	 			formula:
		 	 		a \code{formula} describing the variables to be used for QA. When it is omitted or NULL, the formula stored in \code{qaTask} is used. 
					It is generally of the form y ~ x | g1 * g2 * ... ,
					y is the statistics to be checked in this QA, It must be one of the four types:
					 
						"MFI": 
							Median Fluorescence Intensity of the cell population specified by \code{\link{qaTask}},
					
						"proportion": 
								the percentage of the cell population specified by \code{qaTask} in the parent population, 
					
						"count": 
								the number of events of the cell population specified by \code{qaTask},
					
						"spike": 
								the variance of intensity over time of each channel ,which indicating the stability of the fluorescence intensity.
		
					x is normally used to specify the variable plotted on x-axis in \code{\link[QUALIFIER:plot]{plot}} method.
					when \code{plotType} of the \code{qaTask} is "bwplot", it is also taken as the conditioning variable that divides the samples into subgroups within which
					the \code{outlierfunc} is applied.   
					
					g1,g2,.... are the conditioning variables, which are used to divide the samples into subgroups and 
					perform QA check whitin each individual groups.They may also be omitted,in which case the outliers detection is peformed in the entire sample set.
		
	
	 			subset:
					a logical expression used as a filter.It follows the same syntax as the "subset" expression in \code{\link[base:subset]{subset}}.
					
					\emph{Usage:}
					 
					subset=channel\%in\%c('FITC-A')
					
					subset=Tube=='CD8/CD25/CD4/CD3/CD62L'&channel\%in\%c('FITC-A')
							

				outlierfunc:a \code{function} to be used for outlier detection.
					see \code{\link{outlierFunctions}} for more details.
			
				gOutlierfunc:a \code{function} to be used for group outlier detection.
							see \code{\link{outlierFunctions}} for more details.
				rFunc:a \code{function} for fitting regression model within each individual subgroup.
					
				isTerminal:a logical scalar indicating whether the pop is at terminal node of the gating path.
					
				fixed:a logical scalar indicating whether the pop name is matched as it is .By default it is FALSE,which matches the gating path as the regular expression
			}
}
\details{
The method parses the formula stored in qaTask or explicitly provided by the argument and select
the appropriate gated population,extract the statistics that is pre-calculated by \code{\link{getQAStats}} 
and perform the outlier detection within a certain sample groups specified by the conditioning variables or x term in formula.
Then the outliers detection results are save in database and ready for query or plotting.    
}

\author{
Mike Jiang,Greg Finak

Maintainer: Mike Jiang <wjiang2@fhcrc.org>
}

\seealso{
\code{\link[QUALIFIER:plot]{plot}},\code{\link{getQAStats}}
}
\examples{

\dontrun{

data("ITN029")
checkListFile<-file.path(system.file("data",package="QUALIFIER"),"qaCheckList.csv.gz")
qaTask.list<-makeQaTask(db,checkListFile)


#using t-distribution based outlier detection function  
#applied the linear regression on each group to detect the significant MFI change over time 
qaCheck(qaTask.list[["MFIOverTime"]]
		,outlierfunc=outlier.t
		,rFunc=rlm
		,alpha=0.05
)
plot(qaTask.list[["MFIOverTime"]],y=MFI~RecdDt|stain
		,subset="channel\%in\%c('FITC-A')"
		,rFunc=rlm
)


#detect the outliers that has lower percentage of RBC Lysis than the threshold provided by lBound
qaCheck(qaTask.list[["RBCLysis"]]
		,formula=proportion ~ RecdDt | Tube
		,outlierfunc=outlier.cutoff
		,lBound=0.8
		)
		
plot(qaTask.list[["RBCLysis"]])	
}

}
\keyword{methods}

