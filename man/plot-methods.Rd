\name{plot-methods}
\docType{methods}
\alias{plot}
\alias{plot,qaTask-method}
\alias{plot,qaTask,ANY-method}

\title{plot the statistics for a particular cell population of a group of samples }
\description{
plot the statistics for a particular cell population of a group of samples,this method is usually called 
after \code{qaCheck} to visualize the QA results.  
}
\usage{
	plot(x,y,...)
}
\arguments{

	\item{x}{a \code{qaTask} object}
	
	 \item{y}{a \code{formula} describing the variables to be used for plotting. 
	 								see \code{\link{qaCheck}} for more details.
				}
	
	
			
	
	\item{...}{ arguments to control the output.
	
				pop:a character scalar indicating the population name.If provided,it overwrites the pop slot in qaTask object.
				
				isTerminal:a logical scalar indicating whether the pop is at the terminal node of the gating path.
					
				fixed:a logical scalar indicating whether the pop name is matched as it is .By default it is FALSE,which matches the gating path as the regular expression
				
				subset:a logical expression as a filter. see \code{\link{qaCheck}} for more details.					

				width,height:size specification for the svg output.
				
				dest: a character specifying the output path. It is NULL by default, which indicates using the regular R device as the output.
						Otherwise it outputs to a svg file.
				
				plotAll: a logical/character scalar indicating whether to plot the 1D/2D density plot for all the individual FCS files together with the summary plot(either xyplot or bwplot).
						It is only valid when \code{dest} is specified as non-null path.
						It is FALSE by default,indicating that only the FCS files that are marked as outliers by \code{qaCheck} are plotted. 
						If TRUE, all FCS files are plotted ,which should be used cautously since it could be time consuming for a large dataset. 
						When it is "none",no scatter plot will be generated.
						
				scatterPlot: a logical scalar. When TRUE, the density(scatter) plot is plotted instead of the summary plot(xyplot/bwplot) 
				
				par:A list storing all the lattice arguments.If provided,it overwrites the par slot of qaTask object.
				
			}
}
\details{
The method does the same thing as \code{qaCheck} in terms of parsing the formula 
and selecting the gated population,statistics and subsetting the samples.
The difference is that it reads the outliers detection results saved in database and hightlight them in the summary plots.
Two kinds of lattice plots are currently supported:xyplot and bwplot(boxplot),depends on the \code{plotType} in \code{qaTask} object.
When the output path is provided by \code{dest}, the svg plot is generated. 
In svg plot, each dot or box (or only the one marked as outliers) is annotated by the tooltip 
or hyperlink.which further points to the individual density plot 
of the gated population.      

with \code{scatterPlot} and \code{subset} arguments, scatter plots can be generated for the selected FCS files or sample groups,which allows users to investigate the individual outlier groups or files.  
}

\author{
Mike Jiang,Greg Finak

Maintainer: Mike Jiang <wjiang2@fhcrc.org>
}

\seealso{
\code{\link{qaCheck}},\code{\link[QUALIFIER:qaReport]{qaReport}}
}
\examples{

\dontrun{

data("ITNQASTUDY")
checkListFile<-file.path(system.file("data",package="QUALIFIER"),"qaCheckList.csv.gz")
qaTask.list<-read.qaTask(db,checkListFile)

#using formula to summing up the percentage of boundary events of each channel
#using the cutoff function to detect the FCS files that has the higher percentage of boundary events
#than the upper threshold provided by uBound
#Note that the percentages of all channels for each fcs file ("name" here indicates the fcs file name) 
#are summed up through the formula  
qaCheck(qaTask.list[["BoundaryEvents"]]
		,sum(proportion) ~ RecdDt | name
		,outlierfunc=outlier.cutoff
		,uBound=0.0003
		)

plot(qaTask.list[["BoundaryEvents"]],proportion ~ RecdDt | channel)



#using Interquartile Range based outlier detection function
#to find the outliers that has significant variance of MNC cell population among aliquots
#here the formula is implicitly provided by qaTask object

qaCheck(qaTask.list[["MNC"]],outlierfunc=qoutlier,alpha=1.5)

plot(qaTask.list[["MNC"]])
}
}

\keyword{methods}

