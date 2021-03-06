% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier.R
\name{proportion.outliers.robust}
\alias{proportion.outliers.robust}
\alias{rlm}
\alias{outlierFunctions}
\alias{proportion.outliers.mle}
\alias{qoutlier}
\alias{outlier.norm}
\alias{outlier.t}
\alias{outlier.cutoff}
\title{outliers detection functions}
\usage{
proportion.outliers.robust(x, alpha = 0.01, isUpper = TRUE,
  isLower = TRUE)

proportion.outliers.mle(x, alpha = 0.01, isUpper = TRUE,
  isLower = TRUE)

qoutlier(x, alpha = 1.5, isUpper = TRUE, isLower = TRUE,
  plot = FALSE, ...)

outlier.norm(x, alpha = 0.01, z.cutoff = NULL, isUpper = TRUE,
  isLower = TRUE, plot = FALSE)

outlier.t(x, alpha = 0.01, z.cutoff = NULL, isUpper = TRUE,
  isLower = TRUE, plot = FALSE)

outlier.cutoff(x, lBound = NULL, uBound = NULL)
}
\arguments{
\item{x}{An integer/numeric vector used as the input}

\item{alpha, z.cutoff}{alpha is the percentage of the standard deviation from
the center of the data.  z.cutoff is the standardized z-score value. They are
used as the distribution based thresholds.}

\item{isUpper, isLower}{logical scalars indicating whether the outliers are
checked at upper or lower side of the distribution.}

\item{plot}{logical scalar indicating whether to visualize the outlier
detection results.}

\item{...}{other arguments to be passed to qoutlier function,currently it is
ignored.}

\item{lBound, uBound}{Numeric scalars used as cutoff threshold for either
lower limit or upper limit}
}
\value{
a logical vector with the same length of input vector,indicating
whether each entry of the input is a outlier.
}
\description{
Distribution based outlier detection functions.
}
\details{
These different outlier detection functions are used together with qaCheck method to perform outlier checks.
}
\seealso{
\code{\link{qaCheck}},\code{\link[QUALIFIER:qaReport]{qaReport}}
}
\author{
Mike Jiang,Greg Finak

Maintainer: Mike Jiang <wjiang2@fhcrc.org>
}
\keyword{functions}
