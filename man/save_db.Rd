\name{save_db}
\alias{load_db}
\alias{save_db}
\title{save/load the data environment to/from disk}
\usage{
  save_db(db = .db, path, overwrite = FALSE, cdf = "link",
    ...)

  load_db(path)
}
\arguments{
  \item{db}{An \code{environment} storing all the QA data.
  By default it is an hidden global environment
  \code{.db}.}

  \item{path}{\code{character} data path that stores the
  db.}

  \item{cdf}{\code{character} the option to control cdf
  file operation. see \link{save_gs} for more details.}

  \item{overwrite}{\code{logical} whether to overwrite the
  existing folder}

  \item{...}{other arguments passed to \link{save_gs}}
}
\description{
  save and load the data environment that contains both
  statistics and GatingSets.
}
\examples{
\dontrun{
save_db(db, path = "./PreprocessedData")
db <- load_db(path = "./PreprocessedData")
}
}

