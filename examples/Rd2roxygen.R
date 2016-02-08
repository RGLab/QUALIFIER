#library(Rd2roxygen)
library(roxygen2)
#cat(create_roxygen(parse_file("/home/wjiang2/rglab/workspace/QUALIFIER/man/QUALIFIER-package.Rd")
#                  )
#    , sep  = "\n"
#    )
pkg_path <- "/home/wjiang2/rglab/workspace/QUALIFIER/"
roxygenize(pkg_path)
checkDocFiles(dir = pkg_path)

setwd("/home/wjiang2/rglab/workspace/QUALIFIER/inst/doc/")
library(tools)
Sweave("QUALIFIER.Rnw")
texi2dvi("QUALIFIER.tex",pdf = TRUE,clean=TRUE)