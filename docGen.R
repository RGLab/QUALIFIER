# TODO: Add comment
# 
# Author: mike
###############################################################################

##debug mode:load routines into globla env
removeMethod("plot",sig="qaTask")
removeMethod("qaCheck",sig="qaTask")

lapply(list.files("~/rglab/workspace/QUALIFIER/R",full=T),source)

setwd("output")
#setwd("inst/doc")
library(tools)
Sweave("../inst/doc/QUALIFIER.Rnw")
texi2dvi("QUALIFIER.tex",pdf = TRUE)

