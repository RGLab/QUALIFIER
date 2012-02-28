# TODO: Add comment
# 
# Author: mike
###############################################################################

setwd("output")
#setwd("inst/doc")
library(tools)
Sweave("../inst/doc/QUALIFIER.Rnw")
texi2dvi("QUALIFIER.tex",pdf = TRUE)

