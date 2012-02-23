# TODO: Add comment
# 
# Author: mike
###############################################################################


setwd("inst/doc")
library(tools)
Sweave("QUALIFIER.Rnw")
texi2dvi("QUALIFIER.tex",pdf = TRUE)

