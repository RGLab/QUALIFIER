# TODO: Add comment
# 
# Author: mike
###############################################################################


setwd("inst/doc")
library(tools)
Sweave("flowQA.Rnw")
texi2dvi("flowQA.tex",pdf = TRUE)

