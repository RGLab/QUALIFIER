# TODO: Add comment
# 
# Author: wjiang2
###############################################################################
library(knitr)
report_path <- "/home/wjiang2/rglab/workspace/QUALIFIER/examples/"
setwd(report_path)

soruceFile <- "ITN.R"
spin(file.path(report_path,soruceFile))
#htmlFile <- sub("R", "html", soruceFile)
#file.rename(htmlFile, "ITN.html")


