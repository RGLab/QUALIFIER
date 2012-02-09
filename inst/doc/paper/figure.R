postscript("~/rglab/workspace/flowQA/inst/doc/paper/image/gatingHierarchy.ps")
plot(G[[1]])
dev.off()
epstopdf atingHierarchy.ps
pdf("RBCLysis.pdf",width=800,height=600)
plot(qaTask.list[["RBCLysis"]])
dev.off()