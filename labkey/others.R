

################################################################################  
#5.interactive plot 
#--------------------------------------------------------------------------------

plot(qaTask.list[["NumberOfEvents"]]
#		,count ~ RecdDt
		,subset=Tube=='CD8/CD25/CD4/CD3/CD62L'
#,dest="image"
#		,scales=list(x=list(rot=45
#							,cex=0.5
#							))
#		,pch=19
)


plot(qaTask.list[["MFIOverTime"]]
		,y=MFI~RecdDt|stain
		,subset=channel%in%c('APC-A')
#				&stain=="CD123"
#				&id==806
		,rFunc=rlm
		,scales=list(format="%m/%d/%y")
#		,scatterPlot=TRUE
		,scatterPar=list(xlog=F)
#		,dest="image"
#		,plotAll="none"

)



plot(qaTask.list[["RBCLysis"]]
#		,subset=Tube=='CD8/CD25/CD4/CD3/CD62L'
#				&id%in%c(270)
#		, RecdDt~proportion | Tube
		,scales=list(format="%m/%d/%y")
		,ylab="percent"
#		,scatterPlot=T
		,scatterPar=list(stat=T
				,xbin=128)
#		,horiz=T
#		,dest="image"
		,highlight="coresampleid"
#		,plotAll="none"
		,width=27,height=13
)	




plot(qaTask.list[["spike"]]
		,y=spike~RecdDt|channel
		,subset=Tube=='CD11c/CD80/DUMP/HLADr/CD123'
#	,dest="image"
#	,plotAll=T
)

plot(qaTask.list[["spike"]],y=spike~RecdDt|channel
		,subset=channel=='FITC-A'
#					&id%in%c(245,119)
#		,scatterPlot=TRUE
		,scatterPar=list(ylog=T
				,xlim=c(0,100)
#						,xbin=128
		)
#		,dest="image"
#		,plotAll="none"
)


plot(qaTask.list[["MNC"]]
		,proportion~factor(coresampleid)
#		, factor(coresampleid)~proportion
#		,par=list(horiz=TRUE)
)

#scatter plot for a sample group	
plot(qaTask.list[["MNC"]]
		,proportion~factor(coresampleid)
#		,par=list(xlab="coresampleid")
#		, coresampleid ~proportion
#		,par=list(horiz=TRUE)
		,subset=coresampleid%in%c(
#									11730
				8780
		)
#		,scatterPlot=TRUE
		,scatterPar=list(xbin=128
				,stat=T)
		,dest="image"
		,plotAll=TRUE
)

#scatter for one sample
plot(qaTask.list[["MNC"]]
		,scatterPlot=TRUE
		,subset=coresampleid==11730&id==245)


##example of passing lattice arguments		
plot(qaTask.list[["RedundantStain"]]
		,subset=channel=='APC-A'
#				&stain%in%c('CD123','CD3')
#				&coresampleid==11730
		,y=proportion~factor(coresampleid)|channel:stain
#		,scatterPlot=T
		,scatterPar=list(xlog=F
				,stat=T
		)
		,scales=list(x=list(relation="free"))
#		,layout=c(2,NA,1)
#		,dest="image"
)


################################################################################  
#6.clear or query check results 
#--------------------------------------------------------------------------------
clearCheck(qaTask.list[["MNC"]])

clearCheck(qaTask.list[["MFIOverTime"]])

clearCheck(qaTask.list[["RBCLysis"]])

subset(
		queryStats(qaTask.list[["RBCLysis"]],subset=Tube=='CD8/CD25/CD4/CD3/CD62L')
		,outlier==TRUE
)


################################################################################  
#7.add new aggregated stats 
#--------------------------------------------------------------------------------

QUALIFIER:::addStats(qaTask.list[["BoundaryEvents"]]
		,definition=sum(proportion)~RecdDt|id+gsid
		,pop="/root/MNC/margin"
		,statName="sum.prop")

head(subset(
				queryStats(qaTask.list[["BoundaryEvents"]]
						,y=sum.prop ~ RecdDt 
						,pop="margin"
						,subset=value>0
				)
				,outlier==TRUE)
)
#check on the new stats
qaCheck(qaTask.list[["BoundaryEvents"]]
		,sum.prop ~ RecdDt 
		,outlierfunc=outlier.cutoff
		,uBound=0.0003
)

plot(qaTask.list[["BoundaryEvents"]]
		,sum.prop ~ RecdDt 
#		,subset=channel=="PE-A"
#				&id==806
		,ylab="sum(percent)"
#		,scatterPlot=T
#		,scatterPar=list(
#						xlog=T
#						,stat=T
#						)
		,scales=list(format="%m/%d/%y")
#		,plotAll="none"
#		,dest="image"
)

