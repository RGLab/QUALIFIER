# TODO: Add comment
# 
# Author: mike
###############################################################################
proportion.outliers.robust<-function (x, alpha = 0.01,isUpper=TRUE,isLower=TRUE)
{
	outliers<-rep(FALSE,length(x))
	opt <- optim(par = c(1, 1), function(x, data = x) {
				a <- x[1]
				b <- x[2]
				#method of moments; numerical minimization of squared deviations
				#NOTE MAD^2 = robust variance
				abs((median(data) - a/(a + b))^2 + ((mad(data)^2) -
									((a * b)/((a + b)^2 * (a + b + 1))))^2)
			})
	if (opt$convergence != 0) {
		warning("Robust outlier detection for proportions failed to converge. No outliers returned")
		return(list(out=outliers,par=c(1,1)))
	}else{
		a<-opt$par[1]
		b<-opt$par[2]
		upper<-lower<-rep(FALSE,length(x))
		if(isUpper)	upper<-pbeta(x,a,b)>(1-alpha/2)
		if(isLower) lower<-pbeta(x,a,b)<(alpha/2)
		outliers<-upper|lower
	}
	return(outliers)
}

proportion.outliers.mle<-function (x, alpha = 0.01,isUpper=TRUE,isLower=TRUE)
{
	outliers<-rep(FALSE,length(x))
	opt <- optim(par = c(1, 1), function(x, data = x) {
				a <- x[1]
				b <- x[2]
				-sum(dbeta(data,a,b,log=T))
			})
	if (opt$convergence != 0) {
		warning("MLE Outlier detection for proportions failed to converge. Trying robust method.")
		return(proportion.outliers.robust(x,alpha=alpha))
	}
	else{
		a<-opt$par[1]
		b<-opt$par[2]
		upper<-lower<-rep(FALSE,length(x))
		if(isUpper)	upper<-pbeta(x,a,b)>(1-alpha/2)
		if(isLower) lower<-pbeta(x,a,b)<(alpha/2)
		outliers<-upper|lower
	}
	return(outliers)
}


##based on boxplot ... is the argument passed from parent call which is ignored here
qoutlier <-function (x, alpha = 1.5,isUpper=TRUE,isLower=TRUE,plot=FALSE,...) 
{
#	browser()
	if (alpha < 0) 
		stop("'alpha' must not be negative")
	nna <- !is.na(x)
	n <- sum(nna)
	stats <- stats::fivenum(x, na.rm = TRUE)
	iqr <- diff(stats[c(2, 4)])
	if (alpha == 0) 
		do.out <- FALSE
	else {
		if (!is.na(iqr)) {
			negInd <-x<(stats[2L] - alpha * iqr) 
			posInd <-x>(stats[4L] + alpha *iqr)
		}
		else return(!is.finite(x))
	}
	
	##decide if detect positive or negtive outlier
	posInd<-posInd&isUpper
	negInd<-negInd&isLower
	isOutlier<-posInd|negInd
	
	mu<-stats[3]
			
	if(plot)
	{	
		outlier.plot(x,mu,isOutlier)
	}
	isOutlier
	
}


##outlier detection based on normal distribution robusted estimation of mu and sigma 
outlier.norm <-function (x,alpha = 0.01,z.cutoff=NULL,isUpper=TRUE,isLower=TRUE,plot=FALSE) 
{
#	browser()
	#estimate mu and sd
	par<-try(huber(y=x),silent=TRUE)
	if(class(par)=="try-error")
	{
#		gettext(par)
#		isOutlier<-rep(FALSE,length(x))
		mu<-median(x)
		sigma<-mad(x)
	}else
	{
		mu<-par[[1]]
		sigma<-par[[2]]
	}
	if(is.null(z.cutoff)){
		#stardarize x
#	x1<-(x - mu)/sigma
		#calculate the cumulative probability of each x value
		cp<-pnorm(x,mu,sigma)
	}else{
		#standardize to z-score
		cp<-(x-mu)/sigma
	}
	isOutlier<-outlierDetection(cp,alpha,z.cutoff,isUpper,isLower)

	

	if(plot)
	{	
		outlier.plot(x,mu,isOutlier)
	}
	isOutlier
}


outlier.t <-function (x,alpha = 0.01,z.cutoff=NULL,isUpper=TRUE,isLower=TRUE,plot=FALSE) 
{
#	browser()
	par <- optim(c(mu = 0, sigma = 1) 
				,function(par, data = x) -sum(dt((data -par[1])/par[2], df = 4, log = T)+ log(1/par[2]))
				, method = "L-BFGS-B",lower = c(-Inf, 1e-10), upper = c(Inf, Inf))$par
	
	mu<-par[[1]]
	sigma<-par[[2]]
	#stardarize x
	x1<-(x - mu)/sigma
	#calculate the probability of each x value
	cp<-pt(x1, df = 4)
	
	isOutlier<-outlierDetection(cp,alpha,z.cutoff,isUpper,isLower)
	
	if(plot)
	{	
		outlier.plot(x,mu,isOutlier)
	}
	isOutlier	
	
}

outlier.cutoff<-function(x,lBound=NULL,uBound=NULL)
{
#	browser()
	ret<-rep(TRUE,length(x))
	if(!is.null(lBound))
	{
		ret<-ret&x<lBound
	}
	if(!is.null(uBound))
	{
		ret<-ret&x>uBound
	}
	ret
}


#outlier detection based on the given probability vector(p) and threshold(alpha)
outlierDetection<-function(cp,alpha = 0.01,z.cutoff=NULL,isUpper=TRUE,isLower=TRUE)
{
if(is.null(z.cutoff)){
#	browser()
	#Note: if two sided, test against alpha/2, otherwise against alpha.	
	alpha<- alpha/(isUpper+isLower)
	
	##decide if detect positive or negtive outlier
	posInd<-cp>(1-alpha)&isUpper
	negInd<-cp<alpha&isLower
	posInd|negInd
}else{
	(cp>abs(z.cutoff)&isUpper)|(cp < -abs(z.cutoff)&isLower)	
}
	
}

##plot the original dots and mean and higlight the outliers 
outlier.plot<-function (x,mu,isOutlier)
{
	
	colVec<-rep("black",length(x))
	colVec[isOutlier]<-"red"
	plot(x,col=colVec)
	abline(h=mu,col="blue")
	
}
