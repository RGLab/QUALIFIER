# TODO: Add comment
# 
# Author: mike
###############################################################################

prepanel.default.xyplot <-
		function(x, y, type, subscripts, groups = NULL, ...)
{
	## Note: shingles satisfy is.numeric()
	if (any(!is.na(x)) && any(!is.na(y)))
	{
		ord <- order(as.numeric(x))
		if (!is.null(groups))
		{
			gg <- groups[subscripts]
			dx <- unlist(lapply(split(as.numeric(x)[ord], gg[ord]), diff))
			dy <- unlist(lapply(split(as.numeric(y)[ord], gg[ord]), diff))
			## ok <- !is.na(gg)
			
			## One may argue that points with is.na(gg) should be
			## excluded from the data rectangle since they are not
			## plotted.  For now I'm going to take the other view,
			## namely that the points are there, they just happen to
			## be invisible because the value of the variable defining
			## their graphical parameters is unknown.
		}
		else
		{
			dx <- diff(as.numeric(x[ord]))
			dy <- diff(as.numeric(y[ord]))
			## ok <- TRUE
		}
		list(xlim = lattice:::scale.limits(x), ylim = lattice:::scale.limits(y), dx = dx, dy = dy,
				xat = if (is.factor(x)) sort(unique(as.numeric(x))) else NULL,
				yat = if (is.factor(y)) sort(unique(as.numeric(y))) else NULL)
		
		
	}
	else prepanel.null()
		
	
}


##add svg anno to the original panel function for xyplot of lattice package
##individual oultiers are colored based on the groups argument which passed through oultier column of dataframe
panel.xyplotEx <-
		function(x, y, type = "p",
				groups = NULL,
				pch = if (is.null(groups)) plot.symbol$pch else superpose.symbol$pch,
				col,
				col.line = if (is.null(groups)) plot.line$col else superpose.line$col,
				col.symbol = if (is.null(groups)) plot.symbol$col else superpose.symbol$col,
				font = if (is.null(groups)) plot.symbol$font else superpose.symbol$font,
				fontfamily = if (is.null(groups)) plot.symbol$fontfamily else superpose.symbol$fontfamily,
				fontface = if (is.null(groups)) plot.symbol$fontface else superpose.symbol$fontface,
				lty = if (is.null(groups)) plot.line$lty else superpose.line$lty,
				cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex,
				fill = if (is.null(groups)) plot.symbol$fill else superpose.symbol$fill,
				lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd,
				horizontal = FALSE,
				...,
				grid = FALSE, abline = NULL,
				jitter.x = FALSE, jitter.y = FALSE,
				factor = 0.5, amount = NULL,
				identifier = "xyplot")
{
	if (all(is.na(x) | is.na(y))) return()
	plot.symbol <- trellis.par.get("plot.symbol")
	plot.line <- trellis.par.get("plot.line")
	superpose.symbol <- trellis.par.get("superpose.symbol")
	superpose.line <- trellis.par.get("superpose.line")
	if (!missing(col))
	{
		if (missing(col.line)) col.line <- col
		if (missing(col.symbol)) col.symbol <- col
	}
	if (missing(grid) && ("g" %in% type)) grid <- TRUE ## FIXME: what if list?
	if (!identical(grid, FALSE))
	{
		if (!is.list(grid))
			grid <- switch(as.character(grid),
					"TRUE" = list(h = -1, v = -1, x = x, y = y),
					"h" = list(h = -1, v = 0, y = y),
					"v" = list(h = 0, v = -1, x = x),
					list(h = 0, v = 0))
		do.call(panel.grid, grid)
	}
#	browser()
	if (!is.null(abline))
	{
		if (is.numeric(abline)) abline <- as.list(abline)
		do.call(panel.abline, abline)
	}
#	browser()
	
	if (!is.null(groups))
		panel.superpose(x, y,
				type = type,
				groups = groups,
				pch = pch,
				col.line = col.line,
				col.symbol = col.symbol,
				font = font,
				fontfamily = fontfamily,
				fontface = fontface,
				lty = lty,
				cex = cex,
				fill = fill,
				lwd = lwd,
				horizontal = horizontal,
				panel.groups = panel.xyplotEx,
				jitter.x = jitter.x,
				jitter.y = jitter.y,
				factor = factor,
				amount = amount,
				grid = FALSE, ## grid=TRUE/type="g" already handled
				...)
	else
	{
		x <- as.numeric(x)
		y <- as.numeric(y)
		id <- identifier
#		browser()
		dest<-list(...)$dest
#		dest<-NULL
		if(!is.null(dest))
		{
			###add svg anno
			rowIds<-list(...)$subscripts
			data<-list(...)$data
			plotObjs<-list(...)$plotObjs
			plotAll<-list(...)$plotAll
			db<-list(...)$db
#			browser()
			if(is.null(plotAll))
				plotAll=FALSE
#			browser()
			if ("o" %in% type || "b" %in% type) type <- c(type, "p", "l")
			if ("p" %in% type)
				for(i in 1:length(x))
				{
					curRowID<-rowIds[i]
					curOutRow<-data[curRowID,]
					FileTips<-paste("uniqueID=",curOutRow$id," file=",curOutRow$name,sep="")
					setSVGShapeToolTip(title=FileTips,sub.special=FALSE)
					#				browser()
					paths<-flowQA:::.FileNameGen(prefix="f",ID=curOutRow$id,population=as.character(curOutRow$population)
							,channel=as.character(curOutRow$channel)
							,stats=as.character(curOutRow$stats))
					
					if(!file.exists(file.path(dest,"individual")))system(paste("mkdir",file.path(dest,"individual")))
					paths<-tempfile(pattern=paths,tmpdir="individual",fileext=".png")
					if(curOutRow$outlier||plotAll==TRUE)
					{
						##save the individual plot obj
#						browser()
						assign(basename(paths),qa.singlePlot(db,curOutRow),envir=plotObjs)
				
#						png(file.path(dest,paths))
#						qa.singlePlot(db,curOutRow)
#						dev.off()
#						dev.set(2)

						setSVGShapeURL(paths)	
					}
					
					panel.points(x = if (jitter.x) jitter(x[i], factor = factor, amount = amount) else x[i],
							y = if (jitter.y) jitter(y[i], factor = factor, amount = amount) else y[i],
							cex = cex,
							fill = fill,
							font = font,
							fontfamily = fontfamily,
							fontface = fontface,
							col = col.symbol,
							pch = pch, ...,
							identifier = id)
				}
		}else
		{
			panel.points(x = if (jitter.x) jitter(x, factor = factor, amount = amount) else x,
				y = if (jitter.y) jitter(y, factor = factor, amount = amount) else y,
				cex = cex,
				fill = fill,
				font = font,
				fontfamily = fontfamily,
				fontface = fontface,
				col = col.symbol,
				pch = pch, ...,
				identifier = id)
		}

#		browser()
		if ("l" %in% type)
			panel.lines(x = x, y = y, lty = lty, col = col.line, lwd = lwd,
					..., identifier = id)
		if ("h" %in% type)
		{
			if (horizontal)
				panel.lines(x = x, y = y, type = "H",
						lty = lty, col = col.line, lwd = lwd,
						..., identifier = id)
			else
				panel.lines(x = x, y = y, type = "h",
						lty = lty, col = col.line, lwd = lwd,
						..., identifier = id)
		}
		

		
		## FIXME: should this be delegated to llines with type='s'?
		if ("s" %in% type)
		{
			ord <- if (horizontal) sort.list(y) else sort.list(x)
			n <- length(x)
			xx <- numeric(2*n-1)
			yy <- numeric(2*n-1)
			
			xx[2*1:n-1] <- x[ord]
			yy[2*1:n-1] <- y[ord]
			xx[2*1:(n-1)] <- x[ord][-1]
			yy[2*1:(n-1)] <- y[ord][-n]
			panel.lines(x = xx, y = yy,
					lty = lty, col = col.line, lwd = lwd, ...,
					identifier = id)
		}
		if ("S" %in% type)
		{
			ord <- if (horizontal) sort.list(y) else sort.list(x)
			n <- length(x)
			xx <- numeric(2*n-1)
			yy <- numeric(2*n-1)
			
			xx[2*1:n-1] <- x[ord]
			yy[2*1:n-1] <- y[ord]
			xx[2*1:(n-1)] <- x[ord][-n]
			yy[2*1:(n-1)] <- y[ord][-1]
			panel.lines(x = xx, y = yy,
					lty = lty, col = col.line, lwd = lwd,
					..., identifier = id)
		}
		if ("r" %in% type) panel.lmline(x, y, col = col.line, lty = lty, lwd = lwd, ...)
		if ("smooth" %in% type)
			panel.loess(x, y, horizontal = horizontal,
					col = col.line, lty = lty, lwd = lwd, ...)
		if ("a" %in% type)
			panel.linejoin(x, y, 
					horizontal = horizontal,
					lwd = lwd,
					lty = lty,
					col.line = col.line,
					...)
	}
}

##add svg anno to the original panel function for boxplot of lattice package
panel.bwplotEx <-
		function(x, y, box.ratio = 1, box.width = box.ratio / (1 + box.ratio),
				horizontal = TRUE,
				pch = box.dot$pch,
				col = box.dot$col,
				alpha = box.dot$alpha,
				cex = box.dot$cex,
				font = box.dot$font,
				fontfamily = box.dot$fontfamily,
				fontface = box.dot$fontface,
				fill = box.rectangle$fill,
				varwidth = FALSE,
				notch = FALSE,
				notch.frac = 0.5,
				...,
				levels.fos = if (horizontal) sort(unique(y)) else sort(unique(x)),
				stats = boxplot.statsEx,
				coef = 1.5, do.out = TRUE,
				identifier = "bwplot")
{
#	browser()
	if (all(is.na(x) | is.na(y))) return()
	x <- as.numeric(x)
	y <- as.numeric(y)
	
	box.dot <- trellis.par.get("box.dot")
	box.rectangle <- trellis.par.get("box.rectangle")
	box.umbrella <- trellis.par.get("box.umbrella")
	plot.symbol <- trellis.par.get("plot.symbol")
	
	fontsize.points <- trellis.par.get("fontsize")$points
	cur.limits <- current.panel.limits()
	xscale <- cur.limits$xlim
	yscale <- cur.limits$ylim
	
	if (!notch) notch.frac <- 0
#	browser()
	
	if (horizontal)
	{
		blist <-
				tapply(x, factor(y, levels = levels.fos),
						stats,
						coef = coef,
						do.out = do.out)
		blist.stats <- t(sapply(blist, "[[", "stats"))
		blist.out <- lapply(blist, "[[", "out")
		blist.height <- box.width # box.ratio / (1 + box.ratio)
		if (varwidth)
		{
			maxn <- max(table(y))
			blist.n <- sapply(blist, "[[", "n")
			blist.height <- sqrt(blist.n / maxn) * blist.height
		}
		
		## start of major changes to support notches
		blist.conf <-
				if (notch)
					t(sapply(blist, "[[", "conf"))
				else
					blist.stats[ , c(2,4), drop = FALSE]
		
		xbnd <- cbind(blist.stats[, 3], blist.conf[, 2],
				blist.stats[, 4], blist.stats[, 4],
				blist.conf[, 2], blist.stats[, 3],
				blist.conf[, 1], blist.stats[, 2],
				blist.stats[, 2], blist.conf[, 1],
				blist.stats[, 3])
		ytop <- levels.fos + blist.height / 2
		ybot <- levels.fos - blist.height / 2
		ybnd <- cbind(ytop - notch.frac * blist.height / 2,
				ytop, ytop, ybot, ybot,
				ybot + notch.frac * blist.height / 2,
				ybot, ybot, ytop, ytop,
				ytop - notch.frac * blist.height / 2)
		## xs <- matrix(NA_real_, nrow = nrow(xbnd) * 2, ncol = ncol(xbnd))
		## ys <- matrix(NA_real_, nrow = nrow(xbnd) * 2, ncol = ncol(xbnd))
		## xs[seq(along.with = levels.fos, by = 2), ] <- xbnd[seq(along.with = levels.fos), ]
		## ys[seq(along.with = levels.fos, by = 2), ] <- ybnd[seq(along.with = levels.fos), ]
		
		## box
		
		## append NA-s to demarcate between boxes
		xs <- cbind(xbnd, NA_real_)
		ys <- cbind(ybnd, NA_real_)
		
		
		panel.polygon(t(xs), t(ys),
				lwd = box.rectangle$lwd,
				lty = box.rectangle$lty,
				col = fill,
				alpha = box.rectangle$alpha,
				border = box.rectangle$col,
				identifier = paste(identifier, "box", sep="."))
		## end of major changes to support notches
		
		
		## whiskers
		
		panel.segments(c(blist.stats[, 2], blist.stats[, 4]),
				rep(levels.fos, 2),
				c(blist.stats[, 1], blist.stats[, 5]),
				rep(levels.fos, 2),
				col = box.umbrella$col,
				alpha = box.umbrella$alpha,
				lwd = box.umbrella$lwd,
				lty = box.umbrella$lty,
				identifier = paste(identifier, "whisker", sep="."))
		panel.segments(c(blist.stats[, 1], blist.stats[, 5]),
				levels.fos - blist.height / 2,
				c(blist.stats[, 1], blist.stats[, 5]),
				levels.fos + blist.height / 2,
				col = box.umbrella$col,
				alpha = box.umbrella$alpha,
				lwd = box.umbrella$lwd,
				lty = box.umbrella$lty,
				identifier = paste(identifier, "cap", sep="."))
		
		## dot
		
		if (all(pch == "|"))
		{
			mult <- if (notch) 1 - notch.frac else 1
			panel.segments(blist.stats[, 3],
					levels.fos - mult * blist.height / 2,
					blist.stats[, 3],
					levels.fos + mult * blist.height / 2,
					lwd = box.rectangle$lwd,
					lty = box.rectangle$lty,
					col = box.rectangle$col,
					alpha = alpha,
					identifier = paste(identifier, "dot", sep="."))
		}
		else
		{
			panel.points(x = blist.stats[, 3],
					y = levels.fos,
					pch = pch,
					col = col, alpha = alpha, cex = cex,
					fontfamily = fontfamily,
					fontface = lattice:::chooseFace(fontface, font),
					fontsize = fontsize.points,
					identifier = paste(identifier, "dot", sep="."))
		}
		
		## outliers
		
		panel.points(x = unlist(blist.out),
				y = rep(levels.fos, sapply(blist.out, length)),
				pch = plot.symbol$pch,
				col = plot.symbol$col,
				alpha = plot.symbol$alpha,
				cex = plot.symbol$cex,
				fontfamily = plot.symbol$fontfamily,
				fontface = lattice:::chooseFace(plot.symbol$fontface, plot.symbol$font),
				fontsize = fontsize.points,
				identifier = paste(identifier, "outlier", sep="."))
		
	}
	else
	{
#		browser()
		blist <-
				tapply(y, factor(x, levels = levels.fos),
						stats,
						coef = coef,
						do.out = do.out)
		blist.stats <- t(sapply(blist, "[[", "stats"))
		blist.x <- sapply(blist, "[[", "x")
#		blist.out <- lapply(blist, "[[", "out")
#		blist.outInd <- lapply(blist, "[[", "outInd")
		blist.height <- box.width # box.ratio / (1 + box.ratio)
		if (varwidth)
		{
			maxn <- max(table(x))
			blist.n <- sapply(blist, "[[", "n")
			blist.height <- sqrt(blist.n / maxn) * blist.height
		}
		
		blist.conf <-
				if (notch)
					sapply(blist, "[[", "conf")
				else
					t(blist.stats[ , c(2,4), drop = FALSE])
		
		ybnd <- cbind(blist.stats[, 3], blist.conf[2, ],
				blist.stats[, 4], blist.stats[, 4],
				blist.conf[2, ], blist.stats[, 3],
				blist.conf[1, ], blist.stats[, 2],
				blist.stats[, 2], blist.conf[1, ],
				blist.stats[, 3])
		xleft <- levels.fos - blist.height / 2
		xright <- levels.fos + blist.height / 2
		xbnd <- cbind(xleft + notch.frac * blist.height / 2,
				xleft, xleft, xright, xright,
				xright - notch.frac * blist.height / 2,
				xright, xright, xleft, xleft,
				xleft + notch.frac * blist.height / 2)
		## xs <- matrix(NA_real_, nrow = nrow(xbnd) * 2, ncol = ncol(xbnd))
		## ys <- matrix(NA_real_, nrow = nrow(xbnd) * 2, ncol = ncol(xbnd))
		## xs[seq(along.with = levels.fos, by = 2), ] <- xbnd[seq(along.with = levels.fos), ]
		## ys[seq(along.with = levels.fos, by = 2), ] <- ybnd[seq(along.with = levels.fos), ]
		
		## box
		
		## append NA-s to demarcate between boxes
		xs <- cbind(xbnd, NA_real_)
		ys <- cbind(ybnd, NA_real_)
		
#		browser()
		dest<-list(...)$dest
#		dest<-NULL
		
		gOutResult<-list(...)$gOutResult
		rowIds<-list(...)$subscripts
		data=list(...)$data[rowIds,]
		groupBy<-list(...)$groupBy
		plotObjs<-list(...)$plotObjs
		plotAll<-list(...)$plotAll
		db<-list(...)$db
		if(is.null(plotAll))
			plotAll=FALSE
		
		dataGroups<-split(data,f=eval(parse(text=paste("data$",groupBy,sep=""))))
		
		nGroups<-length(dataGroups)
#			browser()
#			toPlot<-vector("list",nGroups)
		for(i in 1:nGroups)
		{
			curGroup<-dataGroups[[i]]
			curGroupID<-eval(parse(text=paste("curGroup$",groupBy,"[1]",sep="")))
			population<-as.character(curGroup$population[1])
			stats<-as.character(curGroup$stats[1])
			groupTips<-paste("participantid=",curGroup$participantid[1], " ",groupBy,"=",curGroupID
					, " Tube=",curGroup$Tube[1],sep="")
			cur.btw.groups.outliers<-unique(curGroup$gOutlier)
			setSVGShapeToolTip(title=groupTips,sub.special=FALSE)
			##lattice plot for outlier group
			
			if((cur.btw.groups.outliers||plotAll==TRUE)&&!is.null(dest))
			{
#				browser()
				paths<-flowQA:::.FileNameGen(prefix="s"
											,ID=curGroupID
											,population=population
											,stats.=stats)
				
				if(!file.exists(file.path(dest,"individual")))system(paste("mkdir",file.path(dest,"individual")))
				paths<-tempfile(pattern=paths,tmpdir="individual",fileext=".png")

				##can't print right away since there is issue with embeded lattice plot
				##some how it alter the viewport or leves of parent lattice object 
#				browser()
				assign(basename(paths),qa.GroupPlot(db,curGroup),envir=plotObjs)
					
				setSVGShapeURL(paths)
			}
			
			panel.polygon(t(xs)[,i], t(ys)[,i],
					lwd = box.rectangle$lwd,
					lty = box.rectangle$lty,
					col = fill,
					alpha = box.rectangle$alpha,
					border = ifelse(cur.btw.groups.outliers,"red",box.rectangle$col),
					identifier = paste(identifier, "box", sep="."))
			
			## whiskers
			
			panel.segments(rep(levels.fos[i], 2),
					c(blist.stats[i, 2], blist.stats[i, 4]),
					rep(levels.fos[i], 2),
					c(blist.stats[i, 1], blist.stats[i, 5]),
					col = box.umbrella$col,
					alpha = box.umbrella$alpha,
					lwd = box.umbrella$lwd,
					lty = box.umbrella$lty,
					identifier = paste(identifier, "whisker", sep="."))
			
			panel.segments(levels.fos[i] - blist.height / 2,
					c(blist.stats[i, 1], blist.stats[i, 5]),
					levels.fos[i] + blist.height / 2,
					c(blist.stats[i, 1], blist.stats[i, 5]),
					col = box.umbrella$col,
					alpha = box.umbrella$alpha,
					lwd = box.umbrella$lwd,
					lty = box.umbrella$lty,
					identifier = paste(identifier, "cap", sep="."))
			
#				browser()
			## dot
			
			if (all(pch == "|"))
			{
				mult <- if (notch) 1 - notch.frac else 1
				panel.segments(levels.fos[i] - mult * blist.height / 2,
						blist.stats[i, 3],
						levels.fos[i] + mult * blist.height / 2,
						blist.stats[i, 3],
						lwd = box.rectangle$lwd,
						lty = box.rectangle$lty,
						col = box.rectangle$col,
						alpha = alpha,
						identifier = paste(identifier, "dot", sep="."))
			}
			else
			{
				panel.points(x = levels.fos[i],
						y = blist.stats[i, 3],
						pch = pch,
						col = col, alpha = alpha, cex = cex,
						fontfamily = fontfamily,
						fontface = lattice:::chooseFace(fontface, font),
						fontsize = fontsize.points,
						identifier = paste(identifier, "dot", sep="."))
			}
			
			## outliers
#				browser()
				
			for(curOutInd in which(curGroup$outlier))
			{
				curOut<-blist.x[[i]][curOutInd]
				curOutRow<-curGroup[curOutInd,,drop=FALSE]
				
				if(!is.null(dest))
				{
					FileTips<-paste("uniqueID=",curOutRow$id," file=",curOutRow$name,sep="")
					setSVGShapeToolTip(title=FileTips,sub.special=FALSE)
	#				browser()
					paths<-flowQA:::.FileNameGen(prefix="f",ID=curOutRow$id,population=as.character(curOutRow$population)
						,channel=as.character(curOutRow$channel)
						,stats=as.character(curOutRow$stats))
					if(!file.exists(file.path(dest,"individual")))system(paste("mkdir",file.path(dest,"individual")))
					paths<-tempfile(pattern=paths,tmpdir="individual",fileext=".png")

					##save the individual plot obj
#						browser()
					assign(basename(paths),qa.singlePlot(db,curOutRow),envir=plotObjs)
					
					
					setSVGShapeURL(paths)
					
				}
				panel.points(x = levels.fos[i],
					y = curOut,
					pch = plot.symbol$pch,
					col = plot.symbol$col,
					alpha = plot.symbol$alpha,
					cex = plot.symbol$cex,
					fontfamily = plot.symbol$fontfamily,
					fontface = lattice:::chooseFace(plot.symbol$fontface, plot.symbol$font),
					fontsize = fontsize.points,
					identifier = paste(identifier, "outlier", sep="."))
			
						
		}
		
		
	}
	}
}

#modify orginal stats funtion to return more info in the final output 
boxplot.statsEx<-function (x, coef = 1.5, do.conf = TRUE, do.out = TRUE) 
{
	if (coef < 0) 
		stop("'coef' must not be negative")
	nna <- !is.na(x)
	n <- sum(nna)
	stats <- stats::fivenum(x, na.rm = TRUE)
	iqr <- diff(stats[c(2, 4)])
	if (coef == 0) 
		do.out <- FALSE
	else {
		out <- if (!is.na(iqr)) {
					x < (stats[2L] - coef * iqr) | x > (stats[4L] + coef * 
								iqr)
				}
				else !is.finite(x)
		if (any(out[nna], na.rm = TRUE)) 
			stats[c(1, 5)] <- range(x[!out], na.rm = TRUE)
	}
#	browser()
	
	conf <- if (do.conf) 
		stats[3L] + c(-1.58, 1.58) * iqr/sqrt(n)
	list(stats = stats, n = n, conf = conf,x=x, outInd=if (do.out) which(out&nna)
					else numeric()
			,out = if (do.out) x[out & 
										nna] else numeric())
}

#qa.panel.densityplot<-function(x,y,frames,filter,channel,channel.name,overlap,...)
qa.panel.densityplot<-function(...)
{

	y=list(...)$y
	filter=list(...)$filter

	
	


				
	panel.densityplot.flowset(
#			x=x, y=y, 
#			frames=frames, 
#			channel=channel,
#			overlap = overlap, 
#			channel.name=channel.name, 
#			filter=filter,
			
#			fill="red",#superpose.polygon$col,
#					lty=superpose.polygon$lty,
#					lwd=superpose.polygon$lwd,
#					alpha=superpose.polygon$alpha,
#					col=superpose.polygon$border,
#					gpar, 
#			col="red",
			...)
	
	#add gate label
	ny <- nlevels(y)
	for (i in rev(seq_len(ny))){
		
		curFres<-filter[[i]]
		p.stats<-summary(curFres)@p
		#remove stats for "rest" pop(usually the first one) from mulitfilterResults produced by filters such as curv2Filter
		if(length(p.stats)>1)
			p.stats<-p.stats[-1]
		p.stats<-sprintf("%.2f%%",p.stats*100)
		
		bounds<-gateBoundary(filterDetails(curFres)[[1]]$filter,curFres)
#			browser()			
		
		
		for(j in 1:length(bounds))
		{
			
			panel.text(
					x=mean(bounds[[j]]),
					y=i-0.02,
					labels=p.stats[j], 
					col="black",
					alpha=1,
					linheight=1,
					font=1,
					cex=0.7
#					,...
			)
		}
	}	
	
	
}
## Panel function copied from flowViz and change the way it handles density color
panel.xyplot.flowframeEx <- function (x, y, frame, filter = NULL, smooth = TRUE, margin = TRUE, 
		outline = FALSE, channel.x.name, channel.y.name, pch = gpar$flow.symbol$pch, 
		alpha = gpar$flow.symbol$alpha, cex = gpar$flow.symbol$cex, 
		col = gpar$flow.symbol$col, gp,outlier=TRUE, ...) 
{
	argcolramp <- list(...)$colramp
	gpar <- flowViz.par.get()
	if (!is.null(gp)) 
		gpar <- lattice:::updateList(gpar, gp)
	if (is.null(gpar$gate$cex)) 
		gpar$gate$cex <- cex
	if (is.null(gpar$gate$pch)) 
		gpar$gate$pch <- pch
	validName <- !(length(grep("\\(", channel.x.name)) || length(grep("\\(", 
								channel.y.name)))
#				browser()
	
	if (smooth) {
		if (margin) {
			r <- range(frame, c(channel.x.name, channel.y.name))
			l <- length(x)
			inc <- apply(r, 2, diff)/1e+05
			dots <- list(...)
			nb <- if ("nbin" %in% names(dots)) 
						rep(dots$nbin, 2)
					else rep(64, 2)
			selxL <- x > r[2, channel.x.name] - inc[1]
			selxS <- x < r[1, channel.x.name] + inc[1]
			selyL <- y > r[2, channel.y.name] - inc[2]
			selyS <- y < r[1, channel.y.name] + inc[2]
			allsel <- !(selxL | selxS | selyL | selyS)
			if (sum(allsel) > 0) {
				panel.smoothScatter(x[allsel], y[allsel], range.x = list(r[, 
										1], r[, 2]), ...)
				flowViz:::addMargin(r[1, channel.x.name], y[selxS], r, 
						l, nb)
				flowViz:::addMargin(r[2, channel.x.name], y[selxL], r, 
						l, nb, b = TRUE)
				flowViz:::addMargin(x[selyS], r[1, channel.y.name], r, 
						l, nb)
				flowViz:::addMargin(x[selyL], r[2, channel.y.name], r, 
						l, nb, b = TRUE)
			}
			else {
				panel.smoothScatter(x, y, ...)
			}
		}
		else {
			panel.smoothScatter(x, y, ...)
		}
		flowViz:::plotType("gsmooth", c(channel.x.name, channel.y.name))
#		if (!is.null(filter) & validName) {
#			glpolygon(filter, frame, channels = c(channel.x.name, 
#							channel.y.name), verbose = FALSE, gpar = gpar, 
#					strict = FALSE, ...)
#		}
	}
	else {
		
		if (!is.null(argcolramp)) {
			col <- densCols(x, y, colramp = argcolramp)
		}
		panel.xyplot(x, y, col = col, cex = cex, pch = pch, main="gateName",
				alpha = alpha, ...)
		
		flowViz:::plotType("gpoints", c(channel.x.name, channel.y.name))
	}
#	browser()
	##plot filter
	if (!is.null(filter) && validName) {
		if (!is(filter, "filterResult")) 
			filter <- filter(frame, filter)
#			rest <- Subset(frame, !filter)
#			x <- exprs(rest[, channel.x.name])
#			y <- exprs(rest[, channel.y.name])
		
#			browser()
		
		if(!is.null(outlier))
		{
			gpar$gate$col<-ifelse(outlier,"red","black")	
		}else
		{
			gpar$gate$col<-"black"
		}
		
		
		glpolygon(filter, frame, channels = c(channel.x.name, 
						channel.y.name), verbose = FALSE, gpar = gpar, 
				names = FALSE, strict = FALSE)
		if(list(...)$names==FALSE)
		{
			#add gate label
			curFres<-filter
#			browser()
		
			p.stats<-flowCore:::summary(curFres)@p
			#remove stats for "rest" pop(usually the first one) from mulitfilterResults produced by filters such as curv2Filter
			if(length(p.stats)>1)
				p.stats<-p.stats[-1]
			p.stats<-sprintf("%.2f%%",p.stats*100)
			
			
			bounds<-flowQA:::gateBoundary(filterDetails(curFres)[[1]]$filter,curFres)
			
			for(i in 1:length(bounds))
			{
				
				
				xcolname<-channel.x.name
				ycolname<-channel.y.name
				xlim<-range(x)
				ylim<-range(y)
#				browser()
				if(ncol(bounds[[i]])>1)
				{
					xCenterPos<-eval(parse(text=paste("mean(bounds[[i]][,'",xcolname,"'])",sep="")))
					yCenterPos<-eval(parse(text=paste("mean(bounds[[i]][,'",ycolname,"'])",sep="")))
					
					xleft<-xCenterPos-100
					xright=xCenterPos+100
					ybottom=yCenterPos-50
					ytop=yCenterPos+50
				}else
				{
					xCenterPos<-mean(bounds[[i]])
					yCenterPos<-mean(y)
					
					xleft<-xCenterPos-diff(xlim)/6
					xright=xCenterPos+diff(xlim)/6
					
					ybottom=yCenterPos-diff(ylim)/30
					ytop=yCenterPos+diff(ylim)/30
				}
#				browser()
				panel.rect(xleft=xleft,xright=xright,
						ybottom=ybottom,ytop=ytop
#						,bg="black"						
						,fill="white"
						,border="white"
				)
				
				panel.text(
						x=xCenterPos,
						y=yCenterPos,
						labels=p.stats[i], 
						col=gpar$gate.text$col,
						alpha=gpar$gate.text$alpha,
						lineheight=gpar$gate.text$lineheight,
						font=gpar$gate.text$font,
						cex=gpar$gate.text$cex
						,...
						)
			}
			
		}
	}
}


panel.xyplot.flowsetEx <- function(x,
		frames,
		filter=NULL,
		channel.x,
		channel.y,
		...)
{
#	browser()
	nm <- as.character(x)
	if (length(nm) < 1) return()
	## 'filter' either has to be a single filter, or a list of filters matching
	## the flowSet, or a filterResultList.
	if(!is.null(filter)){
		if(!is.list(filter)){
			if(is(filter, "filter")){
				filter <- lapply(seq_along(nm), function(x) filter)
				names(filter) <- nm
			}
		}else if(!is(filter, "filterResultList"))
			filter <- as(filter, "filterResultList")
		if(!(nm %in% names(filter) || !is(filter[[nm]] ,"filter"))){
			warning("'filter' must either be a filterResultList, a single\n",
					"filter object or a named list of filter objects.",
					call.=FALSE)
			filter <- NULL
		}
	}
#	browser()
#	channel.x<-as.expression(parseChannelName(channel=as.character(channel.x),pd=pData(parameters(frames[[nm]]))))
#	channel.y<-as.expression(parseChannelName(channel=as.character(channel.y),pd=pData(parameters(frames[[nm]]))))
	x <- flowViz:::evalInFlowFrame(channel.x, frames[[nm]])
	y <- flowViz:::evalInFlowFrame(channel.y, frames[[nm]])
	
#		browser()
	panel.xyplot.flowframeEx(x, y, frame=frames[[nm]], outlier=list(...)$pd[nm,"outlier"],filter=filter[[nm]], ...)
}