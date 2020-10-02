plot.mosaic<-function(m,add=F,pdf=F,mar=0,xmin=NA,xmax=NA,ymin=NA,ymax=NA,cex=1,lines=T,lwd=1,squares=F,fill=F,outline=F,col=NA,h=0.5,s=0,v=0.5,alpha=0.5,pts.black=T,lines.black=T)	{

	x <- m$x
	y <- m$y
	g <- as.matrix(m$graph)
	g[upper.tri(g)] <- 0

	if (fill == T || outline == T)
		m <- mhull(m)

	if (add == F && pdf == T)
		pdf('mosiac.pdf',height=4,width=4)
	par(mar=c(mar,mar,mar,mar))
	if (is.na(xmin) || is.na(xmax))
		xlim <- range(x)
	else
		xlim <- c(xmin,xmax)
	if (is.na(ymin) || is.na(ymax))
		ylim <- range(y)
	else
		ylim <- c(x=ymin,ymax)

	if (! is.na(col) && col %in% c('red','orange','yellow','green','lightblue','blue','purple','pink'))	{
		s <- 1
		v <- 1
		if (col == 'red')
			h <- 0
		else if (col == 'orange')
			h <- 0.1
		else if (col == 'yellow')
			h <- 0.15
		else if (col == 'green')
			h <- 0.3
		else if (col == 'lightblue')
			h <- 0.5
		else if (col == 'blue')
			h <- 0.65
		else if (col == 'purple')
			h <- 0.8
		else if (col == 'pink')
			h <- 0.9
	}

	pts.s <- 0
	pts.v <- 0
	if (pts.black == F)	{
		pts.s <- s
		pts.v <- v
	}

	if (add == F)
		plot(x,y,cex=cex,pch=19,col=hsv(h=h,s=pts.s,v=pts.v),axes=F,xlab=NA,ylab=NA,xlim=xlim,ylim=ylim,xpd=F)
	else
		points(x,y,cex=cex,pch=19,col=hsv(h=h,s=pts.s,v=pts.v),xpd=F)

	if (fill == T)	{
		if (is.array(m$hull.x))
			polygon(m$hull.x,m$hull.y,col=hsv(h=h,s=s,v=v,alpha=alpha),border=NA,xpd=F)
		else
			for (i in 1:length(m$hull.x))
				polygon(m$hull.x[[i]],m$hull.y[[i]],col=hsv(h=h,s=s,v=v,alpha=alpha),border=NA,xpd=F)
		points(x,y,cex=cex,pch=19,col=hsv(h=h,s=pts.s,v=pts.v),xpd=F)
	}

	for (i in which(g == 1))	{
		j <- row(g)[i]
		k <- col(g)[i]

		lines.s <- 0
		lines.v <- 0
		if (lines.black == F)	{
			lines.s <- s
			lines.v <- v
		}
		if (lines == T)
			lines(c(x[j],x[k]),c(y[j],y[k]),lwd=lwd,col=hsv(h=h,s=lines.s,v=lines.v),xpd=F)

		if (squares == T)	{
			x1 <- x[j] + (y[k] - y[j]) / 2
			y1 <- y[j] - (x[k] - x[j]) / 2
			x2 <- x[j] - (y[k] - y[j]) / 2
			y2 <- y[j] + (x[k] - x[j]) / 2
			x3 <- x[k] + (y[j] - y[k]) / 2
			y3 <- y[k] - (x[j] - x[k]) / 2
			x4 <- x[k] - (y[j] - y[k]) / 2
			y4 <- y[k] + (x[j] - x[k]) / 2
			polygon(c(x1,x2,x3,x4),c(y1,y2,y3,y4),col=hsv(h=h,s=s,v=v,alpha=alpha),border=NA,xpd=F)
		}
	}

	if (outline == T)
		if (is.array(m$hull.x))
			polygon(m$hull.x,m$hull.y,lwd=lwd,col=NA,border=hsv(h=h,s=s,v=v,alpha=alpha),xpd=F)
		else
			for (i in 1:length(m$hull.x))
				polygon(m$hull.x[[i]],m$hull.y[[i]],lwd=lwd,col=NA,border=hsv(h=h,s=s,v=v,alpha=alpha),xpd=F)

	par(mar=c(5, 4, 4, 2) + 0.1)
	if (add == F && pdf == T)
		dev.off()

}
