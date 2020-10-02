tgraph<-function(x,y,longlat=F)	{

	options(warn = -1)

	if (! missing(y))
		m <- cbind(x,y)
	else
		m <- x

	m <- na.omit(m)

	if (! is.matrix(m) || ncol(m) != 2)	{
		warning('input must be two columns of data or a two-dimensional matrix')
		return(NA)
	}

	if (longlat == F)	{
		d <- dist(m)
	} else	{
		gcd<-function(x,y,z)    {
			180 / pi * acos((sin(x * pi / 180) * sin(y * pi / 180)) + (cos(x * pi / 180) * cos(y * pi / 180) * cos(z * pi / 180)))
		}
		d <- matrix(nrow=nrow(m),ncol=nrow(m))
		for (i in 2:nrow(m))
			for (j in 1:(i - 1))
				d[i,j] <- gcd(m[i,2],m[j,2],m[i,1] - m[j,1])
		d[is.na(d)] <- 0
		d[is.nan(d)] <- 0
		d <- as.dist(d)
	}

	d <- as.matrix(d)
	diag(d) <- NA
	d[d < 1e-03] <- 0

	for (i in 1:nrow(d))
		if (sum(d[i,] == 0,na.rm=T) > 0)	{
			d[i,] <- NA
			d[,i] <- NA
		}

	o <- order(d)
	g <- d
	g[! is.na(g)] <- 0

	rd <- row(d)
	cd <- col(d)
	col <- 1:nrow(d)

	for (i in 1:length(d))	{
		j <- rd[o[i]]
		k <- cd[o[i]]
		if (col[j] != col[k] && (sum(g[j,],na.rm=T) < 2 || sum(g[k,],na.rm=T) < 2))	{
			col[col == col[j]] <- col[k]
			g[j,k] <- 1
			g[k,j] <- 1
		}
	}

	d[upper.tri(d)] <- NA
	g[is.na(g)] <- 0
	g[upper.tri(g)] <- NA

	mst <- list(length = sum(d * g,na.rm=T), area = 1.207107 / 0.65^2 * sum(d * g,na.rm=T)^2 / sum(g, na.rm=T), x = m[,1], y = m[,2], distances = as.dist(d), graph = as.dist(g))
	class(mst) <- 'mosaic'
	mst

}
