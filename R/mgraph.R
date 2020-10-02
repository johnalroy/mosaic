mgraph<-function(x,y,longlat=F,neighbors=20,mutual=T)	{

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

	n <- sum(! is.na(d[1,])) + 1

	if (! is.na(neighbors) && neighbors > 0 && neighbors < n)	{
		if (mutual == T)	{
			k <- matrix(nrow=nrow(d),ncol=ncol(d),data=1)
			for (i in 1:nrow(d))	{
				k[i,d[i,] > sort(d[i,])[neighbors]] <- NA
				k[d[i,] > sort(d[i,])[neighbors],i] <- NA
			}
		} else	{
			k <- matrix(nrow=nrow(d),ncol=ncol(d))
			for (i in 1:nrow(d))	{
				k[i,d[i,] <= sort(d[i,])[neighbors]] <- 1
				k[d[i,] <= sort(d[i,])[neighbors],i] <- 1
			}
		}
		d <- d * k
	}

	o <- order(d,decreasing=T)
	g <- d
	g[! is.na(g)] <- 1

	nn <- array()
	for (i in 1:nrow(g))
		nn[i] <- list(which(g[i,] == 1))

	rd <- row(d)
	cd <- col(d)

	for (i in 1:length(d))	{
		j <- rd[o[i]]
		k <- cd[o[i]]
		nnj <- unlist(nn[j])
		nnk <- unlist(nn[k])
		if (sum(nnj %in% nnk) > 0 && length(nnj) > 2 && length(nnk) > 2)	{
			g[j,k] <- 0
			g[k,j] <- 0
			nn[j] <- list(nnj[! nnj %in% k])
			nn[k] <- list(nnk[! nnk %in% j])
		}
	}

	d[upper.tri(d)] <- NA
	g[is.na(g)] <- 0
	g[upper.tri(g)] <- NA

	mosaic <- list(length = sum(d * g,na.rm=T), area = 1.207107 * sum(d * g,na.rm=T)^2 / sum(g,na.rm=T), x = m[,1], y = m[,2], distances = as.dist(d), graph = as.dist(g))
	class(mosaic) <- 'mosaic'
	mosaic

}
