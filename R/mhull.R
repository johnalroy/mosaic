mhull<-function(m)	{

	degrees<-function(at,x,y,w)	{

		dx <- x[at] - x[w]
		dy <- y[at] - y[w]
		h <- (dx^2 + dy^2)^0.5
		a <- asin(dx / h) * 180 / pi

		ur <- which(x[at] < x[w] & y[at] < y[w])
		a[ur] <- -a[ur]
		lr <- which(x[at] < x[w] & y[at] >= y[w])
		a[lr] <- 180 + a[lr]
		ll <- which(x[at] >= x[w] & y[at] >= y[w])
		a[ll] <- a[ll] + 180
		ul <- which(x[at] >= x[w] & y[at] < y[w])
		a[ul] <- 360 - a[ul]

		return(a)

	}

	g <- as.matrix(m$graph)
	x <- m$x
	y <- m$y

	n <- length(x)

	re <- row(g)[g == 1]
	ce <- col(g)[g == 1]
	col <- 1:n

	for (i in 1:sum(g))
		col[col == col[re[i]]] <- col[ce[i]]

	sub <- g
	t <- table(col)
	t <- t[t > 1]
	if (length(t) == 1)	{
		subs <- array(dim=1,data=as.numeric(names(t)))
		w <- which(col == subs[1])
		sub[w,w] <- subs[1]
	} else	{
		subs <- as.numeric(names(t))
		for (i in 1:length(t))	{
			w <- which(col == subs[i])
			sub[w,w] <- subs[i]
		}
	}

	g <- sub * g

	hull.pts <- list()
	hull.x <- list()
	hull.y <- list()
	p <- vector(mode='list',length=length(subs))

	for (i in 1:length(subs))	{

		g2 <- g
		g2[g2 != subs[i]] <- 0
		w <- which(rowSums(g2) > 0)
		x2 <- x
		y2 <- y
		x2[! 1:n %in% w] <- NA
		y2[! 1:n %in% w] <- NA

		from <- 0
		at <- order(y2,decreasing=T)[1]
		if (sum(g2[at,]) == 0)
			at <- order(y2,decreasing=T)[2]

		pts <- array()
		hx <- array()
		hy <- array()
		pts[1] <- at
		hx[1] <- x2[at]
		hy[1] <- y2[at]

		start <- at
		lastang <- 360

		for (j in 2:(2 * n))	{

			w <- as.numeric(which(g2[at,] > 0 & 1:n != from))
			if (length(w) > 0)	{
				ang <- degrees(at,x2,y2,w)
				z <- ang - lastang
				z[z <= 0] <- z[z <= 0] + 360
				to <- w[which(z == min(z,na.rm=T))[1]]
				ang <- ang[which(z == min(z,na.rm=T))[1]]
			} else	{
				to <- from
				ang <- lastang
			}

			lastang <- ang + 180
			if (lastang > 360)
				lastang <- lastang - 360

			if (at == start && j == 2)
				left <- w[which(z == max(z,na.rm=T))[1]]
			from <- at
			at <- to

			pts[j] <- at
			hx[j] <- x2[at]
			hy[j] <- y2[at]

			if (at == start && from == left)
				break

		}

		hull.pts[[i]] <- pts
		hull.x[[i]] <- hx
		hull.y[[i]] <- hy
		p[[i]] <- list(cbind(hx,hy))
		attr(p[[i]],'class') <- c("XY","POLYGON","sfg")

	}

	if (length(p) == 1)	{
		hull.pts <- as.array(hull.pts[[1]])
		hull.x <- as.array(hull.x[[1]])
		hull.y <- as.array(hull.y[[1]])
		p <- p[[1]]
	} else	{
		attr(p,'class') <- c("XY","MULTIPOLYGON","sfg")
	}

	mosaic <- list(area = m$area, x = m$x, y = m$y, distances = m$distances, graph = m$g, hull.pts = hull.pts, hull.x = hull.x, hull.y = hull.y, polygon = p)
	class(mosaic) <- 'mosaic'
	mosaic

}
