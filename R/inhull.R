inhull<-function(m,x,y)	{
	within <- array(dim=length(x),data=0)
	if (length(m$hull.x) == 0)
		m <- mhull(m)
	hxs <- list()
	hys <- list()
	if (is.array(m$hull.x))	{
		hxs[[1]] <- m$hull.x
		hys[[1]] <- m$hull.y
	} else	{
		hxs <- m$hull.x
		hys <- m$hull.y
	}
	for (h in 1:length(hxs))	{
		hx <- hxs[[h]]
		hy <- hys[[h]]
		n <- length(hx)
			for (i in 1:length(x))	{
			k <- 0
			for (j in 1:(n - 1))	{
				dy1 <- hy[j] - y[i]
				dy2 <- hy[j + 1] - y[i]
				if ((dy1 < 0 && dy2 < 0) || (dy1 > 0 && dy2 > 0))
					next
				dx1 <- hx[j] - x[i]
				dx2 <- hx[j + 1] - x[i]
				if (dy2 > 0 && dy1 <= 0)	{
					f <- dx1 * dy2 - dx2 * dy1
					if (f > 0)	{
						k <- k + 1
					} else if (f == 0)	{
						within[i] <- 1
						break
					}
				} else if (dy1 > 0 && dy2 <= 0)	{
					f <- dx1 * dy2 - dx2 * dy1
					if (f < 0)	{
						k <- k + 1
					} else if (f == 0)	{
						within[i] <- 1
						break
					}
				} else if (dy2 == 0 && dy1 < 0)	{
					f <- dx1 * dy2 - dx2 * dy1
					if (f == 0)	{
						within[i] <- 1
						break
					}
				} else if (dy1 == 0 && dy2 < 0)	{
					f <- dx1 * dy2 - dx2 * dy1
					if (f == 0)	{
						within[i] <- 1
							break
					}
				} else if (dy1 == 0 && dy2 == 0)	{
					if (dx2 <= 0 && dx1 >= 0)	{
						within[i] <- 1
						break
					} else if (dx1 <= 0 && dx2 >= 0)	{
						within[i] <- 1
						break
					}
				}
			}
			if (k %% 2 == 0 && within[i] == 0)
				within[i] <- 0
			else
				within[i] <- 1
		}
	}
	return(list(x = x, y = y, within = within))
}
