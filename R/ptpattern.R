ptpattern <-
function(points=100,shape='circle',diameter=1,asp=4){

D <- diameter

center <- cbind(0,0)

if (shape != 'square' && shape != 'rectangle' && shape != 'ring'){

area <- pi * D^2 / 4

x <- runif(points * 10)
y <- runif(points * 10)

x <- x * D - D / 2
y <- y * D - D / 2

for (j in 1:(points * 10))
if (dist(rbind(center,c(x[j],y[j]))) > D / 2)
x[j] <- NA

w <- sample(which(! is.na(x)),points)
x <- x[w]
y <- y[w]

}

if (shape == 'square'){

area <- D^2

x <- runif(points,-D / 2,D / 2)
y <- runif(points,-D / 2,D / 2)

y <- y[! is.na(x)]
x <- x[! is.na(x)]

}

if (shape == 'rectangle'){

area <- asp * D^2

x <- runif(points,-D / 2 * asp,D / 2 * asp)
y <- runif(points,-D / 2,D / 2)

y <- y[! is.na(x)]
x <- x[! is.na(x)]

}

if (shape == 'ring'){

area <- pi * D^2 / 8

x <- runif(points * 10)
y <- runif(points * 10)

x <- x * D - D / 2
y <- y * D - D / 2

for (j in 1:(points * 10)){
d <- dist(rbind(center,c(x[j],y[j])))
if (d > D / 2 || d < D / 2 / 2^.5)
x[j] <- NA
}

w <- sample(which(! is.na(x)),points)
x <- x[w]
y <- y[w]

}

return(list(x=x, y=y, area=area))

}
