\name{tgraph}
\alias{tgraph}
\title{
Compute a Minimum Spanning Tree
}
\description{
\emph{tgraph} uses the algorithm of Kruskal (1956) to compute a minimum spanning tree (MST), and also outputs the length of the tree and the area estimate it implies. The MST can be illustrated using the \link{plot.mosaic} function.
}
\usage{
tgraph(x, y, longlat = F)
}
\arguments{
  \item{x}{
An array of x-axis coordinates or a two-column matrix giving the x and y coordinates.
}
  \item{y}{
An array of y-axis coordinates, only needed if no matrix was supplied.
}
  \item{longlat}{
Are the x and y longitude and latitude coordinates in degrees?
}
}
\details{
Kruskal's algorithm starts with an entirely disconnected graph and adds edges one at a time in ascending order of length. An edge between points \emph{i} and \emph{j} is added only if the points belong to different, disconnected branches. This results in a graph with no cycles.

A valid and precise area estimator can be obtained from the sum of the lengths of the edges of an MST. The sum is squared, divided by the number of edges, and multiplied by 1.207107/0.65. The correction term is necessary because (1) MSTs usually run within the corresponding mosaics, and because these tend to be formed of octagagons, they need to be scaled up by this factor to provide accurate area estimates (see \link{mgraph}); and (2) the summed length of an MST is on average 0.65 the summed length of a mosaic graph because MSTs usually span four out of five edges in a mosaic, excluding the longest one, which tends to be 1.5 times as long as the others because a two-way division usually produces a 3:1 ratio. Thus, the MST includes 3.5 average edge lengths out of every 5 that appear in the mosaic.
}
\value{
\item{length }{The MST length.}
\item{area }{The MST area estimate.}
\item{x }{The x-axis coordinates.}
\item{y }{The y-axis coordinates.}
\item{distances }{The distance matrix.}
\item{graph }{A triangular matrix recording which pairs of points are connected by an edge. Zero means not connected and one means connected.}
}
\references{
Alroy, J. A simple graph theoretic method provides accurate range area estimates. To be submitted to \emph{Ecology Letters}.

Kruskal, J. B. 1956. On the shortest spanning subtree of a graph and the traveling salesman problem. \emph{Proceedings of the American Mathematical Society} 7, 48-50.
}
\author{
John Alroy
}
\examples{
# the length of an MST falling within a generic square of unit width and height
tgraph(runif(50), runif(50))$length

# a plot of the tree (see plot.mosaic for options)
plot(tgraph(runif(50), runif(50)))

# MSTs are sample size-biased
l10 <- array()
l50 <- array()
for (i in 1:100)	{
	l10[i] <- tgraph(runif(10), runif(10))$length
	l50[i] <- tgraph(runif(50), runif(50))$length
}
median(l10)
median(l50)

# mosaic graphs aren't, and area values are accurate
a10 <- array()
a50 <- array()
for (i in 1:100)	{
	a10[i] <- mgraph(runif(10), runif(10))$area
	a50[i] <- mgraph(runif(50), runif(50))$area
}
median(a10)
median(a50)
}
