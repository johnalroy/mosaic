\name{mgraph}
\alias{mgraph}
\title{
Compute a Mosaic Graph
}
\description{
\emph{mgraph} uses a graph theoretic algorithm to compute the estimated area of an unseen shape based on the distribution of data points that are known to fall within it. The mosaic can be illustrated using the \link{plot.mosaic} function.
}
\usage{
mgraph(x, y, longlat = F, neighbors = 20, mutual = T)
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
  \item{neighbors}{
The number of nearest neighbors checked to create new edges.
}
  \item{mutual}{
Should edges only be considered if each point is in the neighborhood of the other (T), or should they be considered if either point is in the neighborhood the other (F)?
}
}
\details{
Mosaics are computed by starting with a fully connected graph and removing edges one at a time in reverse order of length. An edge between points \emph{i} and \emph{j} is retained only if there is no point still linked to both of them and either \emph{i} or \emph{j} is connected to just one or two other points (or both are). This results in a graph in which each point is linked to at least two others, so there is at least one cycle.

The standard area esimate is just the sum of the squares of the edge lengths. Output estimates are based on an alternate and algebraically identical but more precise equation. This is the square root of the sum of lengths divided by the number of edges. Values are multipled by a factor of 1.207107, which follows geometrically from the deduction that mosaic pieces tend to form octagons instead of squares (no adjustment would be required in the latter case).

The \emph{neighbors} argument is used to speed up computations and should have no effect on the results if left alone. Lower values would increase computational speed, but might lead to errors.

If \emph{mutual} is true, particularly long edges will be left out, lowering the area estimate. For example, the long connecting edge will be left out if all points fall within two well-separated clusters and one of them includes at least \emph{N} points where \emph{N} is the \emph{neighbors} value. If \emph{mutual} is false, this would be less likely to occur because both clusters would have to include that many points.

\emph{mgraph} only accepts two-dimensional data sets. However, it can be used to estimate the area of a hyperdimensional shape by averaging the computed mosaic areas of all \emph{P} pairs of dimensions. This is done by multiplying the \emph{P} areas and raising the result to the power of 1/\emph{P}, which converts the value to the projected area of a two-dimensional shape. See example below.

\emph{mgraph} can be used to estimate the area of overlap between two shapes, such as geographic ranges or home ranges, by computing the shape areas individually, summing them, and subtracting the estimated area of the combined data sets. Specifically, if two ranges have respective separate areas of A + B and A + C, the area of the combined data set should be A + B + C and the area of overlap should be (A + B) + (A + C) - (A + B + C) = A. See example below.

Mosaics can be turned into polygons usng the \link{mhull} function. It supplies polygon data in the simple features format that the library \emph{sf} can understand. See \link{mhull} for details.
}
\value{
\item{length }{The mosaic length.}
\item{area }{The mosaic area.}
\item{x }{The x-axis coordinates.}
\item{y }{The y-axis coordinates.}
\item{distances }{The distance matrix.}
\item{graph }{A triangular matrix recording which pairs of points are connected by an edge. Zero means not connected and one means connected.}
}
\references{
Alroy, J. A simple graph theoretic method provides accurate range area estimates. To be submitted to \emph{Ecology Letters}.
}
\author{
John Alroy
}
\examples{
# a generic square of unit width and height
mgraph(runif(50), runif(50))$area

# a rectangle of width 1 and height 2
mgraph(runif(50), runif(50,0,2))$area

# the projected 2D area of a cube of unit width, height, and length
xyz <- matrix(nrow = 50, ncol = 3, data = runif(150))
m12 <- mgraph(xyz[,1], xyz[,2])$area
m13 <- mgraph(xyz[,1], xyz[,3])$area
m23 <- mgraph(xyz[,2], xyz[,3])$area
(m12 * m13 * m23)^(1 / 3)

# the projected 2D area of a four-dimensional hypercube
abcd <- matrix(nrow = 50, ncol = 4, data = runif(200))
a <- 1
for (i in 2:4)
	for (j in 1:(i - 1))
		a <- a * mgraph(abcd[,i], abcd[,j])$area
a^(1 / 6)

# two overlapping squares
AB <- cbind(runif(50), runif(50))
AC <- cbind(runif(50), runif(50)) + 0.5

# the area of overlap between the squares (expected value 0.25)
mgraph(AB)$area + mgraph(AC)$area - mgraph(rbind(AB, AC))$area

# the resulting pattern (see plot.mosaic for options)
plot(mgraph(rbind(AB, AC)))

}
