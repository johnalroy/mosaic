\name{mosaic-package}
\alias{mosaic-package}
\alias{mosaic}
\docType{package}
\title{
\packageTitle{mosaic}
}
\description{
\packageDescription{mosaic}
}
\details{

The primary function in this package is \link{mgraph}, which creates mosaic objects that can be passed to \link{plot.mosaic} and \link{summary.mosaic} methods. \link{mhull} computes non-convex hulls (outlines) around the graphs, and \link{inhull} determines which coordinates out of any supplied list fall within the hull. \link{tgraph} computes a minimum spanning tree that can also be plotted. \emph{mosaic} also includes a simple utility function called \link{ptpattern} that randomly generates sets of data points falling into a pattern such as a circle or ring.
}
\author{
\packageAuthor{mosaic}

Maintainer: \packageMaintainer{mosaic}
}
\references{
Alroy, J. A simple graph theoretic method provides accurate range area estimates. To be submitted to \emph{Ecology Letters}.
}
\keyword{ package }
\seealso{
}
\examples{
mgraph(runif(50), runif(50))$area

pt <- ptpattern(50, shape = 'ring')

m <- mgraph(pt$x, pt$y)

summary(m)

plot(m)
}
