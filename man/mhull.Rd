\name{mhull}
\alias{mhull}
\title{
Compute the Outline (Hull) Around a Mosaic Graph
}
\description{
Computes the hull of a mosaic, which is usually not convex. A \link{mosaic} object created by \link{mgraph} is passed in, and another with added information about the hull is passed back. Called automatically when needed by \link{plot.mosaic}.
}
\usage{
mhull(m)
}
\arguments{
  \item{m}{
A mosaic object generated by the \link{mgraph} function.
}
}
\details{
The \emph{mhull} algorithm starts with the highest point on the graph and works its way around the edge. The angle going back to the previous point is computed, and the next point chosen is the one at the next highest angle.
For example, suppose point P is connected to Q, R, and S. Q was visited in the previous step, and the angle between P and Q is 30º. If the angles between P and R and S are 180º and 100º, S will be visited next because 100 is less than 180.

The algorithm stops when the highest point is reached from the other direction (technically, meaning from the point connected to it that is at the angle closest to but below 360º).

This algorithm can fail, but only in very rare cases: specifically, when edges on the outline cross each other, or if the first point selected belongs to a fragment cut off from the rest of the mosaic by \link{mgraph}.

Doubling back on an edge is allowed if required to continue the outline (because the edge connects two isolated mosaic pieces). Therefore, \emph{mhull} often produces polygons that have more than one vertex per data point.
  
\emph{mhull} outputs the hull.pts, hull.x, and hull.y data as plain vectors if there is only one hull. Otherwise, it outputs them as a series of lists that each include vectors.

Because \emph{mhull} outputs polygons or multipolygons (as appropriate) in simple features format, the simple features library \emph{sf} can immediately interpret the data. This makes it possible to convert the outlines to SpatialPolygon format and then on to shapefile format (see example below).
}
\value{
  \item{... }{The assorted components of a standard \link{mosaic} object come first.}
  \item{hull.pts }{The identification numbers of the selected points.}
  \item{hull.x }{The x coordinates of the points forming the hull.}
  \item{hull.y }{The y coordinates of the points forming the hull.}
  \item{polygon }{The coordinates of the hull in simple features format.}
}
\references{
Alroy, J. A simple graph theoretic method provides accurate range area estimates. To be submitted to \emph{Ecology Letters}.
}
\author{
John Alroy
}
\examples{
# create a mosaic object including the hull
h <- mhull(mgraph(runif(10), runif(10)))

# make a simple plot of the hull
plot(h, fill=T)

# set the fill color to pink
plot(h, fill=T, h = 0.95, s = 1, v = 1)

# you can use sf to manipulate the polygon
library(sf)
p <- h$polygon

# sf has its own plotting routine
plot(p, col="pink")

# mgraph already computes the mosaic area, so this isn't recommended
st_area(p)

# the mosaic area is usually much greater than the hull area
h$area

# export the hull to shapefile format using the library raster
library(raster)
shapefile(as_Spatial(st_sfc(p)),'mosaic.shp')
}
