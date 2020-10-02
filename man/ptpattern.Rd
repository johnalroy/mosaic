\name{ptpattern}
\alias{ptpattern}
\title{
Generate Points Falling Into a Shape
}
\description{
\emph{ptpattern} generates a random set of x and y coordinates that fall within a user-specified shape.
}
\usage{
ptpattern(points = 100, shape = "circle", diameter = 1, asp = 4)
}
\arguments{
  \item{points}{
The number of data points that should be generated at random.
}
  \item{shape}{
The shape within which the points will be placed. The options are circle, square, rectangle, and ring, with circle the default.
}
  \item{diameter}{
The diameter of the shape in units. For example, if the shape is a square and the diameter is 2 the output coordinates will range from -1 to 1.
}
  \item{asp}{
The aspect ratio of a rectangle, specifically meaning the x-axis width divided by the y-axis height. This argument is only interpreted if the shape is a rectangle.
}
}
\value{
\item{x }{A vector of x-axis coordinates.}
\item{y }{A vector of y-axis coordinates.}
\item{area }{The area of the shape within which the points are placed, not the actual area occupied by the generated points.}
}
\references{
Alroy, J. A simple graph theoretic method provides accurate range area estimates. To be submitted to \emph{Ecology Letters}.
}
\author{
John Alroy
}
\examples{

## create 20 points falling in a rectangle with a 10:1 aspect ratio
ptpattern(points = 20, shape = "rectangle", asp = 10)

## create a ring-shaped pattern of points with a diameter of 10 units
ptpattern(shape =  "ring", diameter = 10)

## plot a mosaic using ptpattern data
p <- ptpattern(shape =  "ring", diameter = 10)
plot(mgraph(p$x,p$y))

}
