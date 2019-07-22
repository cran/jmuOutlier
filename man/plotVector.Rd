\name{plotVector}
\alias{plotVector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting Vector Addition}
\description{
Plots one or two 2-dimensional vectors along with their vector sum.
}
\usage{
plotVector(x1, y1, x2 = NULL, y2 = NULL, add.vectors = TRUE, 
           col = c("black", "red", "darkgreen", "purple"), lwd = 8, font = 2, 
           font.lab = 2, las = 1, cex.lab = 1.3, cex.axis = 2, usr = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{Value on the x-axis of the first vector.}
  \item{y1}{Value on the y-axis of the first vector.}
  \item{x2}{Value on the x-axis of the second vector.}
  \item{y2}{Value on the y-axis of the second vector.}
  \item{add.vectors}{Logical; if \code{TRUE} (default), display the vector sum.}
  \item{col}{A vector of size four, specifying the colors of the first vector, the second vector,
             the vector sum, and parallel lines, respectively. Type \code{colors()} for selections.}
  \item{lwd}{The line width of the vectors.}
  \item{font}{An integer specifying which font to use for text.}
  \item{font.lab}{The font to be used for \code{x} and \code{y} labels.}
  \item{las}{Numeric in (0,1,2,3); the style of axis labels.}
  \item{cex.lab}{The magnification to be used for \code{x} and \code{y} labels.}
  \item{cex.axis}{The magnification to be used for axis annotation.}
  \item{usr}{A vector of the form \code{c(x1, x2, y1, y2)} giving the extremes of the user
             coordinates of the plotting region.}
  \item{\dots}{Optional arguments to be passed to the \code{\link[graphics]{plot}} function
      (see \code{\link[graphics]{par}}).  }
}
%\details{}
%\value{}
%\references{
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{ }

\seealso{ \code{\link[graphics]{plot}} and \code{\link[graphics]{curve}}. }
\examples{
par( mfrow=c(2,2) )

# Vectors (2,8) and (4,-3) and their vector sum.
     plotVector( 2, 8, 4, -3 ) 
     
# Colinear vectors (-3,6) and (-1,2).
     plotVector( -3, 6, -1, 2, add=FALSE, col=c("red","black") )

# Colinear vectors (-1,2) and (3,-6).
     plotVector( -1, 2, 3, -6, add=FALSE )

# Vectors (2,3) and (5,-4)
     plotVector( 2, 3, 5, -4, add=FALSE, usr=c( -5, 5, -4, 7) )

par( mfrow=c(1,1) )
}
\keyword{ Vector }
