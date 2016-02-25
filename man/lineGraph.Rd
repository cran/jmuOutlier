\name{lineGraph}
\alias{lineGraph}
\title{Line Graph Plotting}
\description{Constructs a line graph.}
\usage{
lineGraph(x, freq = TRUE, prob = NULL, col = "red", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector of numerical observations to be graphed.}
  \item{freq}{Logical; if \code{freq} is \code{FALSE} or \code{prob} sums to 1, then relative frequencies are graphed;
              otherwise, frequencies are graphed.}
  \item{prob}{Vector of the probabilities or weights on \code{x},
   and does not need to sum to one.  If \code{prob} is \code{NULL}, 
   then all \code{x} values are equally weighted.  Also, see \code{freq}.}
  \item{col}{The color of the plotted lines.  Type \code{colors()} for selections.}
  \item{\dots}{Optional arguments to \code{\link[graphics]{plot}}.}
}
%\details{ }
%\value{ }
%\references{ }
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{ }

\seealso{\code{\link[graphics]{hist}}} 
\examples{
lineGraph( c( rep(6,4), rep(9,7), rep(3,5), 5, 8, 8 ) )
lineGraph( c( rep(6,4), rep(9,7), rep(3,5), 5, 8, 8 ), freq=FALSE )
lineGraph( 11:14, , c( 12, 9, 17, 5 ) ) }

\keyword{ Line graph }
