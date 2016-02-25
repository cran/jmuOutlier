\name{dlaplace}
\alias{dlaplace}
\title{
Laplace (Double Exponential) Density Function
}
\description{
Laplace (double exponential) density with mean equal to \code{mean} and standard deviation equal to \code{sd}. 
}
\usage{
dlaplace(x, mean = 0, sd = 1)

}
\arguments{
  \item{x}{Vector of quantiles.}
  \item{mean}{Population mean.}
  \item{sd}{Population standard deviation.}
}
\details{The Laplace distribution has density
\eqn{e^{-|x-\mu| \sqrt{2} / \sigma} / (\sigma \sqrt{2}),}
where \eqn{\mu} is the mean of the distribution and \eqn{\sigma} is the standard deviation.}
\value{\code{dlaplace} gives the density.}
\references{Higgins, J. J. (2004) \emph{Introduction to Modern Nonparametric Statistics}.}
\author{
Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA
}
\note{The formulas computed within \code{dlaplace} are based on the textbook by Higgins (2004).}

\seealso{\code{\link{plaplace}}, \code{\link{qlaplace}}, and \code{\link{rlaplace}}.}
\examples{ dlaplace( seq( 20, 80, length.out=11 ), 50, 10 ) 


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Laplace }
\keyword{ Double exponential }% __ONLY ONE__ keyword per line
