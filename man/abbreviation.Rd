\name{abbreviation}
\alias{abbreviation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Allows Abbreviations of Character Data}
\description{
Determines if one character variable is an abbreviation among a section of other character variables.}
\usage{
abbreviation(x, choices)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A character string, and consists of some or all letters in a value in \code{choices} or may equal \code{choices}.
}
  \item{choices}{A vector of character strings.}
}
\details{
The function \code{abbreviation} returns a value in \code{choices} specified by \code{x}, 
which may be an abbreviation.  If no such abbreviation exists, then the original value of \code{x} is returned.
}

\value{The value in \code{choices}, which can be abbreviated by \code{x}.}
%\references{}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}
%\note{ }

%\seealso{ }
\examples{
x1 = "two";  x2 = "l";   x3 = "gr";   x4 = "greater";  x5 = "NotInChoices"
choices = c("two.sided", "less", "greater")
abbreviation( x1, choices ) ; abbreviation( x2, choices ) ; abbreviation( x3, choices ) 
abbreviation( x4, choices ) ; abbreviation( x5, choices )
}

\keyword{ abbreviation }
