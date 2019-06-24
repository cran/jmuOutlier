\name{coin.toss}
\alias{coin.toss}
\title{Coin Toss}
\description{Graphs a simulation of the sample proportion of heads.}
\usage{ coin.toss(n, p=0.5, burn.in=0, log.scale=FALSE, col=c("black","red"), ...) }
\arguments{
  \item{n}{An integer denoting the number of times the coin is tossed.}
  \item{p}{The probability of heads, which must be between 0 and 1.}
  \item{burn.in}{An integer denoting the number of initial coin tosses which should be omitted from the graph.}
  \item{log.scale}{Logical; indicating whether or not the x-axis should have a logarithmic scale.}
  \item{col}{A vector of two colors, where the first color is used for the graph of the sample proportions,
and the second color is used for the horizontal line occurring at the value \code{p}.}
  \item{\dots}{Optional arguments to be passed to the \code{\link[graphics]{plot}} function
      (see \code{\link[graphics]{par}}).}
}
\details{This function \code{coin.toss} illustrates the Law of Large Numbers for proportions,
by simulating cumulative sample proportions.
Using nonzero \code{burn.in} typically reveals greater precision in the graph 
as the number of coin tosses increases.}
% \details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Steven T. Garren, James Madison University, Harrisonburg, Virginia, USA}

%\seealso{ }
\examples{
coin.toss( 600, 0.5 )
coin.toss( 3e4, 0.4, )
coin.toss( 3e4, 0.7, 1000, col=c("hotpink","turquoise") )
coin.toss( 7e4, 0.6, , TRUE )
coin.toss( 7e4, 0.3, 1000, TRUE, col=c("purple","green") )
}

\keyword{ Law of Large Numbers }
\keyword{ Sample proportion }
