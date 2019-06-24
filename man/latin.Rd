\name{latin}
\alias{latin}
\title{Latin Square}
\description{Generates a Latin square, which is either standard or based on randomized rows and columns.}
\usage{ latin(n, random = TRUE) }
\arguments{
  \item{n}{An integer between 2 and 26, inclusively, denoting the number of treatment groups.}
  \item{random}{Logical; if \code{TRUE} (default), a Latin square with randomized rows and columns is produced.
                If \code{FALSE}, a standard non-random Latin square is produced.}
}
\details{The Latin square is produced in matrix format with treatments labeled as \emph{A}, \emph{B}, \emph{C}, etc.}
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

\seealso{\code{\link[agricolae:design.lsd]{design.lsd}} in \emph{R}-package \code{\link[agricolae]{agricolae}}}
\examples{
latin( 5, random=FALSE )
latin( 6 ) # Default is random=TRUE
}

\keyword{ Latin square }
