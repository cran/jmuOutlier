\name{read.table2}
\alias{read.table2}
\title{Reads Tables from Author's Website}
\description{Performs read.table of dataset without typing the URL.}
\usage{ read.table2(file.name, course.num=course.number, na.strings=".", ...) }
\arguments{
  \item{file.name}{The file name in character format without the URL.}
  \item{course.num}{The course number in character or numeric format, where \code{course.number} is a global variable.}
  \item{na.strings}{Character vector. Elements of this vector are to be interpreted as missing \code{NA} values.}
  \item{\dots}{Optional arguments to be passed to the \code{\link[utils]{read.table}} function.}
}
\details{The datasets are available on the author's website, http://educ.jmu.edu/~garrenst.
The global variable \code{course.number} may be entered as the value of the second argument, \code{course.num}, in function \code{read.table2}.}
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
\seealso{\code{\link[utils]{read.table}} and \code{\link{scan2}}}
\examples{
# The following two commands, when uncommented, are equivalent.

# read.table2( "ex6.1.txt", 321, header=TRUE )

# read.table( "http://educ.jmu.edu/~garrenst/math321.dir/datasets/ex6.1.txt", header=TRUE )}
\keyword{ read.table }
