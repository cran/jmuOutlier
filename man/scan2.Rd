\name{scan2}
\alias{scan2}
\title{Scans Data from Author's Website}
\description{Performs scan of dataset without typing the URL.}
\usage{ scan2(file.name, course.num=course.number, na.strings=".", comment.char="#", ...) }
\arguments{
  \item{file.name}{The file name in character format without the URL.}
  \item{course.num}{The course number in character or numeric format, where \code{course.number} is a global variable.}
  \item{na.strings}{Character vector. Elements of this vector are to be interpreted as missing \code{NA} values.}
  \item{comment.char}{Single character or empty string, denoting beginning of comment.  
                      Use "" to turn off the interpretation of comments altogether.}
  \item{\dots}{Optional arguments to be passed to the \code{\link[base]{scan}} function.}
}
\details{The datasets are available on the author's website, http://educ.jmu.edu/~garrenst.
The global variable \code{course.number} may be entered as the value of the second argument, \code{course.num}, in function \code{scan2}.}
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
\seealso{\code{\link{read.table2}} and \code{\link[base]{scan}}}
\examples{
# The following two commands, when uncommented, are equivalent.

# scan2( "exercise2.7.txt", 324 )

# scan( "http://educ.jmu.edu/~garrenst/math324.dir/datasets/exercise2.7.txt", comment.char="#" )}
\keyword{ scan }
