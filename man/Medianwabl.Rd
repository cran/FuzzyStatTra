\name{Medianwabl}
\alias{Medianwabl}
\title{
\eqn{\varphi}-wabl/ldev/rdev median of a trapezoidal fuzzy sample
}
\description{
Given a sample of trapezoidal fuzzy numbers contained in a matrix \code{F}, the function calculates the \eqn{\varphi}-wabl/ldev/rdev median of these numbers, characterized by means of \code{nl} equidistant \eqn{\alpha}-levels (by default \code{nl}=101), including always the 0 and 1 levels, with their infimum and supremum values. The function first checks if the input matrix \code{F} is given in the correct form (tested by \code{checkingTra}). 
}
\usage{
Medianwabl(F, nl = 101, a = 1, b = 1)
}
\arguments{
  \item{F}{
matrix of dimension \code{n x 4} containing \code{n} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0}. The function implicitly checks if the matrix is in the correct form (tested by \code{checkingTra}).
}
  \item{nl}{
positive integer, by default \code{nl}=101. It indicates the number of desired \eqn{\alpha}-levels for characterizing the\eqn{\varphi}-wabl/ldev/rdev median.
}
  \item{a}{
number >0, by default \code{a}=1. It is the first parameter of a beta distribution which corresponds to a weighting measure on [0,1].
}
  \item{b}{
number >0, by default \code{b}=1. It is the second parameter of a beta distribution which corresponds to a weighting measure on [0,1].
}
}
\details{
See examples
}
\value{
The function returns the \eqn{\varphi}-wabl/ldev/rdev median, given by an array of dimension \code{nl x 3 x 1} where \code{nl} is the number of considered \eqn{\alpha}-levels and 3 the number of columns of the array: the first column will be the \eqn{\alpha}-levels, the second one their infimum values and the third one their supremum values.
}
\references{
 [1] Sinova, B.; de la Rosa de Saa, S.; Gil, M.A.: \emph{A generalized L1-type metric between fuzzy numbers for an approach to central tendency of fuzzy data}, Information Sciences 242, pp. 22-34 (2013)
 
 [2] Sinova, B.; Gil, M.A.; Van Aelst, S.: \emph{M-estimates of location for the robust central tendency of fuzzy data}, IEEE Transactions on Fuzzy Systems 24(4), pp. 945-956 (2016)
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
\code{\link{checkingTra}}, \code{\link{DwablphiTra}}, \code{\link{Dwablphi}}, \code{\link{Wablphi}}, \code{\link{Median1norm}}
}
\examples{
# Example 1:
F=SimulCASE1(10)
Medianwabl(F,3)

# Example 2:
F=matrix(c(1,0,2,3),nrow=1)
Medianwabl(F)
}
\keyword{univar}
\keyword{robust}
