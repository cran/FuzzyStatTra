\name{Median1norm}
\alias{Median1norm}
\title{
1-norm median of a trapezoidal fuzzy sample
}
\description{
Given a sample of trapezoidal fuzzy numbers contained in a matrix \code{F}, the function calculates the 1-norm median of these numbers, characterized by means of \code{nl} equidistant \eqn{\alpha}-levels (by default \code{nl}=101), including always the 0 and 1 levels, with their infimum and supremum values. The function first checks if the input matrix \code{F} is given in the correct form (tested by \code{checkingTra}). 
}
\usage{
Median1norm(F, nl = 101)
}
\arguments{
  \item{F}{
matrix of dimension \code{n x 4} containing \code{n} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0}. The function implicitly checks if the matrix is in the correct form (tested by \code{checkingTra}).
}
  \item{nl}{
positive integer, by default \code{nl}=101. It indicates the number of desired \eqn{\alpha}-levels for characterizing the 1-norm median.
}
}
\details{
See examples
}
\value{
The function returns the 1-norm median, given by an array of dimension \code{nl x 3 x 1} where \code{nl} is the number of considered \eqn{\alpha}-levels and 3 the number of columns of the array: the first column will be the \eqn{\alpha}-levels, the second one their infimum values and the third one their supremum values.
}
\references{
[1] Sinova, B.; Gil, M.A.; Colubi, A.; Van Aelst, S.: \emph{The median of a random fuzzy number. The 1-norm distance approach}, Fuzzy Sets Syst. 200, pp. 99-115 (2012)
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
\code{\link{checkingTra}},  \code{\link{TransfTra}}, \code{\link{Medianwabl}}
}
\examples{
# Example 1:
F=SimulCASE1(10)
Median1norm(F,200)

# Example 2:
F=matrix(c(1,0,2,3),nrow=1)
Median1norm(F)
}
\keyword{univar}
\keyword{robust}
