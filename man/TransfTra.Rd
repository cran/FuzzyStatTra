\name{TransfTra}
\alias{TransfTra}
\title{
Transformation of a matrix of trapezoidal fuzzy numbers into an array
}
\description{
This function transforms a matrix of dimension \code{n x 4} containing \code{n} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0} into an array of dimension \code{nl x 3 x n} containing these \code{n} fuzzy numbers characterized by means of \code{nl} equidistant \eqn{\alpha}-levels each (by default \code{nl}=101). The function first checks if the input matrix \code{F} is given in the correct form (tested by \code{checkingTra}). In case yes, the function returns an array given in the format explained in the function \code{checking}.
}
\usage{
TransfTra(F, nl = 101)
}
\arguments{
  \item{F}{
matrix of dimension \code{n x 4} containing \code{n} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0}. The function implicitly checks if the matrix is in the correct form (tested by \code{checkingTra}).
}
  \item{nl}{
positive integer, by default \code{nl}=101. It indicates the number of desired \eqn{\alpha}-levels for characterizing the trapezoidal fuzzy numbers.
}
}
\details{
See examples
}
\value{
The function returns an array of dimension \code{nl x 3 x n} containing the \code{n} trapezoidal fuzzy numbers characterized by means of \code{nl} \eqn{\alpha}-levels. The first column of the array are the \eqn{\alpha}-levels, the second one their infimum values and the third one their supremum values. The correct format of the array is explained in the function \code{checking}.
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
\code{\link{checkingTra}}, \code{\link{checking}}
}
\examples{
# Example 1:
F=SimulCASE3(10)
TransfTra(F,200)

# Example 2:
F=matrix(c(1,1,0,2,3,4,5,6),nrow=2)
TransfTra(F)
}
\keyword{manip}
