\name{Mean}
\alias{Mean}
\title{
Mean of a trapezoidal fuzzy sample
}
\description{
Given a sample of trapezoidal fuzzy numbers contained in a matrix \code{F}, the function calculates the Aumann-type mean of these numbers (which is a trapezoidal fuzzy number too). The function first checks if the input matrix \code{F} is given in the correct form (tested by \code{checkingTra}). 
}
\usage{
Mean(F)
}
\arguments{
  \item{F}{
matrix of dimension \code{n x 4} containing \code{n} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0}. The function implicitly checks if the matrix is in the correct form (tested by \code{checkingTra}).
}
}
\details{
See examples
}
\value{
The function returns the Aumann-type mean, given as a matrix of dimension \code{1 x 4}.
}
\references{
[1] Sinova, B.; De la Rosa de Saa, S.; Lubiano, M.A.; Gil, M.A.: \emph{An overview on the statistical central tendency for fuzzy datasets}, International Journal of Uncertainty, Fuzziness and Knowledge-Based Systems 23 (Suppl. 1), pp. 105-132 (2015)
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
\code{\link{checkingTra}}
}
\examples{
# Example 1:
F=SimulCASE1(100)
Mean(F)

# Example 2:
F=matrix(c(1,0,2,3),nrow=1)
Mean(F)

# Example 3:
F=matrix(c(1,0,2,3),nrow=2)
Mean(F)
}   
\keyword{univar}
