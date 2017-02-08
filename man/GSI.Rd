\name{GSI}
\alias{GSI}
\title{
Gini-Simpson diversity index of a trapezoidal fuzzy sample
}
\description{
This function calculates the Gini-Simpson diversity index for a sample of trapezoidal fuzzy numbers contained in a matrix \code{F}. The function first checks if the input matrix \code{F} is given in the correct form (tested by \code{checkingTra}). 
}
\usage{
GSI(F)
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
The function returns the Gini-Simpson diversity index, which is a real number.
}
\references{
[1] De la Rosa de Saa, S.; Gil, M.A.; Gonzalez-Rodriguez, G.; Lopez, M.T.; Lubiano M.A.: \emph{Fuzzy rating scale-based questionnaires and their statistical analysis}, IEEE Trans. Fuzzy Syst. 23(1), pp. 111-126 (2015)
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
F=SimulCASE1(50)
GSI(F)

# Example 2:
F=matrix(c(1,0,2,3),nrow=1)
GSI(F)
}
\keyword{univar}