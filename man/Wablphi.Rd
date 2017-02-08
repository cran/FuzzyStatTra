\name{Wablphi}
\alias{Wablphi}
\title{
\eqn{\varphi}-wabl values of a trapezoidal fuzzy sample
}
\description{
Given a sample of trapezoidal fuzzy numbers contained in a matrix \code{F}, the function calculates the \eqn{\varphi}-wabl value for each of these numbers. The function first checks if the input matrix \code{F} is given in the correct form (tested by \code{checkingTra}). 
}
\usage{
Wablphi(F, a = 1, b = 1)
}
\arguments{
  \item{F}{
matrix of dimension \code{n x 4} containing \code{n} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0}. The function implicitly checks if the matrix is in the correct form (tested by \code{checkingTra}).
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
The function returns a vector giving the \eqn{\varphi}-wabl values of each trapezoidal fuzzy number.
}
\references{
[1] Sinova, B.; Gil, M.A.; Lopez, M.T.; Van Aelst, S.: \emph{A parameterized L2 metric between fuzzy numbers and its parameter interpretation}, Fuzzy Sets and Systems 245, pp. 101-115 (2014)
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}

\seealso{
\code{\link{checkingTra}}, \code{\link{DwablphiTra}}, \code{\link{Dwablphi}}, \code{\link{Medianwabl}}
}
\examples{
# Example 1:
F=SimulCASE4(60)
Wablphi(F,2,1)

# Example 2:
F=matrix(c(1,0,2,3),nrow=1)
Wablphi(F)
}
\keyword{univar}