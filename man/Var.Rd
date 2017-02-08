\name{Var}
\alias{Var}
\title{
Variance of a trapezoidal fuzzy sample
}
\description{
Given a sample of trapezoidal fuzzy numbers contained in a matrix \code{F}, the function calculates the variance of these numbers with respect to the mid/spr distance. The function first checks if the input matrix \code{F} is given in the correct form (tested by \code{checkingTra}). 
}
\usage{
Var(F, a = 1, b = 1, theta = 1/3)
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
  \item{theta}{
number >0, by default \code{theta}=1/3. It is the weight of the spread in the mid/spr distance. 
}
}
\details{
See examples
}
\value{
The function returns the variance of the sample with respect to the mid/spr distance, which is a real number.
}
\references{
[1] De la Rosa de Saa, S.; Lubiano M.A.; Sinova, B.; Filzmoser, P.: \emph{Robust scale estimators for fuzzy data}, Advances in Data Analysis and Classification, pp. 1-28 (2015) 
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}

\seealso{
\code{\link{checkingTra}}, \code{\link{Mean}}, \code{\link{DthetaphiTra}} 
}
\examples{
# Example 1:
F=SimulCASE1(10)
Var(F,1,1,1)

# Example 2:
F=matrix(c(1,0,2,3),nrow=1)
Var(F)
}
\keyword{univar}