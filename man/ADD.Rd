\name{ADD}
\alias{ADD}
\title{
Average Distance Deviation of a trapezoidal fuzzy sample with respect to a fuzzy number
}
\description{
This function calculates the scale measure Average Distance Deviation (ADD) for a matrix of trapezoidal fuzzy numbers \code{F} with respect to a fuzzy number \code{U}. The employed metric in the calculation can be the 1-norm distance, the mid/spr distance or the \eqn{(\varphi,\theta)}-wabl/ldev/rdev distance. The function first checks if the input matrix \code{F} is given in the correct form (tested by \code{checkingTra}) and also the input fuzzy number \code{U} (tested by \code{checking} or \code{checkingTra}).
}
\usage{
ADD(F, U, type, a = 1, b = 1, theta = 1/3)
}
\arguments{
  \item{F}{
matrix of dimension \code{n x 4} containing \code{n} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0}. The function implicitly checks if the matrix is in the correct form (tested by \code{checkingTra}).
}
  \item{U}{
can be a matrix of dimension \code{1 x 4} (trapezoidal fuzzy number) or an array of dimension \code{nl x 3 x 1} (general fuzzy number), where \code{nl} is the number of considered \eqn{\alpha}-levels and 3 the number of columns of the array: the first column will be the \eqn{\alpha}-levels, the second one their infimum values and the third one their supremum values. The function implicitly checks if the fuzzy number \code{U} is in the correct form (tested by \code{checking} if \code{U} is an array and tested by \code{checkingTra} if \code{U} is a matrix).
}
  \item{type}{
number 1, 2 or 3: if \code{type}==1, the 1-norm distance will be considered in the calculation of the measure ADD. If \code{type}==2, the mid/spr distance will be considered. By contrast, if \code{type}==3, the \eqn{(\varphi,\theta)}-wabl/ldev/rdev distance will be used.
}
  \item{a}{
number >0, by default \code{a}=1. It is the first parameter of a beta distribution which corresponds to a weighting measure on [0,1] in the mid/spr distance or in the \eqn{(\varphi,\theta)}-wabl/ldev/rdev distance. 
}
  \item{b}{
number >0, by default \code{b}=1. It is the second parameter of a beta distribution which corresponds to a weighting measure on [0,1] in the mid/spr distance or in the \eqn{(\varphi,\theta)}-wabl/ldev/rdev distance.
}
  \item{theta}{
number >0, by default \code{theta}=1/3. It is the weight of the spread in the mid/spr distance and the weight of the ldev and rdev in the \eqn{(\varphi,\theta)}-wabl/ldev/rdev distance. 
}
}
\details{
See examples
}
\value{
The function returns the scale measure ADD, which is a real number.
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
\code{\link{checkingTra}}, \code{\link{checking}}, \code{\link{TransfTra}}, \code{\link{Rho1Tra}}, \code{\link{Rho1}}, \code{\link{DthetaphiTra}}, \code{\link{Dthetaphi}}, \code{\link{DwablphiTra}}, \code{\link{Dwablphi}}
}
\examples{
# Example 1:
F=SimulCASE1(10)
U=Mean(F)
ADD(F,U,1)

# Example 2:
F=SimulCASE1(100)
U=Median1norm(F)
ADD(F,U,2,2,1,1)

# Example 3:
F=SimulCASE1(100)
U=matrix(c(1,2,3,2),nrow=1)
ADD(F,U,1)

# Example 4:
F=matrix(1:4,nrow=2)
U=matrix(1:4,nrow=1)
ADD(F,U,3,1,1,1)
}
\keyword{univar}
