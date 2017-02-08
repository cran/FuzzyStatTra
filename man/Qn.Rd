\name{Qn}
\alias{Qn}
\title{
Qn scale measure of a trapezoidal fuzzy sample 
}
\description{
This function calculates the scale measure Qn for a matrix of trapezoidal fuzzy numbers \code{F}. The employed metric in the calculation can be the 1-norm distance, the mid/spr distance or the \eqn{(\varphi,\theta)}-wabl/ldev/rdev distance. The function first checks if the input matrix \code{F} is given in the correct form (tested by \code{checkingTra}).
}
\usage{
Qn(F, type, a = 1, b = 1, theta = 1/3)
}
\arguments{
  \item{F}{
matrix of dimension \code{n x 4} containing \code{n} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0}. The function implicitly checks if the matrix is in the correct form (tested by \code{checkingTra}).
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
The function returns the scale measure Qn, which is a real number.
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
\code{\link{checkingTra}}, \code{\link{Rho1Tra}}, \code{\link{DthetaphiTra}}, \code{\link{DwablphiTra}}
}
\examples{
# Example 1:
F=SimulCASE1(10)
Qn(F,3,1,1,1)

# Example 2:
F=matrix(c(1,3,2,2),nrow=1)
Qn(F,2,5,1,1)
}
\keyword{univar}
\keyword{robust}
