\name{DwablphiTra}
\alias{DwablphiTra}
\title{
\eqn{(\varphi,\theta)}-wabl/ldev/rdev distance between trapezoidal fuzzy numbers
}
\description{
This function calculates the \eqn{(\varphi,\theta)}-wabl/ldev/rdev distance between the trapezoidal fuzzy numbers contained in two matrixes, which should be given in the desired format. For this, the function first checks if the input matrixes \code{R} and \code{S} are in the correct form (tested by \code{checkingTra}). 
}
\usage{
DwablphiTra(R, S, a = 1, b = 1, theta = 1)
}
\arguments{
  \item{R}{
matrix of dimension \code{r x 4} containing \code{r} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0}. The function first calls \code{checkingTra} to check if the matrix \code{R} has the correct format.
}
  \item{S}{
matrix of dimension \code{s x 4} containing \code{s} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0}. The function first calls \code{checkingTra} to check if the matrix \code{S} has the correct format.
}
  \item{a}{
number >0, by default \code{a}=1. It is the first parameter of a beta distribution which corresponds to a weighting measure on [0,1].
}
  \item{b}{
number >0, by default \code{b}=1. It is the second parameter of a beta distribution which corresponds to a weighting measure on [0,1].
}
  \item{theta}{
number >0, by default \code{theta}=1. It is the weight of the ldev and rdev in the \eqn{(\varphi,\theta)}-wabl/ldev/rdev distance.
}
}
\details{
See examples
}
\value{
The function returns a matrix of dimension \code{r x s} containing the \eqn{(\varphi,\theta)}-wabl/ldev/rdev distances between the trapezoidal fuzzy numbers of the matrix \code{R} and the trapezoidal fuzzy numbers of the matrix \code{S}.
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
\code{\link{checkingTra}}, \code{\link{Dwablphi}}, \code{\link{Wablphi}}
}
\examples{
# Example 1:
F=SimulCASE1(10)
S=SimulCASE1(20)
DwablphiTra(F,S,5,1,1)


# Example 2:
F=matrix(c(1,1,0,2,3,4,5,6),nrow=2)
S=SimulCASE1(8)
DwablphiTra(F,S)
}
\keyword{arith}
