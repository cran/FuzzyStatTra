\name{Dwablphi}
\alias{Dwablphi}
\title{
\eqn{(\varphi,\theta)}-wabl/ldev/rdev distance between fuzzy numbers
}
\description{
This function calculates the \eqn{(\varphi,\theta)}-wabl/ldev/rdev distance between the fuzzy numbers contained in two arrays, which should be given in the desired format. For this, the function first checks if the input arrays \code{R} and \code{S} are in the correct form (tested by \code{checking}) and if the \eqn{\alpha}-levels of all fuzzy numbers coincide. 
}
\usage{
Dwablphi(R, S, a = 1, b = 1, theta = 1)
}
\arguments{
  \item{R}{
array of dimension \code{nl x 3 x r} containing \code{r} fuzzy numbers characterized by means of \code{nl} \eqn{\alpha}-levels each. The function first calls \code{checking} to check if the array \code{R} has the correct format. Moreover, the \eqn{\alpha}-levels of the array \code{R} should coincide with the ones of the array \code{S} (the function checks this condition).
}
  \item{S}{
array of dimension \code{nl x 3 x s} containing \code{s} fuzzy numbers characterized by means of \code{nl} \eqn{\alpha}-levels each. The function first calls \code{checking} to check if the array \code{S} has the correct format. Moreover, the \eqn{\alpha}-levels of the array \code{S} should coincide with the ones of the array \code{R} (the function checks this condition).
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
The function returns a matrix of dimension \code{r x s} containing the \eqn{(\varphi,\theta)}-wabl/ldev/rdev distances between the fuzzy numbers of the array \code{R} and the fuzzy numbers of the array \code{S} .
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
\code{\link{checking}}, \code{\link{DwablphiTra}}, \code{\link{Wablphi}} 
}
\examples{
# Example 1:
F=SimulCASE1(3)
S=SimulCASE1(4)
F=TransfTra(F)
S=TransfTra(S)
Dwablphi(F,S,2,1,1)

# Example 2:
F=SimulCASE1(10)
S=SimulCASE1(10)
Dwablphi(F,S)

# Example 3:
F=SimulCASE1(10)
S=SimulCASE1(10)
F=TransfTra(F)
S=TransfTra(S,50)
Dwablphi(F,S,2,1,1)
}
\keyword{arith}
