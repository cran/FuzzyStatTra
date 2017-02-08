\name{Dthetaphi}
\alias{Dthetaphi}
\title{
Mid/spr distance between fuzzy numbers
}
\description{
This function calculates the mid/spr distance between the fuzzy numbers contained in two arrays, which should be given in the desired format. For this, the function first checks if the input arrays \code{R} and \code{S} are in the correct form (tested by \code{checking}) and if the \eqn{\alpha}-levels of all fuzzy numbers coincide. 
}
\usage{
Dthetaphi(R, S, a = 1, b = 1, theta = 1/3)
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
number >0, by default \code{theta}=1/3. It is the weight of the spread in the mid/spr distance. 
}
}
\details{
See examples
}
\value{
The function returns a matrix of dimension \code{r x s} containing the mid/spr distances between the fuzzy numbers of the array \code{R} and the fuzzy numbers of the array \code{S} .
}
\references{
[1] Blanco-Fernandez, A.; Casals, R.M.; Colubi, A.; Corral, N.; Garcia-Barzana, M.; Gil, M.A.; Gonzalez-Rodriguez, G.; Lopez, M.T.; Lubiano, M.A.; Montenegro, M.; Ramos-Guajardo, A.B.; de la Rosa de Saa, S.; Sinova, B.: \emph{Random fuzzy sets: A mathematical tool to develop statistical fuzzy data analysis}, Iranian Journal on Fuzzy Systems 10(2), pp. 1-28 (2013)
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}

\seealso{
\code{\link{checking}}, \code{\link{DthetaphiTra}} 
}
\examples{
# Example 1:
F=SimulCASE1(10)
S=SimulCASE1(20)
F=TransfTra(F)
S=TransfTra(S)
Dthetaphi(F,S,1,5,1)

# Example 2:
F=SimulCASE1(10)
S=SimulCASE1(10)
Dthetaphi(F,S,2,1,1/3)

# Example 3:
F=SimulCASE1(10)
S=SimulCASE1(10)
F=TransfTra(F)
S=TransfTra(S,50)
Dthetaphi(F,S,2,1,1)
}
\keyword{arith}
