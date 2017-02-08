\name{Rho1}
\alias{Rho1}
\title{
1-norm distance between fuzzy numbers
}
\description{
This function calculates the 1-norm distance between the fuzzy numbers contained in two arrays, which should be given in the desired format. For this, the function first checks if the input arrays \code{R} and \code{S} are in the correct form (tested by \code{checking}) and if the \eqn{\alpha}-levels of all fuzzy numbers coincide. 
}
\usage{
Rho1(R, S)
}
\arguments{
  \item{R}{
array of dimension \code{nl x 3 x r} containing \code{r} fuzzy numbers characterized by means of \code{nl} \eqn{\alpha}-levels each. The function first calls \code{checking} to check if the array \code{R} has the correct format. Moreover, the \eqn{\alpha}-levels of the array \code{R} should coincide with the ones of the array \code{S} (the function checks this condition).
}
  \item{S}{
array of dimension \code{nl x 3 x s} containing \code{s} fuzzy numbers characterized by means of \code{nl} \eqn{\alpha}-levels each. The function first calls \code{checking} to check if the array \code{S} has the correct format. Moreover, the \eqn{\alpha}-levels of the array \code{S} should coincide with the ones of the array \code{R} (the function checks this condition).
}
}
\details{
See examples
}
\value{
The function returns a matrix of dimension \code{r x s} containing the 1-norm distances between the fuzzy numbers of the array \code{R} and the fuzzy numbers of the array \code{S} .
}
\references{
[1] Diamond, P.; Kloeden, P.: \emph{Metric spaces of fuzzy sets}, Fuzzy Sets Syst. 35, pp. 241-249 (1990)
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}


\seealso{
\code{\link{checking}}, \code{\link{Rho1Tra}}
}
\examples{
# Example 1:
F=SimulCASE1(4)
S=SimulCASE1(5)
F=TransfTra(F)
S=TransfTra(S)
Rho1(F,S)

# Example 2:
F=SimulCASE1(4)
S=SimulCASE1(5)
S=TransfTra(S)
Rho1(F,S)

# Example 3:
F=SimulCASE1(4)
S=SimulCASE1(5)
F=TransfTra(F)
S=TransfTra(S,10)
Rho1(F,S)
}
\keyword{arith}
