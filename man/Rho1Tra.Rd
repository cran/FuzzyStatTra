\name{Rho1Tra}
\alias{Rho1Tra}
\title{
1-norm distance between trapezoidal fuzzy numbers
}
\description{
This function calculates the 1-norm distance between the trapezoidal fuzzy numbers contained in two matrixes, which should be given in the desired format. For this, the function first checks if the input matrixes \code{R} and \code{S} are in the correct form (tested by \code{checkingTra}). 
}
\usage{
Rho1Tra(R, S)
}
\arguments{
  \item{R}{
matrix of dimension \code{r x 4} containing \code{r} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0}. The function first calls \code{checkingTra} to check if the matrix \code{R} has the correct format.
}
  \item{S}{
matrix of dimension \code{s x 4} containing \code{s} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0}. The function first calls \code{checkingTra} to check if the matrix \code{S} has the correct format.
}
}
\details{
See examples
}
\value{
The function returns a matrix of dimension \code{r x s} containing the 1-norm distances between the trapezoidal fuzzy numbers of the matrix \code{R} and the trapezoidal fuzzy numbers of the matrix \code{S}.
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
\code{\link{checkingTra}}, \code{\link{Rho1}}
}
\examples{
# Example 1:
F=SimulCASE2(4)
S=SimulCASE3(5)
Rho1Tra(F,S)

# Example 2:
F=matrix(c(1,1,0,2,3,4,5,6),nrow=2)
S=SimulCASE3(5)
Rho1Tra(F,S)
}
\keyword{arith}
