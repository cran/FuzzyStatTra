\name{DthetaphiTra}
\alias{DthetaphiTra}
\title{
Mid/spr distance between trapezoidal fuzzy numbers
}
\description{
This function calculates the mid/spr distance between the trapezoidal fuzzy numbers contained in two matrixes, which should be given in the desired format. For this, the function first checks if the input matrixes \code{R} and \code{S} are in the correct form (tested by \code{checkingTra}). 
}
\usage{
DthetaphiTra(R, S, a = 1, b = 1, theta = 1/3)
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
number >0, by default \code{theta}=1/3. It is the weight of the spread in the mid/spr distance. 
}
}
\details{
See examples
}
\value{
The function returns a matrix of dimension \code{r x s} containing the mid/spr distances between the trapezoidal fuzzy numbers of the matrix \code{R} and the trapezoidal fuzzy numbers of the matrix \code{S}.
}
\references{
[1] Lubiano, M.A.; Montenegro, M.; Sinova, B.; De la Rosa de Saa, S.; Gil, M.A.: \emph{Hypothesis testing for means in connection with fuzzy rating scale-based data: algorithms and applications}, European Journal of Operational Research 251, pp. 918-929 (2016)
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}

\seealso{
\code{\link{checkingTra}}, \code{\link{Dthetaphi}}
}
\examples{
# Example 1:
F=SimulCASE1(6)
S=SimulCASE1(8)
DthetaphiTra(F,S)

# Example 2:
F=matrix(c(1,1,0,2,3,4,5,6),nrow=2)
S=SimulCASE1(8)
DthetaphiTra(F,S,1,1,1)
}
\keyword{arith}