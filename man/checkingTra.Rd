\name{checkingTra}
\alias{checkingTra}
\title{
Checking correct data format (as matrix)
}
\description{
The function checks if the input data are given in the correct form of a matrix of dimension \code{n x 4} containing \code{n} trapezoidal fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0} each. The following conditions have to be fulfilled: (1) the number of columns of the matrix must be 4 (the four values characterizing each trapezoidal fuzzy number), (2) the four values of each trapezoidal number have to be non-decreasing.  This function is used internally in almost all the other functions to do a preliminary checking if the input data are in the correct form.
}
\usage{
checkingTra(F)
}
\arguments{
  \item{F}{
can be any matrix.
}
}
\details{
See examples
}
\value{
The function returns the value 1 if the input fulfills all conditions, if not, the value 0 is returned.
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
\code{\link{checking}}, \code{\link{filterNA}}
}
\examples{
# Example 1:
F=matrix(c(1,2,3,4),nrow=1)
c=checkingTra(F)
c

# Example 2:
F=matrix(c(1,2,3,4),nrow=2)
c=checkingTra(F)
c

# Example 3:
F=matrix(c(1,2,1,4),nrow=1)
c=checkingTra(F)
c
}
\keyword{attribute}
