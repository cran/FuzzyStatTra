\name{checking}
\alias{checking}
\title{
Checking correct data format (as array)
}
\description{
The function checks if the input data are given in the correct form of an array of dimension \code{nl x 3 x n} containing \code{n} fuzzy numbers characterized by means of \code{nl} \eqn{\alpha}-levels each. The following conditions have to be fulfilled: (1) the number of columns of the array must be 3 (the first column will be the \eqn{\alpha}-levels, the second one their infimum values and the third one their supremum values), (2) all the fuzzy numbers must have the same column of \eqn{\alpha}-levels, (3) the minimum \eqn{\alpha}-level should be 0 y the maximum 1, (4)  the \eqn{\alpha}-levels have to increase from 0 to 1, (5) the infimum values have to be non-decreasing, (6) the supremum values have to be non-creasing, (7) the infimum value has to be smaller or equal than the supremum value for each \eqn{\alpha}-level. This function is used internally in some of the other functions to do a preliminary checking if the input data are in the correct form.
}
\usage{
checking(R)
}
\arguments{
  \item{R}{
can be any array.
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
\code{\link{checkingTra}}, \code{\link{filterNA}}
}
\examples{
# Example 1:
F=SimulCASE1(10)
R=TransfTra(F)
c=checking(R)
c

# Example 2:
R=array(c(1:10),dim=c(2,1,2))
c=checking(R)
c

# Example 3:
R=array(c(1:10),dim=c(2,3,2))
c=checking(R)
c

# Example 4:
R=array(c(1,2,3,4,5,6,1,2,4,5,6,7),dim=c(2,3,2))
c=checking(R)
c

# Example 5:
R=array(c(0,0,1,2,3,4,5,0,1,0,0,1,7,8,9,19,30,3),dim=c(3,3,2))
c=checking(R)
c

# Example 6:
R=array(c(0,0.5,1,2,3,4,5,0,1,0,0.5,1,7,8,7,19,30,3),dim=c(3,3,2))
c=checking(R)
c

# Example 7:
R=array(c(0,0.5,1,2,3,4,5,0,1,0,0.5,1,7,8,9,19,30,3),dim=c(3,3,2))
c=checking(R)
c

# Example 8:
R=array(c(0,0.5,1,2,3,4,6,5,4,0,0.5,1,7,8,9,19,10,2),dim=c(3,3,2))
c=checking(R)
c
}
\keyword{attribute}

