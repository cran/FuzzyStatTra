\name{filterNA}
\alias{filterNA}
\title{
Deleting missing values
}
\description{
Given any matrix, this function deletes those rows with missing values.
}
\usage{
filterNA(F)
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
The function returns a list with two components: the first one is a matrix identical to the input matrix F but without the rows containing missing values, and the second component is the number of rows of the input matrix without missing values.
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}

\seealso{
\code{\link{checkingTra}}, \code{\link{checking}}
}
\examples{
# Example 1:
F=matrix(c(1,2,3,NA,5,4,7,2),nrow=2)
filterNA(F)

# Example 2:
F=matrix(c(1,2,3,NA,5,4,7,2,1,2,3,4),nrow=3)
filterNA(F)

# Example 3:
data(M2)
filterNA(M2)
}
\keyword{NA}