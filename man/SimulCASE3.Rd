\name{SimulCASE3}
\alias{SimulCASE3}
\title{
Simulation of trapezoidal fuzzy numbers CASE 3
}
\description{
This function generates \code{n} trapezoidal fuzzy numbers from an asymmetric distribution and with independent components (for a detailed explanation of the simulation see the paper [1] below, namely, the Case 3 for noncontaminated samples).
}
\usage{
SimulCASE3(n)
}
\arguments{
  \item{n}{
positive integer. It is the number of trapezoidal fuzzy numbers to be generated.
}
}
\details{
See examples
}
\value{
This function returns \code{n} trapezoidal fuzzy numbers contained in a matrix of dimension \code{n x 4}. Each trapezoidal fuzzy number is characterized by its four values \code{inf0,inf1,sup1,sup0}.
}
\references{
[1] Sinova, B.; Gil, M.A.; Van Aelst, S.: \emph{M-estimates of location for the robust central tendency of fuzzy data}, IEEE Transactions on Fuzzy Systems 24(4), pp. 945-956 (2016)
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
\code{\link{SimulCASE1}}, \code{\link{SimulCASE2}}, \code{\link{SimulCASE4}}, \code{\link{SimulFRSTra}}
}
\examples{
# Example 1:
SimulCASE3(10)
}
\keyword{datagen}