\name{HyperI}
\alias{HyperI}
\title{
Hyperbolic inequality index of a trapezoidal positive fuzzy sample
}
\description{
This function calculates the hyperbolic inequality index for a sample of trapezoidal positive fuzzy numbers contained in a matrix \code{F}. The function first checks if the input matrix \code{F} is given in the correct form (tested by \code{checkingTra}). 
}
\usage{
HyperI(F, c = 0)
}
\arguments{
  \item{F}{
matrix of dimension \code{n x 4} containing \code{n} trapezoidal positive fuzzy numbers characterized by their four values \code{inf0,inf1,sup1,sup0}. The function implicitly checks if the matrix is in the correct form (tested by \code{checkingTra}).
}
  \item{c}{
number in [0,0.5]. The c*100\% trimmed mean will be used in the calculation of the hyperbolic inequality index. 
}
}
\details{
See examples
}
\value{
The function returns the hyperbolic inequality index, which is a real number.
}
\references{
 [1] Lubiano, M.A.; Gil, M.A.: \emph{f-Inequality indices for fuzzy random variables}, in Statistical Modeling, Analysis and Management of Fuzzy Data (Bertoluzza, C., Gil, M.A., Ralescu, D.A., Eds.), Physica-Verlag, pp. 43-63 (2002)
 
 [2] De la Rosa de Saa, S.; Gil, M.A.; Gonzalez-Rodriguez, G.; Lopez, M.T.; Lubiano M.A.: \emph{Fuzzy rating scale-based questionnaires and their statistical analysis}, IEEE Trans. Fuzzy Syst. 23(1), pp. 111-126 (2015)
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
\code{\link{checkingTra}}
}
\examples{
# Example 1:
F=SimulFRSTra(100,6,0.05,0.35,0.6,2,1)
HyperI(F)

# Example 2:
F=SimulCASE2(10)
HyperI(F,0.5)
}
\keyword{univar}
