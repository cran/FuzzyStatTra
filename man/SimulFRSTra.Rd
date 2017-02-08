\name{SimulFRSTra}
\alias{SimulFRSTra}
\title{
Simulation of trapezoidal fuzzy rating responses to a questionnaire
}
\description{
This function generates \code{n} trapezoidal responses based on the fuzzy rating scale. They are simulated mimicking the human behavior, considering for it a finite mixture of three different procedures (for a detailed explanation of the simulation see the paper [1] below), and generated in the interval [1,k], being \code{k} the number of Likert responses of the supposed questionnaire.
}
\usage{
SimulFRSTra(n, k, w1, w2, w3, p, q)
}
\arguments{
  \item{n}{
positive integer. It is the number of trapezoidal fuzzy numbers to be generated.  
}
  \item{k}{
positive integer and >1. It's the number of Likert responses of the supposed questionnaire. The trapezoidal fuzzy responses will be generated in the interval [1,k].
}
  \item{w1}{
number in [0,1]. It should be fulfilled that \code{w1+w2+w3=1}. 
}
  \item{w2}{
number in [0,1]. It should be fulfilled that \code{w1+w2+w3=1}. 
}
  \item{w3}{
number in [0,1]. It should be fulfilled that \code{w1+w2+w3=1}. 
}
  \item{p}{
number >0. It is the first parameter of the beta distribution.
}
  \item{q}{
number >0. It is the second parameter of the beta distribution.
}
}
\details{
See examples
}
\value{
This function returns \code{n} trapezoidal fuzzy rating responses contained in a matrix of dimension \code{n x 4}, with values in the interval [1,k]. Each trapezoidal fuzzy rating response is characterized by its four values \code{inf0,inf1,sup1,sup0}.
}
\references{
[1] De la Rosa de Saa, S.; Gil, M.A.; Gonzalez-Rodriguez, G.; Lopez, M.T.; Lubiano M.A.: \emph{Fuzzy rating scale-based questionnaires and their statistical analysis}, IEEE Trans. Fuzzy Syst. 23(1), pp. 111-126 (2015) 
}
\author{
Asun Lubiano <lubiano@uniovi.es>, Sara de la Rosa de Saa  <rosasara@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
\code{\link{SimulCASE1}}, \code{\link{SimulCASE2}}, \code{\link{SimulCASE3}}, \code{\link{SimulCASE4}}
}
\examples{
# Example 1:
SimulFRSTra(100,6,0.05,0.35,0.6,2,1)
}
\keyword{datagen}
