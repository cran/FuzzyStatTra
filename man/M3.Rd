\name{M3}
\alias{M3}
\docType{data}
\title{
M3 dataset
}
\description{
M3 is a matrix of dimension 69 x 4 containing 69 trapezoidal fuzzy rating responses, each of which is characterized by its four values \code{inf0,inf1,sup1,sup0}. The data correspond to the well-known questionnaire TIMSS-PIRLS2011. This questionnaire was adapted to allow a double-type response, namely, the original Likert and a fuzzy rating scale-based (to simplify, trapezoidal). The questionnaire was conducted on 69 fourth grade students from Colegio San Ignacio (Oviedo-Asturias, Spain). Trapezoidal fuzzy rating responses to the Question M3 "Mathematics is harder for me than any other subject" are collected in this dataset.
}
\usage{data("M3")}
\format{
A matrix of dimension 69 x 4 containing 69 trapezoidal fuzzy rating responses, each of which is characterized by its four values \code{inf0,inf1,sup1,sup0}.
}
\details{
See examples
}
\source{
The complete dataset can be found in http://bellman.ciencias.uniovi.es/SMIRE/FuzzyRatingScaleQuestionnaire-SanIgnacio.html
}
\references{
 [1] Gil, M.A.; Lubiano, M.A.; De la Rosa de Saa, S.; Sinova, B.: \emph{Analyzing data from a fuzzy rating scale-based questionnaire. A case study}, Psicothema 27(2), pp. 182-191 (2015)
 
 [2] Lubiano, M.A.; De la Rosa de Saa, S.; Montenegro, M.; Sinova, B.; Gil, M.A.: \emph{Descriptive analysis of responses to items in questionnaires. Why not a fuzzy rating scale?}, Information Sciences 360, pp. 131-148 (2016)
 
 [3] Lubiano, M.A.; Montenegro, M.; Sinova, B.; De la Rosa de Saa, S.; Gil, M.A.: \emph{Hypothesis testing for means in connection with fuzzy rating scale-based data: algorithms and applications}, European Journal of Operational Research 251, pp. 918-929 (2016)
}
\examples{
data(M3)
filterNA(M3)
F=filterNA(M3)[[1]]
Median1norm(F)
}
\keyword{datasets}
\keyword{survey}
