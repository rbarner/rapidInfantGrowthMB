\name{summarize_1MonthCharacteristics_byRapidGrowth}
\alias{summarize_1MonthCharacteristics_byRapidGrowth}

\title{
Summarize baseline characteristics of participants by rapid growth status
}
\description{
Baseline characteristics of mother and infant are averaged by rapid growth status. Rapid growth status was defined as a difference of sex adjusted weight-for-age from birth - 12 month that is greater than 0.67.
}
\usage{
summarize_1MonthCharacteristics_byRapidGrowth()
}

\arguments{
None
}
\details{
}
\value{
The output will be a data frame with the following columns:
\item{allOutcomes }{List of baseline characteristics}
\item{overallColumn}{Means and standard deviations across all participants}
\item{rapidGrowthColumn}{Means and standard deviations across infants who experience rapid growth}
\item{nonRapidGrowthColumn}{Means and standard deviations across infants who do not experience rapid growth}
\item{crudeORList}{Odds-ratio of rapid growth by given characteristic determined by logistic regression}
\item{pValORList}{P-value of odds-ratio of rapid growth by given characteristic}
}
\references{

}
\author{
Roshonda Barner Jones
}
\note{
}


\seealso{
}
\examples{
summarize_1MonthCharacteristics_byRapidGrowth()
}

\keyword{ ~manip }

