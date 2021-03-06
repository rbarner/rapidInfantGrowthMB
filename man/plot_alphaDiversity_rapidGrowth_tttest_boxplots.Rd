\name{plot_alphaDiversity_rapidGrowth_ttest_boxplots}
\alias{plot_alphaDiversity_rapidGrowth_ttest_boxplots}

\title{
Create boxplots of diversity indices by rapid growth status.
}
\description{
Create boxplots of diversity indices by rapid growth status. The title of the box plot will include the t-statistic and a p-value generated by a t-test to determine differences in diversity indices by rapid growth status.
}
\usage{
plot_alphaDiversity_rapidGrowth_ttest_boxplots(variable = "shannonDiversity",variableName = "Shannon Diversity")
}

\arguments{
  \item{variable}{
  Diversity index to be plotted. Default is "shannonDiversity". Other options are strictly: "richness","evenness" and "numSeqPerSample"
}
  \item{variableName}{
  Name of diversity index to be plotted. Default is "Shannon Diversity". Other options suggested options are: "Richness","Evenness" and "Number Sequences /Sample". These names should be related to the "variable" being plotted.
}
}
\details{

}
\value{
None
}
\references{

}
\author{
Roshonda  Barner Jones
}
\note{

}

\seealso{

}
\examples{
 plot_alphaDiversity_rapidGrowth_ttest_boxplots()
 plot_alphaDiversity_rapidGrowth_ttest_boxplots(variable = "shannonDiversity",variableName = "Shannon Diversity")
 plot_alphaDiversity_rapidGrowth_ttest_boxplots(variable = "srichness",variableName = "Richness")
}

\keyword{ ~aplot }

