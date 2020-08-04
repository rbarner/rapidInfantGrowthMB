\name{plot_taxa_growthMeasures_association}
\alias{plot_taxa_growthMeasures_association}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales",taxaOfInterestTitle = "Logged Abundance of Pseudomonadales", measureOfInterest = "inf_weight_kg_12m", measureOfInterestTitle = "12-Month Weight (kg)")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{taxaOfInterest}{
  Name of taxa that is the independent variable. Default is "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales".
  Other options are strictly:
  "k__Bacteria| p__Firmicutes| c__Bacilli| o__Lactobacillales| f__Enterococcaceae",
  "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Lachnospiraceae| g__Blautia",
  "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Ruminococcaceae| g__Unclassified",
  "k__Bacteria| p__Proteobacteria| c__Deltaproteobacteria| o__Desulfovibrionales| f__Desulfovibrionaceae",
  "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Enterobacteriales| f__Enterobacteriaceae| g__Enterobacter",
  "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Enterobacteriales| f__Enterobacteriaceae| g__Klebsiella",
  "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Enterobacteriales| f__Enterobacteriaceae| g__Salmonella",
  "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales"
}
  \item{taxaOfInterestTitle}{
  Name of x label that matches the independent variable. Default is "Logged Abundance of Pseudomonadales".
  Other suggested options are:
  "Logged Abundance of Enterococcaceae",
  "Logged Abundance of Blautia",
  "Logged Abundance of unclassified Ruminococcaceae",
  "Logged Abundance of Desulfovibrionaceae",
  "Logged Abundance of Enterobacter",
  "Logged Abundance of Klebsiella",
  "Logged Abundance of Salmonella",
  "Logged Abundance of Pseudomonadales"
}
  \item{measureOfInterest}{
  Name of growth measure that is the dependent variable of interest. Default is "inf_weight_kg_12m".
  Other options are strictly:
  "diffGrowth_zwei","diffGrowth_zbmi","diffGrowth_zwfl",
  "zwei_12m","zbmi_12m","zwfl_12m","inf_length_cm_12m",
  "skinf_midthigh_mm_12m","skinf_tricep_mm_12m","skinf_supra_mm_12m","skinf_subscap_mm_12m"
}
  \item{measureOfInterestTitle}{
  Name of y label that matches the dependent variable of interest. Default is "12-Month Weight (kg)".
  Other suggested options are:
  "Difference in weight-for-age z-score","Difference in BMI-for-age z-score","Difference in weight-for-length z-score"",
  "Weight-for-age z-score at 12m","BMI z-score at 12m","Weight-for-length z-score at 12m","Length (cm) at 12m",
  "Midthigh skinfold (mm) at 12m","Tricep skinfold (mm) at 12m","Suprailaic skinfold (mm) at 12m","Subscapular skinfold (mm) at 12m"
}


}
\details{

}
\value{

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
plot_taxa_growthMeasures_association()

plot_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales",taxaOfInterestTitle = "Logged Abundance of Pseudomonadales", measureOfInterest = "inf_weight_kg_12m", measureOfInterestTitle = "12-Month Weight (kg)")
}

\keyword{ ~aplot }

