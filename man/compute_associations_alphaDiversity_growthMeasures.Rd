\name{compute_associations_alphaDiversity_growthMeasures}
\alias{compute_associations_alphaDiversity_growthMeasures}

\title{
Compute associations between 12-month growth measures and diversity indices that were determined to be significantly different between rapid growth status.

}
\description{
This function will output the results of multiple linear regression models determining association 12-month growth measures and diversity indices. The models adjust for birthweight and birthlength.
}
\usage{
compute_associations_alphaDiversity_growthMeasures()
}

\arguments{
Non
}
\details{

}
\value{
The output will be a data frame with the following columns:
\item{DiversityMeasure }{Diversity metric}
\item{diffGrowth_zwei_beta_(CI)}{Beta for association between diversity metric at 1-M and change in weight for age z-score from birth to 12 months }
\item{diffGrowth_zwei_pval}{P-value for association between diversity metric at 1-M and change in weight for age z-score from birth to 12 months }
\item{diffGrowth_zwei_adj_pval}{FDR adjusted p-value for association between diversity metric at 1-M and change in weight for age z-score from birth to 12 months }
\item{diffGrowth_zbmi_beta_(CI)}{FDR adjusted p-value for association between diversity metric at 1-M and change in BMI z-score from birth to 12 months }
\item{diffGrowth_zbmi_pval}{P-value for association between diversity metric at 1-M and change in BMI z-score from birth to 12 months }
\item{diffGrowth_zbmi_adj_pval}{FDR adjusted p-value for association between diversity metric at 1-M and change in BMI z-score from birth to 12 months }
\item{diffGrowth_zwfl_beta_(CI)}{FDR adjusted p-value for association between diversity metric at 1-M and change in weight-for-length z-score from birth to 12 months }
\item{diffGrowth_zwfl_pval}{P-value for association between diversity metric at 1-M and change in weight-for-length z-score from birth to 12 months }
\item{diffGrowth_zwfl_adj_pval}{FDR adjusted p-value for association between diversity metric at 1-M and change in weight-for-length z-score from birth to 12 months }
\item{zwei_12m_beta_(CI)}{FDR adjusted p-value for association between diversity metric at 1-M and weight-for-age z-score at 12 months }
\item{zwei_12m_pval}{P-value for association between diversity metric at 1-M and weight-for-age z-score at 12 months }
\item{zwei_12m_adj_pval}{FDR adjusted p-value for association between diversity metric at 1-M and weight-for-age z-score at 12 months }
\item{zbmi_12m_beta_(CI)}{FDR adjusted p-value for association between diversity metric at 1-M and weight-for-age z-score at 12 months }
\item{zbmi_12m_pval}{P-value for association between diversity metric at 1-M and weight-for-age z-score at 12 months }
\item{zbmi_12m_adj_pval}{FDR adjusted p-value for association between diversity metric at 1-M and weight-for-age z-score at 12 months }
\item{zwfl_12m_beta_(CI)}{FDR adjusted p-value for association between diversity metric at 1-M and weight-for-length z-score at 12 months }
\item{zwfl_12m_pval}{P-value for association between diversity metric at 1-M and weight-for-length z-score at 12 months }
\item{zwfl_12m_adj_pval}{FDR adjusted p-value for association between diversity metric at 1-M and weight-for-length z-score at 12 months }
\item{inf_weight_kg_12m_beta_(CI)}{FDR adjusted p-value for association between diversity metric at 1-M and weight (kg) at 12 months }
\item{inf_weight_kg_12m_pval}{P-value for association between diversity metric at 1-M and weight (kg) at 12 months }
\item{inf_weight_kg_12m_adj_pval}{FDR adjusted p-value for association between diversity metric at 1-M and weight (kg) at 12 months }
\item{inf_length_cm_12m_beta_(CI)}{FDR adjusted p-value for association between diversity metric at 1-M and length (cm) at 12 months }
\item{inf_length_cm_12m_pval}{P-value for association between diversity metric at 1-M and length (cm) at 12 months }
\item{inf_length_cm_12m_adj_pval}{FDR adjusted p-value for association between diversity metric at 1-M and length (cm) at 12 months }
\item{skinf_midthigh_mm_12m_beta_(CI)}{FDR adjusted p-value for association between diversity metric at 1-M and mid-thigh skinfold (mm) at 12 months }
\item{skinf_midthigh_mm_12m_pval}{P-value for association between diversity metric at 1-M and mid-thigh skinfold (mm) at 12 months }
\item{skinf_midthigh_mm_12m_adj_pval}{FDR adjusted p-value for association between diversity metric at 1-M and mid-thigh skinfold (mm) at 12 months }
\item{skinf_tricep_mm_12m_beta_(CI)}{FDR adjusted p-value for association between diversity metric at 1-M and tricep skinfold (mm) at 12 months }
\item{skinf_tricep_mm_12m_pval}{P-value for association between diversity metric at 1-M and tricep skinfold (mm) at 12 months }
\item{skinf_tricep_mm_12m_adj_pval}{FDR adjusted p-value for association between diversity metric at 1-M and tricep skinfold (mm) at 12 months }
\item{skinf_supra_mm_12m_beta_(CI)}{FDR adjusted p-value for association between diversity metric at 1-M and suprailiac skinfold (mm) at 12 months }
\item{skinf_supra_mm_12m_pval}{P-value for association between diversity metric at 1-M and suprailiac skinfold (mm) at 12 months }
\item{skinf_supra_mm_12m_adj_pval}{FDR adjusted p-value for association between diversity metric at 1-M and suprailiac skinfold (mm) at 12 months }
\item{skinf_subscap_mm_12m_beta_(CI)}{FDR adjusted p-value for association between diversity metric at 1-M and subscapular skinfold (mm) at 12 months }
\item{skinf_subscap_mm_12m_pval}{P-value for association between diversity metric at 1-M and subscapular skinfold (mm) at 12 months }
\item{skinf_subscap_mm_12m_adj_pval}{FDR adjusted p-value for association between diversity metric at 1-M and subscapular skinfold (mm) at 12 months }
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
compute_associations_alphaDiversity_growthMeasures()
}

\keyword{ ~manip }

