print_summary_1MonthCharacteristics_byRapidGrowth <- function(filepath)
{
  summaryData <- summarize_1MonthCharacteristics_byRapidGrowth()
  write.table(summaryData,filepath,quote=FALSE,sep="\t",append=FALSE,col.names = TRUE,row.names=FALSE)
}

print_OR_RapidGrowth_byAlphaDiversity <- function(filepath)
{
  thisTable <- compute_OR_RapidGrowth_byAlphaDiversity()
  write.table(thisTable,filepath,quote=FALSE,sep="\t",append=FALSE,col.names = TRUE,row.names=FALSE)
}

print_associations_alphaDiversity_growthMeasures <- function(filepath)
{
  thisTable <- compute_associations_alphaDiversity_growthMeasures()
  write.table(t(thisTable),filepath,quote=FALSE,sep="\t",append=FALSE,col.names = FALSE,row.names=TRUE)
}

print_OR_RapidGrowth_bylefseTaxa <- function(filepath)
{
  thisTable <- compute_OR_RapidGrowth_bylefseTaxa()
  write.table(thisTable,filepath,quote=FALSE,sep="\t",append=FALSE,col.names = TRUE,row.names=FALSE)
}

print_associations_lefseTaxa_growthMeasures <- function(filepath)
{
  thisTable <- compute_associations_lefseTaxa_growthMeasures()
  write.table(thisTable,filepath,quote=FALSE,sep="\t",append=FALSE,col.names = TRUE,row.names=FALSE)
}

print_phylum_relativeAbundances_barplot <- function(filepath)
{
  pdf(file = filepath)
  plot_phylum_relativeAbundances_barplot()
  graphics.off()
}

print_mds_coloredByRapidGrowth_plot  <- function(filepath)
{
  pdf(file = filepath)
  plot_mds_coloredByRapidGrowth()
  graphics.off()
}

print_mds_coloredByRapidGrowth_plot  <- function(filepath)
{
  pdf(file = filepath)
  plot_mds_coloredByRapidGrowth()
  graphics.off()
}

print_lefse_LDA_graph <- function(filepath)
{
  pdf(file = filepath)
  plot_lefse_LDA_graph()
  graphics.off()
}

print_intercept_slope_taxa_growthMeasures_association <- function(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales", measureOfInterest = "inf_weight_kg_12m",filepath)
{
  load("data/metaData_generalCharacteristics.RData") #loads in "dataSet" which is a data frame containing metadata
  load("data/lefse_allData_rapidGrowth_updated_notLogged.RData") #loads in "myT" which is a data frame with lefse_formatted taxa counts per sample

  taxaMeta <- merge(metaData,lefseTaxaCounts,by.x="dyad_id",by.y="row.names")

  thisDataInstance <- taxaMeta[,names(taxaMeta) %in% c(taxaOfInterest,"k__Bacteria","baby_birthlength_cm","baby_birthweight_kg","gestational_age_category",measureOfInterest,"dyad_id")]
  thisDataInstance$dyad_id <- factor(thisDataInstance$dyad_id)
  thisDataInstance$gestational_age_category <- factor(thisDataInstance$gestational_age_category,levels = c("On Time","Late","Early"))

  names(thisDataInstance)[ names(thisDataInstance) %in% measureOfInterest] <- "thisVariable"
  names(thisDataInstance)[ names(thisDataInstance) %in% taxaOfInterest] <- "thisTaxa"

  thisDataInstance$taxaRelAbundance <- as.numeric(as.character(thisDataInstance$thisTaxa))/as.numeric(as.character(thisDataInstance$k__Bacteria))
  thisDataInstance$logTaxaRelAbundance <- log10(((as.numeric(as.character(thisDataInstance$thisTaxa))/as.numeric(as.character(thisDataInstance$k__Bacteria)))*mean(as.numeric(as.character(thisDataInstance$k__Bacteria))))+1)

  modelForm=as.formula(paste("thisVariable","~logTaxaRelAbundance+baby_birthlength_cm+baby_birthweight_kg+gestational_age_category"  ));
  modelInfo <- lm(modelForm, data = thisDataInstance)
  coefs <- coef(modelInfo)

  lineData <- data.frame(slope = coefs["logTaxaRelAbundance"],intercept = coefs["(Intercept)"]+mean(thisDataInstance$baby_birthlength_cm,na.rm=TRUE)*coefs["baby_birthlength_cm"] +  mean(thisDataInstance$baby_birthweight_kg)*coefs["baby_birthweight_kg"])
  write.table(lineData,filepath,quote=FALSE, sep="\t",append=FALSE, row.names=FALSE, col.names=TRUE)

}


