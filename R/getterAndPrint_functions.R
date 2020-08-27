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
  write.table(thisTable,filepath,quote=FALSE,sep="\t",append=FALSE,col.names = TRUE,row.names=FALSE)
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
