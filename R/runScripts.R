################# Create Tables ########################################
print_summary_1MonthCharacteristics_byRapidGrowth(filepath = "../testPlotData/table1.txt")
print_OR_RapidGrowth_byAlphaDiversity(filepath = "../testPlotData/table2.txt")
print_associations_alphaDiversity_growthMeasures(filepath = "../testPlotData/table3.txt")
print_OR_RapidGrowth_bylefseTaxa(filepath = "../testPlotData/table4.txt")

print_associations_lefseTaxa_growthMeasures(filepath = "../testPlotData/supplementalTable1.txt")

################# Create Plot line data ########################################
print_intercept_slope_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales", measureOfInterest = "skinf_tricep_mm_12m",filepath="../testPlotData/supplementalFigure4_pseudomonadales_tricep_lineData.txt")
print_intercept_slope_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales", measureOfInterest = "skinf_midthigh_mm_12m",filepath="../testPlotData/supplementalFigure4_pseudomonadales_midthigh_lineData.txt")
print_intercept_slope_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Ruminococcaceae| g__Unclassified", measureOfInterest = "skinf_midthigh_mm_12m",filepath="../testPlotData/supplementalFigure4_ruminococcaceae_midthigh_lineData.txt")
print_intercept_slope_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Firmicutes| c__Bacilli| o__Lactobacillales| f__Enterococcaceae", measureOfInterest = "skinf_midthigh_mm_12m",filepath="../testPlotData/supplementalFigure4_enterococcaceae_midthigh_lineData.txt")
print_intercept_slope_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Deltaproteobacteria| o__Desulfovibrionales| f__Desulfovibrionaceae", measureOfInterest = "skinf_supra_mm_12m",filepath="../testPlotData/supplementalFigure4_desulfovibrionaceae_supra_lineData.txt")
print_intercept_slope_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Ruminococcaceae| g__Unclassified", measureOfInterest = "skinf_supra_mm_12m",filepath="../testPlotData/supplementalFigure4_ruminococcaceae_supra_lineData.txt")
print_intercept_slope_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Firmicutes| c__Bacilli| o__Lactobacillales| f__Enterococcaceae", measureOfInterest = "skinf_supra_mm_12m",filepath="../testPlotData/supplementalFigure4_enterococcaceae_supra_lineData.txt")

print_intercept_slope_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Deltaproteobacteria| o__Desulfovibrionales| f__Desulfovibrionaceae", measureOfInterest = "skinf_subscap_mm_12m",filepath="../testPlotData/supplementalFigure3_desulfovibrionaceae_subscap_lineData.txt")
print_intercept_slope_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Deltaproteobacteria| o__Desulfovibrionales| f__Desulfovibrionaceae", measureOfInterest = "skinf_tricep_mm_12m",filepath="../testPlotData/supplementalFigure3_desulfovibrionaceae_tricep_lineData.txt")
print_intercept_slope_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Firmicutes| c__Bacilli| o__Lactobacillales| f__Enterococcaceae", measureOfInterest = "skinf_tricep_mm_12m",filepath="../testPlotData/supplementalFigure3_enterococcaceae_tricep_lineData.txt")

print_intercept_slope_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales", measureOfInterest = "inf_weight_kg_12m",filepath="../testPlotData/figure3_pseudomonadales_weight_lineData.txt")
print_intercept_slope_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Ruminococcaceae| g__Unclassified", measureOfInterest = "inf_weight_kg_12m",filepath="../testPlotData/figure3_ruminococcaceae_weight_lineData.txt")



################# Create Plot point data ########################################
print_plotData_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales", measureOfInterest = "skinf_tricep_mm_12m",filepath="../testPlotData/supplementalFigure4_pseudomonadales_tricep_dataPoints.txt")

print_plotData_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales", measureOfInterest = "skinf_midthigh_mm_12m",filepath="../testPlotData/supplementalFigure4_pseudomonadales_midthigh_dataPoints.txt")
print_plotData_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Ruminococcaceae| g__Unclassified", measureOfInterest = "skinf_midthigh_mm_12m",filepath="../testPlotData/supplementalFigure4_ruminococcaceae_midthigh_dataPoints.txt")
print_plotData_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Firmicutes| c__Bacilli| o__Lactobacillales| f__Enterococcaceae", measureOfInterest = "skinf_midthigh_mm_12m",filepath="../testPlotData/supplementalFigure4_enterococcaceae_midthigh_dataPoints.txt")
print_plotData_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Deltaproteobacteria| o__Desulfovibrionales| f__Desulfovibrionaceae", measureOfInterest = "skinf_supra_mm_12m",filepath="../testPlotData/supplementalFigure4_desulfovibrionaceae_supra_dataPoints.txt")
print_plotData_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Ruminococcaceae| g__Unclassified", measureOfInterest = "skinf_supra_mm_12m",filepath="../testPlotData/supplementalFigure4_ruminococcaceae_supra_dataPoints.txt")
print_plotData_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Firmicutes| c__Bacilli| o__Lactobacillales| f__Enterococcaceae", measureOfInterest = "skinf_supra_mm_12m",filepath="../testPlotData/supplementalFigure4_enterococcaceae_supra_dataPoints.txt")

print_plotData_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Deltaproteobacteria| o__Desulfovibrionales| f__Desulfovibrionaceae", measureOfInterest = "skinf_subscap_mm_12m",filepath="../testPlotData/supplementalFigure3_desulfovibrionaceae_subscap_dataPoints.txt")
print_plotData_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Deltaproteobacteria| o__Desulfovibrionales| f__Desulfovibrionaceae", measureOfInterest = "skinf_tricep_mm_12m",filepath="../testPlotData/supplementalFigure3_desulfovibrionaceae_tricep_dataPoints.txt")
print_plotData_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Firmicutes| c__Bacilli| o__Lactobacillales| f__Enterococcaceae", measureOfInterest = "skinf_tricep_mm_12m",filepath="../testPlotData/supplementalFigure3_enterococcaceae_tricep_dataPoints.txt")

print_plotData_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales", measureOfInterest = "inf_weight_kg_12m",filepath="../testPlotData/figure3_pseudomonadales_weight_dataPoints.txt")
print_plotData_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Ruminococcaceae| g__Unclassified", measureOfInterest = "inf_weight_kg_12m",filepath="../testPlotData/figure3_ruminococcaceae_weight_dataPoints.txt")
