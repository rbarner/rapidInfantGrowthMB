# rapidInfantGrowthMB
Association between infant microbiome at 1 month and rapid growth status from birth to 12 months


## Table of contents
* [Overview](##Overview)
* [Contents](#Contents)
* [Requirements](#Requirements)
* [Installation](#Installation)
* [Demo](#Demo)
* [License](#License)


## Overview
Code used to create statistical tables and plots used in the manuscript "The Early Gut Microbiota is Associated with Infant Growth in Hispanics from Southern California". The aim of the research is to determine whether the gut microbiota at 1 month of age explains rapid growth status at 12 months of age.

## Repo Contents
* [R](#R)
* [man](#man)
* [data](#data)

## System Requirements
This program was tested and developed on the macOS Catalina 10.15 system. The following packages in R are necessary for this package:
* R (>= 3.6.2) 
* ggplot2
* withr 
* stringr
* dplyr

RStudio Version 1.2.5019 is optional.

## Installation

To install type the following lines in R:

```
install.packages("devtools")
library("devtools")
```

You will then need to download the R package "rapidInfantGrowthMB" using the following lines:

```
install_github("rbarner/rapidInfantGrowthMB")
library("rapidInfantGrowthMB")
```

## Demo 
(The runtime of each of these functions should be <30 seconds each.)

1. First we can summarize the baseline characteristics of the participants in our study by rapid growth status (defined as a difference in weight-for-age z-score > 0.67)
```
summarize_1MonthCharacteristics_byRapidGrowth()
```
This command will give us the means and standard deviations of the baseline characteristics of all of the participants, those who experience rapid growth and those who do not experience rapid growth. We also show the crude odds ratio of experiencing rapid growth by given characteristic.

We can also show the distribution of the taxa at the phylum level by rapid growth status:
```
plot_phylum_relativeAbundances_barplot()
```

And we can show a PCA plot of the gut microbes of the infants at 1 month of age:
```
plot_mds_coloredByRapidGrowth()
```

2. Next, we determine the whether there are differences in alpha diversity by rapid growth status. We will also show p-values from a logistic regression testing whether alpha diversity explains rapid growth status. (These models adjust for birthweight and birthlength). 
```
compute_OR_RapidGrowth_byAlphaDiversity()

```
This will give us the t-statistic and p-values from a t-test testing for differences in alpha diversities by rapid growth status. We will also get an odds ratio and a p-value from the logistic regression.

We can also plot individual alpha diversity metrics. For example:

```
plot_alphaDiversity_rapidGrowth_ttest_boxplots(variable = "shannonDiversity",variableName = "Shannon Diversity")
```

We will also determine whether alpha diversity of gut microbiome at 1 month of age explains other measures of growth at 12 months using linear regression models. (These models adjust for birthweight and birthlength).
```
compute_associations_alphaDiversity_growthMeasures()
```

3. Outside of this R analysis pipeline, we have determined taxa that are significantly different by rapid growth status using LEfSe analysis (https://galaxyproject.org/learn/visualization/custom/lefse/). Now we will first look at the taxa that LEfSe analysis determined to be significantly different by rapid growth status.
```
plot_lefse_LDA_graph()
```

Using these only these taxa, we will determine the whether there are differences in taxa by rapid growth status. We will also show odd ratios and p-values from a logistic regression testing whether these significant taxa explain rapid growth status. (These models adjust for birthweight and birthlength). 

```
compute_OR_RapidGrowth_bylefseTaxa()
```

We will also determine whether taxa considered significantly different by rapid growth status explains other measures of growth at 12 months using linear regression models. (These models adjust for birthweight and birthlength).
```
compute_associations_lefseTaxa_growthMeasures()
```

We can also plot individualy associations between taxa of interest and growth measure of interest using:
```
plot_taxa_growthMeasures_association()
```

We can specify the taxa of interest and growth measure of interest by calling:
```
plot_taxa_growthMeasures_association(taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales",taxaOfInterestTitle = "Logged Abundance of Pseudomonadales", measureOfInterest = "inf_weight_kg_12m", measureOfInterestTitle = "12-Month Weight (kg)")
```

## License
This package is licensed under GNU General Public License (GPL-3)
