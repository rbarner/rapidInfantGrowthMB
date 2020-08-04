plot_phylum_relativeAbundances_barplot <- function( )
{
  load("data/metaData_generalCharacteristics.RData") #loads in "dataSet" which is a data frame containing metadata
  rapidGrowthAssignments <- dataSet
  mergeData <- rapidGrowthAssignments[,names(rapidGrowthAssignments) %in% c("dyad_id","rapidGrowth")]
  mergeData$dyad_id <- factor(as.character(with_options(c(scipen = 999), str_pad(mergeData$dyad_id, 4, pad = "0"))))
  
  load("data/phylum_counts_infant_baseline.RData") #loads in "myT" which is a data frame containing phylum counts of infants at baseline
  totalReads <- colSums(myT)
  totalReadsFiltered <- totalReads[totalReads>=10]
  myT_RA <- (myT/(rowSums(myT)))
  topHits1<- colnames(myT_RA[,colnames(myT_RA) %in% names(totalReadsFiltered)])
  finalTopHits <- colnames(myT_RA[,(colSums(myT_RA==0)/nrow(myT_RA)) <= 0.9 & colnames(myT_RA) %in% topHits1])
  myT_RA2 <- as.data.frame(myT_RA)
  myT_RA2$dyad_id <- substr(row.names(myT_RA2),10,13)
  
  otherMicrobes <- as.data.frame(rowSums(myT_RA[,!colnames(myT_RA) %in% finalTopHits]))
  names(otherMicrobes)[1] <- "Others"
  myT2 <- myT_RA[,colnames(myT_RA) %in% finalTopHits]
  myT3 <- merge(myT2,otherMicrobes,by="row.names")
  row.names(myT3) <- substr(myT3$Row.names,10,13)
  myT3 <- myT3[,-1]
  
  taxaMeta <- merge(mergeData,myT3,by.x="dyad_id",by.y="row.names")
  taxaMeta2 <-  taxaMeta[order(taxaMeta$rapidGrowth,taxaMeta$` p__Actinobacteria`,taxaMeta$` p__Bacteroidetes`,taxaMeta$` p__Firmicutes`),];
  taxaMeta3 <- taxaMeta2[,!colnames(taxaMeta2) %in% c("dyad_id")]
  
  phylum_colors <- c("#fa69e8",
                     "#7fcc32",
                     "#554fd5",
                     "#da381f",
                     "#e70067",
                     "#01cbdd",
                     "#9c7bab",
                     "#982450",
                     "#ffacb5")
  
  summaries <- taxaMeta3 %>%
    group_by(rapidGrowth) %>%
    summarize_if(is.numeric, mean)
  summariesnames <- summaries$rapidGrowth
  summaries <- summaries[,-1]
  summaries <- as.matrix(summaries)
  row.names(summaries) <- summariesnames
  dat <- melt(summaries,id.vars="rapidGrowth")
  names(dat) <- c("rapidGrowth","variable","value")
  dat$rapidGrowth <- factor(dat$rapidGrowth,levels=c("Rapid Growth","Non-rapid Growth"))
  
  p <- ggplot(dat, aes(x=factor(rapidGrowth), y=value, fill = variable))
  print(p +geom_bar(position="stack", stat="identity", 
                    colour="black",# Use black outlines,
                    size=.5) +      # Thinner lines
          scale_fill_manual(values=phylum_colors) +
          #guides(fill=FALSE)+
          ggtitle("Phylum-level")+
          ylab("Percentage") + xlab("") +
          theme_classic(base_size = 20)+
          theme(axis.line=element_line(size=1),
                axis.ticks.y=element_line(size=1),
                axis.ticks.x=element_blank(),
                axis.text=element_text(face="bold",size=16),
                text=element_text(face="bold",size=20),
                legend.position="left",
                legend.title=element_blank()
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  ) 
}

plot_mds_coloredByRapidGrowth <- function()
{
  load("data/infant_1m_ordination_for_MDS_plots.RData") # load in "ordinationMeta" which has the ordinations from deicode for first 3 axes
  load("data/infant_1m_eigenvalues_for_ordination_for_MDS_plots.RData") # load in "eigen1m" which has the eigenvalues for the deicode ordinations
  
  comp1<-as.character(paste("Axis 1,"," ",(round(eigen1m$ProportionExplained[1],2))*100,"% variance explained",sep=""));
  comp2<-as.character(paste("Axis 2,"," ",(round(eigen1m$ProportionExplained[2],2))*100,"% variance explained",sep=""));

  p <- ggplot(ordinationMeta )
  print(p + geom_point(aes(Axis1,Axis2,colour = rapidGrowth,shape=rapidGrowth),size = 10) +
          scale_color_manual(values=c("red3","mediumblue"))+  
          stat_ellipse(aes(Axis1,Axis2,colour = rapidGrowth))+
          #guides(shape=FALSE)+
          xlab(comp1) + ylab(comp2) +
          theme_classic(base_size = 30)+
          theme(text=element_text(face="bold",size=20),
                title=element_text(face="bold",size=16),
                axis.line=element_line(size=1),
                axis.ticks=element_line(size=1),
                axis.text=element_text(face="bold",size=16),
                axis.title=element_text(face="bold",size=16),
                plot.margin=unit(c(9.0, 5.5, 6.5, 5.5), "points"),
                legend.position = "bottom"
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2),
                legend.title = element_blank(),
                legend.text=element_text(size=20)
          )
  )
}

plot_alphaDiversity_rapidGrowth_ttest_boxplots <- function(variable = "shannonDiversity",variableName = "Shannon Diversity")
{
  load("data/metaData_generalCharacteristics.RData") #loads in "dataSet" which is a data frame containing metadata
  load("data/alphaDiversity_infants_1month.RData") #loads in "myT5" which is a data frame with alpha diversity measures per sample
  taxaMeta <- merge(dataSet,myT5,by="dyad_id")
  
  modelForm=as.formula(paste("thisDiversityIndex","~rapidGrowth"));
  
  thisDataInstance <- data.frame(dyad_id=taxaMeta$dyad_id,
                                 thisDiversityIndex=as.numeric(as.character(taxaMeta[,names(taxaMeta) %in% variable])),
                                 rapidGrowth =taxaMeta$rapidGrowth)
  
  modelInfo <- t.test(modelForm, data = thisDataInstance)
  thisTstatistic <- format(modelInfo$statistic,digits = 3)
  pVal <- format.pval(modelInfo$p.value,digits=3);
  
  title <- paste("t stat =",thisTstatistic,", p-val= ",pVal,sep = "")
  thisDataInstance$rapidGrowth = factor(thisDataInstance$rapidGrowth,levels = c("Rapid Growth","Non-rapid Growth"))
  p <- ggplot(thisDataInstance,aes(rapidGrowth,thisDiversityIndex))
  print(p + geom_boxplot(outlier.size = -1) +
          geom_jitter(colour="grey46",size=5, position=position_jitter(0.2),aes(shape=rapidGrowth)) +
          guides(shape=FALSE)+
          ggtitle(title)+
          ylab(variable) +
          xlab("")+
          theme_classic(base_size = 20)+
          theme(axis.line=element_line(size=1),
                axis.ticks.y=element_line(size=1),
                axis.ticks.x=element_blank(),
                axis.text=element_text(face="bold",size=20),
                text=element_text(face="bold",size=16),
                legend.position="bottom",
                legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
}


plot_lefse_LDA_graph <- function()
{
  load("data/significantLEFSEtaxa.RData") # load in results of lefse analysis for significant taxa as "lefseData"
  taxaShort <- strsplit(as.character(lefseData$Taxa),"\\.")
  taxaToBeExamined <- lefseData$Taxa
  taxaNames <- character(0)
  for(i in 1:length(taxaShort))
  {
    thisTaxa <- taxaShort[[i]][length(taxaShort[[i]])]
    taxaNames <- c(taxaNames,thisTaxa)
  }
  
  lefseData$shortNames <- taxaNames
  lefseData <- lefseData[order(lefseData$Value),]
  lefseData$Group <- factor(ifelse(lefseData$Group %in% "1_Rapid","Rapid Growth","Non-Rapid Growth"),levels=c("Rapid Growth","Non-Rapid Growth"))
  
  p <- ggplot(lefseData, aes(y=Value, x=shortNames, fill = Group))
  print(p +geom_bar(position="dodge",stat="identity") +      # Thinner lines
          ggtitle("")+
          coord_flip() +
          scale_fill_manual(values=c("red3","mediumblue"))+
          scale_x_discrete(limits= lefseData$shortNames)+
          xlab("") + ylab("LDA SCORE (log 10)") +
          geom_text(aes(x = shortNames,y = -1*(Value/abs(Value)) *0.3, label = shortNames),size=6,hjust="outward",fontface = "bold")+
          theme_classic(base_size = 20)+
          theme(axis.line=element_line(size=1),
                axis.ticks.x=element_line(size=1),
                axis.ticks.y=element_blank(),
                axis.text.x=element_text(face="bold",size=16),
                axis.text.y=element_blank(),
                text=element_text(face="bold",size=20),
                legend.position="bottom",
                legend.title=element_blank()
          )+
          theme(axis.line.x = element_line(color="black", size = 1),
                axis.line.y = element_line(color="black", size = 1)
          )
  ) 
}

plot_taxa_growthMeasures_association <- function(taxaData = lefseTaxa,taxaOfInterest = "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales",taxaOfInterestTitle = "Logged Abundance of Pseudomonadales", measureOfInterest = "inf_weight_kg_12m", measureOfInterestTitle = "12-Month Weight (kg)")
{
  load("data/metaData_generalCharacteristics.RData") #loads in "dataSet" which is a data frame containing metadata
  load("data/lefse_allData_rapidGrowth_updated_notLogged.RData") #loads in "myT" which is a data frame with lefse_formatted taxa counts per sample
  
  taxaMeta <- merge(dataSet,myT,by.x="dyad_id",by.y="row.names")
  # taxaOfInterest <- c("k__Bacteria| p__Firmicutes| c__Bacilli| o__Lactobacillales| f__Enterococcaceae",
  #                     "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Lachnospiraceae| g__Blautia",
  #                     "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Ruminococcaceae| g__Unclassified",
  #                     "k__Bacteria| p__Proteobacteria| c__Deltaproteobacteria| o__Desulfovibrionales| f__Desulfovibrionaceae",
  #                     "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Enterobacteriales| f__Enterobacteriaceae| g__Enterobacter",
  #                     "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Enterobacteriales| f__Enterobacteriaceae| g__Klebsiella",
  #                     "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Enterobacteriales| f__Enterobacteriaceae| g__Salmonella",
  #                     "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales")

  thisDataInstance <- taxaMeta[,names(taxaMeta) %in% c(taxaOfInterest,"k__Bacteria","baby_birthlength_cm","baby_birthweight_kg",measureOfInterest,"dyad_id")]
  thisDataInstance$dyad_id <- factor(thisDataInstance$dyad_id)
  
  taxaShort1 <- strsplit(as.character(taxaOfInterest),"\\|")
  taxaName1 <- taxaShort1[[1]][length(taxaShort1[[1]])]
  
  names(thisDataInstance)[ names(thisDataInstance) %in% measureOfInterest] <- "thisVariable"
  names(thisDataInstance)[ names(thisDataInstance) %in% taxaOfInterest] <- "thisTaxa"
  
  thisDataInstance$taxaRelAbundance <- as.numeric(as.character(thisDataInstance$thisTaxa))/as.numeric(as.character(thisDataInstance$k__Bacteria))
  thisDataInstance$logTaxaRelAbundance <- log10(((as.numeric(as.character(thisDataInstance$thisTaxa))/as.numeric(as.character(thisDataInstance$k__Bacteria)))*mean(as.numeric(as.character(thisDataInstance$k__Bacteria))))+1)
  
  modelForm=as.formula(paste("thisVariable","~logTaxaRelAbundance+baby_birthlength_cm+baby_birthweight_kg"  ));
  modelInfo <- lm(modelForm, data = thisDataInstance)
  coefs <- coef(modelInfo)
  #ci_est <- confint(modelInfo, 'logTaxaRelAbundance', level=0.95)
  #lower_ci_est <- ci_est[1]
  #upper_ci_est <- ci_est[2]
  
  thisBeta <- format(coefs["logTaxaRelAbundance"],digits=3);
  pVal <- format.pval(coef(summary(modelInfo))["logTaxaRelAbundance",4],digits=3);
  
  title <- paste("beta =",thisBeta,", p-val= ",pVal,sep = "")
  p <- ggplot(thisDataInstance,aes(x=logTaxaRelAbundance,y=thisVariable))
  print(p + geom_point( size = 8,color="grey36") +
          geom_abline(slope = coefs["logTaxaRelAbundance"],intercept = coefs["(Intercept)"]+mean(thisDataInstance$baby_birthlength_cm)*coefs["baby_birthlength_cm"]+mean(thisDataInstance$baby_birthweight_kg)*coefs["baby_birthweight_kg"] ,size=1.5,linetype = "dashed",colour="grey36")+
          ggtitle(title)+
          xlab(paste("Log Abundance of ", taxaName1)) +
          ylab(measureOfInterestTitle)+
          theme_classic(base_size = 20)+
          theme(axis.line=element_line(size=1),
                axis.ticks.y=element_line(size=1),
                axis.ticks.x=element_blank(),
                axis.text=element_text(face="bold",size=20),
                text=element_text(face="bold",size=16),
                legend.position="bottom",
                legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5)
          )+
          theme(axis.line.x = element_line(color="black", size = 2),
                axis.line.y = element_line(color="black", size = 2)
          )
  )
}








