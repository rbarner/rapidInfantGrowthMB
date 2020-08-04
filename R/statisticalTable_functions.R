summarize_1MonthCharacteristics_byRapidGrowth <- function()
{
  load("data/metaData_generalCharacteristics.RData") #loads in "dataSet" which is a data frame containing metadata
  MM <- dataSet
  MM$mode_of_delivery_cat <- factor(ifelse(MM$mode_of_delivery %in% 1, "Vaginal","C-Section"),levels = c("Vaginal","C-Section"))
  MM$baby_gender_cat <- factor(ifelse(MM$baby_gender %in% 1, "Female","Male"))
  MM$Milk_BL_Cat <- factor(ifelse(MM$breastmilk_per_day_baseline < 8, "Low","High"))

  DespFactors <- c("mom_age_at_birth","prepreg_bmi_kgm2", "mode_of_delivery_cat",
                   "SES_index_final",
                   "age_in_days_baseline", "baby_gender_cat",
                   	"baby_birthweight_kg","baby_birthlength_cm",
                   "inf_weight_kg_baseline","inf_length_cm_baseline",
                   "zwfl_baseline","zbmi_baseline", "Milk_BL_Cat")

  exposureNamesList <- character(0); allOutcomes <- character(0);
  overallColumn <- character(0); rapidGrowthColumn <- character(0); nonRapidGrowthColumn <- character(0);

  crudeORList <- numeric(0);
  pValORList <- numeric(0);

  for(vari in DespFactors)
  {
    allOutcomes[[length(allOutcomes)+1]] <- vari;
    if(vari %in% c("mode_of_delivery_cat","baby_gender_cat","Milk_BL_Cat"))
    {
      countFactor1_All <- table(MM[,names(MM) %in% vari])[1]
      countFactor2_All <- table(MM[,names(MM) %in% vari])[2]
      percentFactor_All <- format(countFactor1_All/(countFactor1_All+countFactor2_All)*100,digits = 2)
      thisAllColumn <- paste(countFactor1_All, ", ", countFactor2_All,", ",percentFactor_All,"%",sep="")
      overallColumn[[length(overallColumn)+1]] <- thisAllColumn;

      countFactor1_Rapid <- table(MM[MM$rapidGrowth %in% "Rapid Growth",names(MM) %in% vari])[1]
      countFactor2_Rapid <- table(MM[MM$rapidGrowth %in% "Rapid Growth",names(MM) %in% vari])[2]
      percentFactor_Rapid <- format(countFactor1_Rapid/(countFactor1_Rapid+countFactor2_Rapid)*100,digits = 2)
      thisRapidColumn <- paste(countFactor1_Rapid, ", ", countFactor2_Rapid,", ",percentFactor_Rapid,"%",sep="")
      rapidGrowthColumn[[length(rapidGrowthColumn)+1]] <- thisRapidColumn;

      countFactor1_NonRapid <- table(MM[MM$rapidGrowth %in% "Non-rapid Growth",names(MM) %in% vari])[1]
      countFactor2_NonRapid <- table(MM[MM$rapidGrowth %in% "Non_rapid Growth",names(MM) %in% vari])[2]
      percentFactor_NonRapid <- format(countFactor1_NonRapid/(countFactor1_NonRapid+countFactor2_NonRapid)*100,digits = 2)
      thisRapidColumn <- paste(countFactor1_NonRapid, ", ", countFactor2_NonRapid,", ",percentFactor_NonRapid,"%",sep="")
      nonRapidGrowthColumn[[length(nonRapidGrowthColumn)+1]] <- thisNonRapidColumn;
    }
    else
    {
      meanAll<- format(mean(MM[,names(MM) %in% vari]),digits=3)
      sdAll <- format(sd(MM[,names(MM) %in% vari]),digits=3)
      thisAllColumn <- paste(meanAll, "±", sdAll)
      overallColumn[[length(overallColumn)+1]] <- thisAllColumn;

      meanRapid <- format(mean(MM[MM$rapidGrowth %in% "Rapid Growth",names(MM) %in% vari]),digits=3)
      sdRapid <- format(sd(MM[MM$rapidGrowth %in% "Rapid Growth",names(MM) %in% vari]),digits=3)
      thisRapidColumn <- paste(meanRapid, "±", sdRapid)
      rapidGrowthColumn[[length(rapidGrowthColumn)+1]] <- thisRapidColumn;

      meanNonRapid <- format(mean(MM[MM$rapidGrowth %in% "Non-rapid Growth",names(MM) %in% vari]),digits=3)
      sdNonRapid <- format(sd(MM[MM$rapidGrowth %in% "Non-rapid Growth",names(MM) %in% vari]),digits=3)
      thisNonRapidColumn <- paste(meanNonRapid, "±", sdNonRapid)
      nonRapidGrowthColumn[[length(nonRapidGrowthColumn)+1]] <- thisNonRapidColumn;
    }

    modelForm=as.formula(paste("rapidGrowth","~",vari  ));
    modelInfo <- glm(modelForm, data = MM,family="binomial")

    coefs <- coef(modelInfo)
    oddsRatio <- exp(cbind(OR = coef(modelInfo), confint(modelInfo)))
    thisOR <- format(oddsRatio[2,1],digits = 3)
    thisORUpper <- format(oddsRatio[2,3],digits = 3)
    thisORLower <- format(oddsRatio[2,2],digits = 3)
    crudeOR <- paste(thisOR," (",thisORLower,", ",thisORUpper, ")",sep="")
    crudeORList[[length(crudeORList)+1]] <- crudeOR;

    pVal <- format.pval(coef(summary(modelInfo))[2,4],digits=2);
    pValORList[[length(pValORList)+1]] <- pVal
  }

  allOutcomes <- cbind(allOutcomes,overallColumn,rapidGrowthColumn,nonRapidGrowthColumn,crudeORList,pValORList);
  allOutcomes <- data.frame(allOutcomes)
  return(allOutcomes)
}

compute_OR_RapidGrowth_byAlphaDiversity <- function()
{
  load("data/metaData_generalCharacteristics.RData") #loads in "dataSet" which is a data frame containing metadata
  load("data/alphaDiversity_infants_1month.RData") #loads in "myT5" which is a data frame with alpha diversity indices per sample
  taxaMeta <- merge(dataSet,myT5,by="dyad_id")

  diversityIndices <- c("shannonDiversity","richness", "evenness","numSeqPerSample")

  allOutcomes <- character(0);
  tStatList <- numeric(0); pValTtestList <- numeric(0);
  adjustedORList <- character(0); pValORList <- numeric(0);

  for(vari in diversityIndices)
  {
    allOutcomes[[length(allOutcomes)+1]] <- vari;
    modelFormTtest=as.formula(paste(vari,"~rapidGrowth"  ));
    modelFormOR=as.formula(paste("rapidGrowth","~",vari, "+baby_birthlength_cm+baby_birthweight_kg" ));

    ### T-test to test for differences in diversity by rapid growth status ####
    modelInfoTtest <- t.test(modelFormTtest, data = taxaMeta)

    thisTstatistic <- format(modelInfoTtest$statistic,digits = 3)
    tStatList[[length(tStatList)+1]] <- thisTstatistic;

    pValTtest <- format.pval(modelInfoTtest$p.value,digits=3);
    pValTtestList[[length(pValTtestList)+1]] <- pValTtest

    ### Logistic Regression to test if diversity explains rapid growth status ####
    modelInfoOR <- glm(modelFormOR, data = taxaMeta,family="binomial")

    coefs <- coef(modelInfoOR)
    oddsRatio <- exp(cbind(OR = coef(modelInfoOR), confint(modelInfoOR)))
    thisOR <- format(oddsRatio[2,1],digits = 3)
    thisORUpper <- format(oddsRatio[2,3],digits = 3)
    thisORLower <- format(oddsRatio[2,2],digits = 3)
    adjustedOR <- paste(thisOR," (",thisORLower,", ",thisORUpper, ")",sep="")
    adjustedORList[[length(adjustedORList)+1]] <- adjustedOR;

    pValOR <- format.pval(coef(summary(modelInfoOR))[2,4],digits=2);
    pValORList[[length(pValORList)+1]] <- pValOR
  }
  adjTtestList <- p.adjust(pValTtestList,method = "BH");
  adjORpValList <- p.adjust(pValORList,method = "BH");
  allOutcomes <- cbind(allOutcomes,tStatList,pValTtestList,adjTtestList,adjustedORList,pValORList,adjORpValList);
  allOutcomes <- data.frame(allOutcomes)
  return(allOutcomes)
}

compute_associations_alphaDiversity_growthMeasures <- function()
{
  load("data/metaData_generalCharacteristics.RData") #loads in "dataSet" which is a data frame containing metadata
  load("data/alphaDiversity_infants_1month.RData") #loads in "myT5" which is a data frame with alpha diversity indices per sample
  taxaMeta <- merge(dataSet,myT5,by="dyad_id")

  diversityIndices <- c("shannonDiversity","richness", "evenness","numSeqPerSample")

  growthMeasuresList<-c("diffGrowth_zwei","diffGrowth_zbmi","diffGrowth_zwfl",
                        "zwei_12m","zbmi_12m","zwfl_12m","inf_weight_kg_12m","inf_length_cm_12m",
                        "skinf_midthigh_mm_12m","skinf_tricep_mm_12m","skinf_supra_mm_12m","skinf_subscap_mm_12m")

  outcomesNamesList <- character(0);
  allOutcomes <- diversityIndices;
  for(variableOfInterest in growthMeasuresList)
  {
    adjustedLMList <- character(0);
    pValLMList <- numeric(0);

    for(diversityIndex in diversityIndices)
    {
      #allOutcomes[[length(allOutcomes)+1]] <- diversityIndex;
      modelFormLM=as.formula(paste(variableOfInterest,"~",diversityIndex, "+baby_birthlength_cm+baby_birthweight_kg" ));

      ### Linear Regression to test if diversity explains 12 month growth measures ####
      modelInfoLM <- lm(modelFormLM, data = taxaMeta)
      coefs <- coef(modelInfoLM)
      ci_est <- confint(modelInfoLM, diversityIndex, level=0.95)

      lower_ci_est <- format(ci_est[1],digits = 3)
      upper_ci_est <- format(ci_est[2],digits = 3)
      thisBeta <- format(coefs[diversityIndex],digits=3);
      adjustedLM <- paste(thisBeta," (",lower_ci_est,", ",upper_ci_est, ")",sep="")
      adjustedLMList[[length(adjustedLMList)+1]] <- adjustedLM;

      pValLM <- format.pval(coef(summary(modelInfoLM))[diversityIndex,4],digits=3);
      pValLMList[[length(pValLMList)+1]] <- pValLM
    }
    adjPvals <- p.adjust(pValLMList,method = "BH")
    allOutcomes <- cbind(allOutcomes,adjustedLMList, pValLMList,adjPvals);
    outcomesNamesList <- cbind(outcomesNamesList,paste(variableOfInterest,"beta_(CI)",sep="_"),paste(variableOfInterest,"pval",sep="_"),paste(variableOfInterest,"adj_pval",sep="_"))
  }
  allOutcomes <- data.frame(allOutcomes)
  names(allOutcomes) <- c("DiversityMeasure",outcomesNamesList);

  return(allOutcomes)
}


compute_OR_RapidGrowth_bylefseTaxa <- function()
{
  load("data/metaData_generalCharacteristics.RData") #loads in "dataSet" which is a data frame containing metadata
  load("data/lefse_allData_rapidGrowth_updated_notLogged.RData") #loads in "myT" which is a data frame with lefse_formatted taxa counts per sample

  taxaMeta <- merge(dataSet,myT,by.x="dyad_id",by.y="row.names")
  taxaOfInterest <- c("k__Bacteria| p__Firmicutes| c__Bacilli| o__Lactobacillales| f__Enterococcaceae",
                      "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Lachnospiraceae| g__Blautia",
                      "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Ruminococcaceae| g__Unclassified",
                      "k__Bacteria| p__Proteobacteria| c__Deltaproteobacteria| o__Desulfovibrionales| f__Desulfovibrionaceae",
                      "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Enterobacteriales| f__Enterobacteriaceae| g__Enterobacter",
                      "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Enterobacteriales| f__Enterobacteriaceae| g__Klebsiella",
                      "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Enterobacteriales| f__Enterobacteriaceae| g__Salmonella",
                      "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales")

  allOutcomes <- character(0);
  tStatList <- numeric(0); pValTtestList <- numeric(0);
  adjustedORList <- character(0); pValORList <- numeric(0);

  for(vari in taxaOfInterest)
  {
    thisDataInstance2 <- data.frame(dyad_id=taxaMeta$dyad_id,
                                    thisTaxa=taxaMeta[,names(taxaMeta) %in% vari],
                                    k__Bacteria=taxaMeta$k__Bacteria,
                                    birthlength=taxaMeta$baby_birthlength_cm,
                                    birthweight=taxaMeta$baby_birthweight_kg,
                                    rapidGrowth =taxaMeta$rapidGrowth.x)

    if(table(thisDataInstance2$thisTaxa)[names(table(thisDataInstance2$thisTaxa)) %in% c(" 0","  0","0","   0","    0")] < dim(thisDataInstance2)[1]/2)
    {
      allOutcomes[[length(allOutcomes)+1]] <- vari;
      thisDataInstance2$taxaRelAbundance <- as.numeric(as.character(thisDataInstance2$thisTaxa))/as.numeric(as.character(thisDataInstance2$k__Bacteria))
      thisDataInstance2$logTaxaRelAbundance <- log10(((as.numeric(as.character(thisDataInstance2$thisTaxa))/as.numeric(as.character(thisDataInstance2$k__Bacteria)))*mean(as.numeric(as.character(thisDataInstance2$k__Bacteria))))+1)

      thisDataInstance2$dyad_id <- factor(thisDataInstance2$dyad_id)
      modelFormOR=as.formula(paste("rapidGrowth","~logTaxaRelAbundance+birthlength+birthweight" ));

      ### Logistic Regression to test if diversity explains rapid growth status ####
      modelInfoOR <- glm(modelFormOR, data = thisDataInstance2,family="binomial")
      coefs <- coef(modelInfoOR)
      oddsRatio <- exp(cbind(OR = coef(modelInfoOR), confint(modelInfoOR)))
      thisOR <- format(oddsRatio[2,1],digits = 3)
      thisORUpper <- format(oddsRatio[2,3],digits = 3)
      thisORLower <- format(oddsRatio[2,2],digits = 3)
      adjustedOR <- paste(thisOR," (",thisORLower,", ",thisORUpper, ")",sep="")
      adjustedORList[[length(adjustedORList)+1]] <- adjustedOR;

      pValOR <- format.pval(coef(summary(modelInfoOR))[2,4],digits=2);
      pValORList[[length(pValORList)+1]] <- pValOR

    }

  }
  adjORpValList <- p.adjust(pValORList,method = "BH");
  allOutcomes <- cbind(allOutcomes,adjustedORList,pValORList,adjORpValList);
  allOutcomes <- data.frame(allOutcomes)
  return(allOutcomes)
}

compute_associations_lefseTaxa_growthMeasures <- function()
{
  load("data/metaData_generalCharacteristics.RData") #loads in "dataSet" which is a data frame containing metadata
  load("data/lefse_allData_rapidGrowth_updated_notLogged.RData") #loads in "myT" which is a data frame with lefse_formatted taxa counts per sample

  taxaMeta <- merge(dataSet,myT,by.x="dyad_id",by.y="row.names")
  taxaOfInterest <- c("k__Bacteria| p__Firmicutes| c__Bacilli| o__Lactobacillales| f__Enterococcaceae",
                      "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Lachnospiraceae| g__Blautia",
                      "k__Bacteria| p__Firmicutes| c__Clostridia| o__Clostridiales| f__Ruminococcaceae| g__Unclassified",
                      "k__Bacteria| p__Proteobacteria| c__Deltaproteobacteria| o__Desulfovibrionales| f__Desulfovibrionaceae",
                      "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Enterobacteriales| f__Enterobacteriaceae| g__Enterobacter",
                      "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Enterobacteriales| f__Enterobacteriaceae| g__Klebsiella",
                      "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Enterobacteriales| f__Enterobacteriaceae| g__Salmonella",
                      "k__Bacteria| p__Proteobacteria| c__Gammaproteobacteria| o__Pseudomonadales")

  growthMeasuresList<-c("diffGrowth_zwei","diffGrowth_zbmi","diffGrowth_zwfl",
                        "zwei_12m","zbmi_12m","zwfl_12m","inf_weight_kg_12m","inf_length_cm_12m",
                        "skinf_midthigh_mm_12m","skinf_tricep_mm_12m","skinf_supra_mm_12m","skinf_subscap_mm_12m")
  outcomesNamesList <- character(0);
  allOutcomes <- taxaOfInterest;
  for(variableOfInterest in growthMeasuresList)
  {
    adjustedLMList <- character(0);
    pValLMList <- numeric(0);

    for(vari in taxaOfInterest)
    {
      thisDataInstance2 <- data.frame(dyad_id=taxaMeta$dyad_id,
                                      thisTaxa=taxaMeta[,names(taxaMeta) %in% vari],
                                      thisVariable=taxaMeta[,names(taxaMeta) %in% variableOfInterest],
                                      k__Bacteria=taxaMeta$k__Bacteria,
                                      birthlength=taxaMeta$baby_birthlength_cm,
                                      birthweight=taxaMeta$baby_birthweight_kg)
      thisDataInstance2$taxaRelAbundance <- as.numeric(as.character(thisDataInstance2$thisTaxa))/as.numeric(as.character(thisDataInstance2$k__Bacteria))
      thisDataInstance2$logTaxaRelAbundance <- log10(((as.numeric(as.character(thisDataInstance2$thisTaxa))/as.numeric(as.character(thisDataInstance2$k__Bacteria)))*mean(as.numeric(as.character(thisDataInstance2$k__Bacteria))))+1)
      modelFormLM=as.formula(paste("thisVariable","~logTaxaRelAbundance+birthlength+birthweight"  ));


      ### Linear Regression to test if diversity explains 12 month growth measures ####
      modelInfoLM <- lm(modelFormLM, data = thisDataInstance2)
      coefs <- coef(modelInfoLM)
      ci_est <- confint(modelInfoLM, "logTaxaRelAbundance", level=0.95)

      lower_ci_est <- format(ci_est[1],digits = 3)
      upper_ci_est <- format(ci_est[2],digits = 3)
      thisBeta <- format(coefs["logTaxaRelAbundance"],digits=3);
      adjustedLM <- paste(thisBeta," (",lower_ci_est,", ",upper_ci_est, ")",sep="")
      adjustedLMList[[length(adjustedLMList)+1]] <- adjustedLM;

      pValLM <- format.pval(coef(summary(modelInfoLM))["logTaxaRelAbundance",4],digits=3);
      pValLMList[[length(pValLMList)+1]] <- pValLM
    }
    adjPvals <- p.adjust(pValLMList,method = "BH")
    allOutcomes <- cbind(allOutcomes,adjustedLMList, pValLMList,adjPvals);
    outcomesNamesList <- cbind(outcomesNamesList,paste(variableOfInterest,"beta_(CI)",sep="_"),paste(variableOfInterest,"pval",sep="_"),paste(variableOfInterest,"adj_pval",sep="_"))
  }
  allOutcomes <- data.frame(allOutcomes)
  names(allOutcomes) <- c("Significant_LEfSe_Taxa",outcomesNamesList);

  return(allOutcomes)
}
