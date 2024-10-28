# Load packages
library(openxlsx)
library(csv)
library(plyr)

#import megastudy datasets: BLP, ELP, SDP, AELP, MALD

# BLP data
load("./Input/StudyData/blp-items.Rdata") #blp.items
BLP <- blp.items[blp.items$lexicality == "W" ,c("spelling", "rt", "accuracy")] #takes only words
colnames(BLP) <- c("Word", "RT", "Acc")
BLP$zRT <- scale(BLP$RT, center = FALSE, scale = TRUE)


# ELP data and separate into lexical decision and naming
ELP <- read.csv("./Input/StudyData/ELP.csv", stringsAsFactors=F)
ELP[ELP == "NULL"] <- NA
ELP_LD <- ELP[, !names(ELP) %in% c("I_NMG_Mean_RT", "I_NMG_Mean_Accuracy")]
ELP_NMG <- ELP[, !names(ELP) %in% c("I_Mean_RT", "I_Mean_Accuracy")]

ELP_LD = ELP_LD[complete.cases(ELP_LD[c("I_Mean_RT", "I_Mean_Accuracy")]),]
ELP_LD$RT <- as.numeric(ELP_LD$I_Mean_RT)
ELP_LD$Acc <- as.numeric(ELP_LD$I_Mean_Accuracy)

ELP_NMG = ELP_NMG[complete.cases(ELP_NMG[c("I_NMG_Mean_RT", "I_NMG_Mean_Accuracy")]),]
ELP_NMG$RT <- as.numeric(ELP_NMG$I_NMG_Mean_RT)
ELP_NMG$Acc <- as.numeric(ELP_NMG$I_NMG_Mean_Accuracy)


# SDP (Pexman) data
SDP.items <- read.xlsx("./Input/StudyData/Pexman.xlsx")
SDP <- SDP.items[,c("Word", "RTclean_mean", "zRTclean_mean", "ACC")]
names(SDP) <- c("Word", "RT", "zRT", "Acc")


# AELP data
AELP.items <- read.csv("./Input/StudyData/aelp_data.csv")
AELP <- AELP.items[, c("ï..word_us", "m1_ldt_acc", "m2_ldt_acc", "m3_ldt_acc", 
                       "f1_ldt_acc", "f2_ldt_acc", "f3_ldt_acc", 
                       "m1_ldt_rt_m", "m2_ldt_rt_m", "m3_ldt_rt_m",
                       "f1_ldt_rt_m", "f2_ldt_rt_m", "f3_ldt_rt_m")]
names(AELP)[names(AELP) == "ï..word_us"] <- "Word"
##get average across speakers
AELP$Acc <- rowMeans(AELP[, c("m1_ldt_acc", "m2_ldt_acc", "m3_ldt_acc",
                         "f1_ldt_acc", "f2_ldt_acc", "f3_ldt_acc")])
AELP$RT <- rowMeans(AELP[, c("m1_ldt_rt_m", "m2_ldt_rt_m", "m3_ldt_rt_m",
                             "f1_ldt_rt_m", "f2_ldt_rt_m", "f3_ldt_rt_m")])


# MALD
MALD.items <- read.table("./Input/StudyData/MALD1_AllData.txt", header = T)
MALDraw <- MALD.items[MALD.items$IsWord == "TRUE", c("Item", "RT", "ACC")] #take only words
MALD <- ddply(MALDraw, .(Item), summarise, RT = mean(RT), Acc = mean(ACC))
names(MALD)[names(MALD) == "Item"] <- "Word"


#clean megatsudies data for accuracy >= 50%
BLP <- BLP[BLP$Acc>=0.5,]
ELP_LD <- ELP_LD[ELP_LD$Acc>=0.5,]
ELP_NMG <- ELP_NMG[ELP_NMG$Acc>=0.5,]
SDP <- SDP[SDP$Acc>=0.5,]
AELP <- AELP[AELP$Acc>=0.5,]
MALD <- MALD[MALD$Acc>=0.5,]

# Jager stimuli
Jager.items <- read.xlsx("./Input/StudyData/JagerStimuli.xlsx")


#import covariates

# CELEX (British lemma freq)
celex.items <- read.table("./Input/Predictors/efl.cd", sep="\\", quote = "", header = F)
colnames(celex.items) <- c("IdNum", "Word", "Cob", "CobDev", "CobMln", "CobLog", "CobW", "CobWMln", "CobWLog", "CobS", "CobSMln", "CobSLog")
celex <- celex.items[,c("Word", "CobWMln")] #cobuild written word per mil
celex <- ddply(celex, "Word", summarise, WF = sum(CobWMln))

# Contextual richness/diversity data: Hoffman2013, Brysbaert2009, Johns2021
Hoffman.items <- read.xlsx("./Input/Predictors/HoffmanContextualDiversity.xlsx")
colnames(Hoffman.items)[1] <- "Word"

SUBTL.items <- read.xlsx("./Input/Predictors/SUBTLEXusExcel2007.xlsx")
SUBTL.items$Word <- tolower(SUBTL.items$Word)
colnames(SUBTL.items)[colnames(SUBTL.items) == "Lg10CD"] = "SubCD"

Johns.items <- read.xlsx("./Input/Predictors/Johns_MC_CDvals.xlsx")
colnames(Johns.items)[colnames(Johns.items) == "DCD-SD-PR"] = "MemCD"

# Concreteness data: Brysbaert
Brysbaert.items <- read.xlsx("./Input/Predictors/Concreteness_ratings_Brysbaert.xlsx")
colnames(Brysbaert.items)[3] <- "Conc"

# Wordsmyth factors
WSstats <- readxl::read_xlsx("./Input/Predictors/WSstats_updated.xlsx")
  
# Other covariates: word length, neighbourhood, SUBLEXus word frequency (already restricted to >= one per million)
UNION_full <- read.table("./Input/Predictors/SUBTL_CMU_expanded_May_20_2013.txt", sep="\t", fill = T, header = T)
colnames(UNION_full)[1] <- "Word"
UNION <- UNION_full[, c("Word", "SUBTLwf", "nLet", "nSyll", "nPhon", "coltNOrth", "coltNPhon", "old20", "pld20", "posBigram", "posUni", "NphonOnsetNeighbour", "NphonOffsetNeighbours")]
colnames(UNION) <- c("Word", "SUBTLwf", "nLet", "nSyll", "nPhon", "coltNOrth", "coltNPhon", "old20", "pld20", "posBi", "posUni", "NphonOn", "NphonOff")

# Covariates lists to merge
df_blp.list <- list(BLP, celex, UNION, WSstats)
df_sdp.list <- list(SDP, UNION, WSstats)
df_elp_ld.list <- list(ELP_LD, UNION, WSstats)
df_elp_nmg.list <- list(ELP_NMG, UNION, WSstats)
df_aelp.list <- list(AELP, UNION, WSstats)
df_mald.list <- list(MALD, UNION, WSstats)


#merge lists for each megastudy analysed and covariates

#dfs <- c("df_blp.list", "df_sdp.list", "df_elp_ld.list", "df_elp_nmg.list", "df_aelp.list", "df_mald.list")
dfid <- c("BLP", "SDP", "ELP_LD", "ELP_NMG", "AELP", "MALD")

dfMain <- list()

for (i in 1:length(dfid)){
  df.full <- get(dfid[i])
  
  if (dfid[i] == "BLP"){
    df.full <- merge(df.full, celex, by = "Word", all.x = TRUE) #use CELEX WF for British English
  }
  
  
  #df.full <- Reduce(function(x, y) merge(x, y, by="Word", all.x = TRUE), mergelist)
  df.full <- merge(df.full, UNION, by = "Word", all.x = TRUE)
  df.full <- merge(df.full, WSstats, by = "Word", all.x = TRUE)
  df.full <- merge(df.full, Hoffman.items[, c("Word", "SemD")], by = "Word", all.x = TRUE)
  df.full <- merge(df.full, SUBTL.items[c("Word", "SubCD")], by = "Word", all.x = T)
  df.full <- merge(df.full, Johns.items[c("Word", "MemCD")], by = "Word", all.x = T)
  df.full <- merge(df.full, Brysbaert.items[c("Word", "Conc")], by = "Word", all.x = T)
  
  ##clean-up for WF transforms
  ###clean <- subset(df.full, df.full$type != "homonymhybrid")
  clean <- df.full
  ## Refer to SUBTLwf for word frequency (if not BLP)
  if(dfid[i] != "BLP"){
    names(clean)[names(clean) == 'SUBTLwf'] <- 'WF'
  }
  
  ### Add transforms for WF
  clean$lnWF <- log(clean$WF+1)
  clean$logWF <- log10(clean$WF+1)
  clean$logsqWF <- (log10(clean$WF+1))*(log10(clean$WF+1))
   
  ## Ensure dependent variables are complete for a clean merge
  clean <- clean[complete.cases(clean$RT),]
  clean <- clean[complete.cases(clean$Acc),]
  #clean <- clean[complete.cases(clean$logWF),]
  #clean <- clean[complete.cases(clean$NOS),]

  # Append to list
  dfMain[[i]]<- clean
}  

#NOTE: editted names -- make sure order matches dfid above
dfid2 <- c("VLD_BLP", "SD_SDP", "VLD_ELP", "NMG_ELP", "ALD_AELP", "ALD_MALD")
names(dfMain) <- dfid2
