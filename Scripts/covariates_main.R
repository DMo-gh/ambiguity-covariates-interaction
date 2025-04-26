#######Section 0: prepare workspace######

###set working directory to current folder###
library(rstudioapi)

current_path <- getActiveDocumentContext()$path #get the path of current open file
setwd(dirname(current_path)) #set the wd to the relevant one:
setwd('..')


##set/create output directory
outpath = "Output"
ifelse(!dir.exists(file.path(getwd(), outpath)), dir.create(file.path(getwd(), outpath)), FALSE)

##covariates included in test for interaction
covariate.list <- c("nNoun", "nVerb", "nAdj", "nAdv",
                    "Conc", "SemD", "SubCD", "MemCD",
                    "logWF", "nLet", "nSyll", "pld20", "old20", 
                    "posUni", "posBi")

##principle components of covariates to test for interaction
pca.list <- c("RC1", "RC2", "RC3", "RC4")

##regression models to be run: NOS X each covariate; 
##                             NOS X PCA components; 
##                             NOS X all covariates except nAdv (due to statistical invariance in AP datasets); 
##                             NOS X all covariates; 
model.list <- list()
for (i in 1:length(covariate.list)){model.list[i] <- paste("~ NOS *", covariate.list[i])}
model.list <- c(model.list, paste("~ NOS * (", paste(pca.list, collapse = ' + '), ")"),
                paste("~ NOS * (", paste(covariate.list[-which(covariate.list=="nAdv")], collapse = ' + '), ")"),
                paste("~ NOS * (", paste(covariate.list, collapse = ' + '), ")")) #this one has to come last as missing nAdvs VIFs will error out

##names of datasets to be run
#modeldata <- c("AP_full_e", "AP_deg_h", "BLP", "ELP_LD", "AELP", "MALD", "ELP_NMG", "SDP")
modeldata <- c("VLD_AP_H-F", "VLD_AP_V-D", "VLD_BLP", "VLD_ELP", "ALD_AELP", "ALD_MALD", "NMG_ELP", "SD_SDP")



###import basic dataframes###

#import megastudies
#generates dfMain with named list of clean dataset
source(".\\Scripts\\import_df_finalised.R")

#import AP16
#generates splitarray2 with named list of clean dataset
source(".\\Scripts\\import_AP_final.R")

#import jager data
source(".\\Scripts\\jagerdata_final.R")

#combine datasets to work with
dfMain2 <- c(dfMain, splitarray)

#Clean the datasets: exclude items with WF < 1 and items without all relevant covariate values
source(".\\Scripts\\clean_df_finalised.R")
assign("dfMain2", clean_df("dfMain2", covariate.list, modeldata)) #assigns cleaned df to the dflistname

#get intersection of each dataset with jager
source(".\\Scripts\\getinteresction.R")

#limit each dataset with Jager WF limits (2-353)
source(".\\Scripts\\jagerlimits_final.R")

#available datasets:
subfolder = paste(outpath, "\\AnalysedDatasets\\", sep = "")
ifelse(!dir.exists(file.path(getwd(), subfolder)), dir.create(file.path(getwd(), subfolder)), FALSE)

#dfMain: only BLP, ELP, SDP, AELP, MALD
saveRDS(dfMain, paste0(subfolder, "dfMain.rds"))
#dfMain2: all non-jager datasets
saveRDS(dfMain2, paste0(subfolder, "dfMain2.rds"))
#dfMain3: all datasets intersected with Jager words (freq split at 24; sense split at 4)
saveRDS(dfMain3, paste0(subfolder, "dfMain3.rds"))
#dfMain4: all datasets limited by Jager WF and senses upper and lower bounds
saveRDS(dfMain4, paste0(subfolder, "dfMain4.rds"))

###Section 1: Covariate Interactions###

#set working dataframe: all table and plot generation code point to "df.list"
dflistname = "dfMain2"
df.list = get(dflistname)

#get table of descriptives: brief summaries of analysed studies and descriptive statistics of analysed items
subfolder = paste(outpath, "\\Descriptives", sep = "")
ifelse(!dir.exists(file.path(getwd(), subfolder)), dir.create(file.path(getwd(), subfolder)), FALSE)
source(".\\Scripts\\descriptives.R")

#get union of items from all datasets: needed to plot correlograms and run PCA
library(GGally)
source('.\\Scripts\\rbindcols.R') #script to concatenate dataframes with different column names

dataunion <- data.frame(matrix(NA, nrow = 0, ncol = 0))

for (i in modeldata) {
  if (nrow(dataunion)==0) {
    dataunion <- df.list[[i]]
  }else{
    dataunion <- rbind.match.columns(dataunion, df.list[[i]])
  }
}
dataunion <- dataunion[, !(colnames(dataunion) %in% c("RT","Acc"))]
dataunion <- dataunion[match(unique(dataunion$Word), dataunion$Word),]

#run PCA: rotated principle components will be appended as new columns in df.list; generates scree plot and loadings

subfolder = paste(outpath, "\\PCA", sep = "")
ifelse(!dir.exists(file.path(getwd(), subfolder)), dir.create(file.path(getwd(), subfolder)), FALSE)

source(".\\Scripts\\principlecomponents.R")

#plot correlograms of raw covariates (overall and by-dataset)and behaviour across datasets (RT and Acc respectively)

subfolder = paste(outpath, "\\Correlograms", sep = "")
ifelse(!dir.exists(file.path(getwd(), subfolder)), dir.create(file.path(getwd(), subfolder)), FALSE)

source(".\\Scripts\\plot_descriptive_correlograms.R")

#plot binned regressions for simple regression

subfolder = paste(outpath, "\\BinnedPlots", sep = "")
ifelse(!dir.exists(file.path(getwd(), subfolder)), dir.create(file.path(getwd(), subfolder)), FALSE)

source(".\\Scripts\\binnedplot_compact_pca.R")
source(".\\Scripts\\binnedplot_compact_simple.R")
source(".\\Scripts\\binnedplot_compact_multi.R")

#plot interaction plots

subfolder = paste(outpath, "\\InteractionPlots", sep = "")
ifelse(!dir.exists(file.path(getwd(), subfolder)), dir.create(file.path(getwd(), subfolder)), FALSE)

ptype = "simple" #plot type: simple regressions
source(".\\Scripts\\interplot_flex.R")

ptype = "multi" #plot type: multiple regressions
source(".\\Scripts\\interplot_flex.R")

ptype = "pca" #plot type: multiple regressions with principle components
source(".\\Scripts\\interplot_flex.R")


#regression analyses: runs all models in model.list on all datasets in df.list
#outputs: printout of model summaries, latex tables of reported values, excel tables of reported values

subfolderT = paste(outpath, "\\RegressionTxt", sep = "")
subfolderL = paste(outpath, "\\UnformattedLatexTables", sep = "")
subfolderE = paste(outpath, "\\ExcelTables", sep = "")
ifelse(!dir.exists(file.path(getwd(), subfolderT)), dir.create(file.path(getwd(), subfolderT)), FALSE)
ifelse(!dir.exists(file.path(getwd(), subfolderL)), dir.create(file.path(getwd(), subfolderL)), FALSE)
ifelse(!dir.exists(file.path(getwd(), subfolderE)), dir.create(file.path(getwd(), subfolderE)), FALSE)

source(".\\Scripts\\tables_flex_final.R")

#run bonferroni familywise corrections
source(".\\Scripts\\bonferronifamilywise.R")


###Section 2: replications of previous findings###

#Jager:

#datasets as intersected with Jager items: runs both factoral and continuous regressions
##all datasets run, but only visual lexical decision and semantic decision are of interest
dflistname = "dfMain3"
df.list = get(dflistname)
model.list = c("~ Sense * Freq", "~ NOS * logWF")
source(".\\Scripts\\tables_flex_final.R")

#datasets with items falling within word frequency and NOS of Jager items: runs continuous regressions
##all datasets run, but only visual lexical decision and semantic decision are of interest
dflistname = "dfMain4"
df.list = get(dflistname)
model.list = c("~ NOS * logWF")
source(".\\Scripts\\tables_flex_final.R")


#Rice )

library(readxl)
Rice2019 <- read_excel(".\\Input\\Misc\\Rice et al Supplementary Materials.xlsx")
##get ColtNOrth from UNION_full and combine with BLP data
BLPdf = dfMain2$VLD_BLP %>% 
  filter(Word %in% Rice2019$Item)%>%
  merge(UNION_full[c("Word", "coltNOrth")], by = "Word", all.x = T)

sink(paste0(outpath, "\\RiceReplication.txt"))

print("Full Interaction Models:")
print(summary(lm("RT ~ NOS * coltNOrth", data = BLPdf)))
print(car::vif(lm("RT ~ NOS * coltNOrth", data = BLPdf)))
print(summary(lm("Acc ~ NOS * coltNOrth", data = BLPdf)))
print(car::vif(lm("Acc ~ NOS * coltNOrth", data = BLPdf)))
      
print("Interaction Terms Only:")
print(summary(lm("RT ~ NOS : coltNOrth", data = BLPdf)))
print(summary(lm("Acc ~ NOS : coltNOrth", data = BLPdf)))

sink()
sink()