library(xtable)

#create table of descriptive stats

df.listJ <- df.list
df.listJ$Jager <- jager.full
#df.listJ$AP <- df.list$AP_deg_h

description <- read.csv(".\\Input\\Misc\\descriptivestext.csv", colClasses = rep("character", 7))

descriptives <- data.frame(matrix(NA, nrow = 10, ncol = (8 + (3 * 4) + length(covariate.list)*4)))

models <- c("Jager", "BLP", "ELP_LD", "AP_full_e", "AP_deg_h", "AELP", "MALD", "SDP", "ELP_NMG")

covariate.list_long_headings = rep("", 4*length(covariate.list))
for (i in 1:length(covariate.list)){
  covariate.list_long_headings[4*i - 3] = covariate.list[i]
}

items <- c("RT", "Acc", "NOS", covariate.list)


descriptives[1, 1:(8 + (3 * 4) + length(covariate.list)*4)] <- c("Task", "Dataset", "Notes", "Foil Type", "No. of Words", 
                          "No. of Participants", "Words per Participant", "Words Analysed", 
                          "RT","","","","Acc","","","", "NOS","","","", 
                          covariate.list_long_headings)

descriptives[2, 1:(8 + (3 * 4) + length(covariate.list)*4)] <- c("", "", "", "", "", "", "", "",
                          rep(c("Mean","SD","Range","IQR"), 3+length(covariate.list)))

descriptives[3:11, 2] <- models

#no dp for RT; 1 dp for NOS, nN, nV, nA, nA; 1 dp for nLet, nSyll; no dp for pld, old,posU, posB 


for(n in 1:length(models)){ #where n+2 is row of interest
  descriptives[(n+2), 1] <- description[grep(models[n], description$Dataset), 1]
  descriptives[(n+2), 3:7] <- description[grep(models[n], description$Dataset), 3:7]
  descriptives[(n+2), 8] <- length(df.listJ[[models[n]]]$Word) #find no. of words
  for(o in 1:length(items)){
    dp = 2
    if(items[o] %in% c("RT", "plo20", "old20", "posUni", "posBi")){dp = 0} #set number of decimals to round
    else if(items[o] %in% c("NOS", "nNoun", "nVerb", "nAdj", "nAdv", "nLet", "nSyll")){dp = 1}

    
    if (!(models[n] == "Jager" & items[o] %in% c("RT", "Acc"))){ 
      descriptives[(n+2), 9+((o-1)*4)] <- round(unlist(lapply(df.listJ[[models[n]]][items[o]], mean, na.rm = T)), dp) #find mean
      descriptives[(n+2), 10+((o-1)*4)] <- round(unlist(lapply(df.listJ[[models[n]]][items[o]], sd, na.rm = T)), dp) #find sd
      descriptives[(n+2), 11+((o-1)*4)] <- as.character(paste(round(min(df.listJ[[models[n]]][items[o]], na.rm = T), dp), 
                                                 "-",round(max(df.listJ[[models[n]]][items[o]], na.rm = T), dp)))  #find min and max
      quant <- lapply(df.listJ[[models[n]]][items[o]], quantile, na.rm = TRUE)
      descriptives[(n+2), 12+((o-1)*4)] <- as.character(paste(round(quant[[1]]["25%"], dp),
                                                 "-", round(quant[[1]]["75%"], dp)))#find IQR
    }
  }
}

write.table(descriptives, paste(subfolder, "\\descriptives_", dflistname, ".csv", sep=""), row.names = F, col.names = F, sep = ",")

print(
     xtable(descriptives[,1:8], type = "latex")
     , include.rownames = FALSE
     , include.colnames = FALSE
     , size = "normalsize",
     file = paste(subfolder, "\\Descriptives_original.tex", sep = "")
     )

desc_colnames = c("Dataset", descriptives[1,8], descriptives[2,c(9:12)], "Variable")
descriptives_long = descriptives[3:(3+length(modeldata)-1),c(2, 8, 9:12)]
descriptives_long[7] = descriptives[1,9]
colnames(descriptives_long) = desc_colnames
coln = 13
while (coln < ncol(descriptives) - 4){
  d_l = descriptives[3:(3+length(modeldata)-1),c(2, 8, coln:(coln+3))]
  d_l[7] = descriptives[1,coln]
  colnames(d_l) = desc_colnames
  descriptives_long = rbind(descriptives_long, d_l)
  coln = coln + 4
}

descriptives_long_arranged = dplyr::arrange(descriptives_long, factor(Dataset, levels = models))

print(
  xtable(descriptives_long_arranged[,c(1,2,7, c(3:6))], type = "latex")
  , include.rownames = FALSE
  , include.colnames = TRUE
  , size = "normalsize",
  file = paste(subfolder, "\\Descriptives_current", dflistname, ".tex", sep = "")
)
