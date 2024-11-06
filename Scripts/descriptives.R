library(xtable)

#create tables of descriptive stats (one for original data; one for analysed data)

#change original data to latex table:
description <- read.csv(".\\Input\\Misc\\descriptivestext.csv", colClasses = rep("character", 7))

print(
  xtable(description, type = "latex")
  , include.rownames = FALSE
  , include.colnames = TRUE
  , size = "normalsize",
  file = paste(subfolder, "\\Descriptives_original.tex", sep = "")
)

#descriptives table of dependent and independent variables

models <- c("VLD_AP_H-F", "VLD_AP_V-D", "VLD_BLP", "VLD_ELP", "ALD_AELP", "ALD_MALD", "NMG_ELP", "SD_SDP")

descriptives <- data.frame(matrix(NA, nrow = 1+((length(covariate.list)+3)*4), ncol = 7))
descriptives[1,] = c("Dataset", "Words Analysed", "Variable", "Mean","SD","Range","IQR")

items <- c("RT", "Acc", "NOS", covariate.list)

for(n in 1:length(models)){ #where n+2 is row of interest
  
  descriptives[((n-1) * length(items))+2, 1] <- models[n]
  descriptives[((n-1) * length(items))+2, 2] <- length(df.list[[models[n]]]$Word) #find no. of words
  descriptives[(((n-1) * length(items))+2):(n * length(items) + 1), 3] <- items

  for(o in 1:length(items)){
    #set number of decimals to round
    dp = 2 #default
    if(items[o] %in% c("RT", "plo20", "old20", "posUni", "posBi")){dp = 0 
    }else if(items[o] %in% c("NOS", "nNoun", "nVerb", "nAdj", "nAdv", "nLet", "nSyll")){dp = 1}
    
    descriptives[(((n-1) * length(items))+ 1 + o), 4] <- round(unlist(lapply(df.list[[models[n]]][items[o]], mean, na.rm = T)), dp) #find mean
    descriptives[(((n-1) * length(items))+ 1 + o), 5] <- round(unlist(lapply(df.list[[models[n]]][items[o]], sd, na.rm = T)), dp) #find sd
    descriptives[(((n-1) * length(items))+ 1 + o), 6] <- as.character(paste(round(min(df.list[[models[n]]][items[o]], na.rm = T), dp), 
                                                            "-",round(max(df.list[[models[n]]][items[o]], na.rm = T), dp)))  #find min and max
    quant <- lapply(df.list[[models[n]]][items[o]], quantile, na.rm = TRUE)
    descriptives[(((n-1) * length(items))+ 1 + o), 7] <- as.character(paste(round(quant[[1]]["25%"], dp),
                                                            "-", round(quant[[1]]["75%"], dp)))#find IQR
  }
}


write.table(descriptives, paste(subfolder, "\\descriptives_", dflistname, ".csv", sep=""), row.names = F, col.names = F, sep = ",")

print(
     xtable(descriptives, type = "latex")
     , include.rownames = FALSE
     , include.colnames = FALSE
     , size = "normalsize",
     file = paste(subfolder, "\\Descriptives_current.tex", sep = "")
     )
