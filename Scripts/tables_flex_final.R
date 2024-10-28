#generates:
#1. for each model, a txt file of the model summary
#2. for each model, a tex file of: dataset name, df, adj.r.sq, b, se, vif, p, sig

library(car)
library(xtable)
library(gtools)
library(weights)
library(csv)
library(tibble)
library(pscl)
library(rsq)

#generate text output
y.list = list("RT")

RTModels <- list()

for(j in 1:length(model.list)){
  lmlist <- list()
  sink(file = paste(subfolderT,'\\', dflistname, "RT_", gsub("[^[:alnum:]+]","",gsub("[*]", "X", model.list[j])), ".txt", sep = ""))
  for(k in 1:length(df.list)){
    for(i in 1:length(y.list)){
      #lmlist[[name]] <- list(NA, NA, NA, NA)
      
      df <- df.list[[k]]
      
      model <- paste(y.list[[i]], model.list[[j]], sep = "")
      dataname <- names(df.list)[k]
      
      gdmodel <- try(lm(model, data=df, na.rm = T), silent=TRUE)
      
      if(class(gdmodel) != "try-error"){
        
        coeff <- summary(lm(model, data=df, na.rm = T))$coef
        
        print(paste(dataname,":", model))
        
        print(summary(lm(model, data = df, na.rm = T)))
        
        print(paste("BIC: ", BIC(lm(model, data = df, na.rm = T))))
        
        gdvif <- try(vif(lm(model, data = df, na.rm = T)), silent=TRUE)
        
        if(class(gdvif) == "try-error"){
          VIF <- rep(NA, length(variable.names(lm(model, data=df, na.rm = T), full = T)[-1]))
          names(VIF) <- variable.names(lm(model, data=df, na.rm = T), full = T)[-1]
        }
        
        if(class(gdvif) != "try-error"){
          VIF <- vif(lm(model, data = df, na.rm = T))
          print(paste('VIF', names(VIF), ':', VIF))
        }
        
        name <- paste(dataname, y.list[i], sep = "_")
        lmlist[[name]] <- list(coeff, summary(lm(model, data = df, na.rm = T))$df[2], summary(lm(model, data = df, na.rm = T))$adj.r.square, VIF)
      }
    }
  }
  sink(file = NULL)
  RTModels[[model.list[[j]]]] <- lmlist
}

#generate text output

y.list = list("Acc")


#create table of results to print
#create table with the following columns:
AccModels <- list()
for(j in 1:length(model.list)){
  sink(file = paste(subfolderT,'\\', dflistname, "Acc_", gsub("[^[:alnum:]+]","",gsub("[*]", "X", model.list[j])), ".txt", sep = ""))
  lmlist <- list()
  for(k in 1:length(df.list)){
    for(i in 1:length(y.list)){
      df <- df.list[[k]]
      
      model <- paste(y.list[[i]], model.list[[j]], sep = "")
      dataname <- names(df.list)[k]
      
      gdmodel <- try(glm(model, family = binomial(link = "logit"), data=df, na.action = na.omit), silent=TRUE)
      
      if(class(gdmodel) != "try-error"){
        
        modsum <- summary(glm(model, family = binomial(link = "logit"), data=df, na.action = na.omit))
        
        print(paste(dataname,"binomial:", model))
        
        print(summary(glm(model, family = binomial(link = "logit"), data=df, na.action = na.omit)))
        
        print(anova(glm(model, family = binomial(link = "logit"), data=df, na.action = na.omit), test="Chisq"))
        
        print(pR2(glm(model, family = binomial(link = "logit"), data=df, na.action = na.omit)))
        
        print(paste("adj.r.sq: ", rsq(glm(model, family = binomial(link = "logit"), data=df, na.action = na.omit), adj = T)))
        
        print(paste("BIC: ", BIC(glm(model, family = binomial(link = "logit"), data=df, na.action = na.omit))))
        
        gdvif <- try(vif(glm(model, family = binomial(link = "logit"), data = df, na.action = na.omit)), silent=TRUE)
        
        if(class(gdvif) == "try-error"){
          VIF <- rep(NA, length(variable.names(glm(model, family = binomial(link = "logit"), data=df, na.action = na.omit), full = T)[-1]))
          names(VIF) <- variable.names(lm(model, data=df, na.action = na.omit), full = T)[-1]
        }
        
        if(class(gdvif) != "try-error"){
          VIF <- vif(glm(model, family = binomial(link = "logit"), data = df, na.action = na.omit))
          print(paste('VIF', names(VIF), ':', VIF))
        }
        
        name <- paste(dataname, y.list[i], sep = "_")
        
        lmlist[[name]] <- list(modsum$coef, modsum$df[2], rsq(glm(model, family = binomial(link = "logit"), data=df, na.action = na.omit), adj = T), VIF)
        
      }
    }
  }
  sink(file = NULL)
  AccModels[[model.list[[j]]]] <- lmlist 
}


#arrange dataset sequence

for (m in 1:length(model.list)){
  print(model.list[m])
  logacc <- AccModels[model.list[[m]]]
  wfrt <- RTModels[model.list[[m]]]
  
  if (length(logacc[[1]]) == 0){
    next
  }
  collated <- data.frame(matrix(NA, nrow = length(df.list)*length(rownames(logacc[[1]][[1]][[1]])) - 1 + 4, ncol = 19)) #one per model
  collated[1, 1] <- model.list[[m]]
  collated[2, 2] <- "Accuracy"
  collated[2, 11] <- "RT"
  
  collated[3,1:19] <- list("Task", "Dataset", "df", 
                           "adj.r.sq", "Predictor", "b", "SE", "t", "p", "sig", 
                           "adj.r.sq", "Predictor", "b", "SE", "t", "p", "sig",
                           "VIF", "VIF")
  
  #for (i in 1:length(logacc[[1]])){
  for (i in 1:length(modeldata)){
    print(modeldata[i])
    #get dataset name (assume names are in the format of dataset_y)
    #dname <- sub('_[^_]*$', '', grep(modeldata[i], names(logacc), value=TRUE))
    if(length(grep(modeldata[i], names(logacc[[1]]), value=TRUE))==0){next}
    dname <- sub('_Acc', '', grep(modeldata[i], names(logacc[[1]]), value=TRUE))
    #dname <- modeldata[sapply(modeldata, function(x) length(names(logacc[[1]])[grepl(x, names(logacc[[1]]))])>0)]
    #if(length(dname)==0){next}
    
    #set rownames
    effectsVIF <- names(logacc[[1]][grep(dname, names(logacc[[1]]), value=TRUE)][[1]][[4]]) #vif (shorter) names
    effectsFull <- rownames(logacc[[1]][grep(dname, names(logacc[[1]]), value=TRUE)][[1]][[1]])[-1]
      
    #change predictor name in printout if needed
    effects <- sub("Freqhigh", "Freq", effectsVIF)
    effects <- sub("Sensemany", "Sense", effects)
    
    #################
    
    collated[((i-1)*length(effects)+4):(i*length(effects)+3), c(5, 12)] <- effects
    
    collated[((i-1)*length(effects)+4), 2] <- dname
    #set task name
    if(dname == "AP_full_e"){collated[((i-1)*length(effects)+4), 1] <- "VLD"}
    if(dname == "SDP"){collated[((i-1)*length(effects)+4), 1] <- "SD"}
    if(dname == "ELP_NMG"){collated[((i-1)*length(effects)+4), 1] <- "NMG"}
    if(dname == "AELP"){collated[((i-1)*length(effects)+4), 1] <- "ALD"}
    
    #get dataset df
    collated[((i-1)*length(effects)+4), 3] <- logacc[[1]][grep(dname, names(logacc[[1]]), value=TRUE)][[1]][[2]]
    #get dataset adj.r.sq
    collated[((i-1)*length(effects)+4), 4] <- logacc[[1]][grep(dname, names(logacc[[1]]), value=TRUE)][[1]][[3]]
    collated[((i-1)*length(effects)+4), 11] <- wfrt[[1]][grep(dname, names(wfrt[[1]]), value=TRUE)][[1]][[3]]
    #get dataset coeff
    COEFacc <- data.frame(matrix(NA, nrow = length(effectsVIF), ncol = 4))
    COEFrt <- data.frame(matrix(NA, nrow = length(effectsVIF), ncol = 4))
    
    #for(c in 1:length(effects0)){
    #  COEFacc[c,] <- logacc[[1]][grep(dname, names(logacc[[1]]), value=TRUE)][[1]][[1]][effects0[c],]
    #  COEFrt[c,] <- wfrt[[1]][grep(dname, names(wfrt[[1]]), value=TRUE)][[1]][[1]][effects0[c],]
    #}
    
    for(c in 1:length(effectsVIF)){
      if(effectsVIF[c] %in% rownames(logacc[[1]][grep(dname, names(logacc[[1]]), value=TRUE)][[1]][[1]])){
        COEFacc[c,] <- logacc[[1]][grep(dname, names(logacc[[1]]), value=TRUE)][[1]][[1]][effectsVIF[c],]
      }
      if(effectsVIF[c] %in% rownames(wfrt[[1]][grep(dname, names(wfrt[[1]]), value=TRUE)][[1]][[1]])){
        COEFrt[c,] <- wfrt[[1]][grep(dname, names(wfrt[[1]]), value=TRUE)][[1]][[1]][effectsVIF[c],]
      }
      
      if(effectsFull[c] %in% rownames(logacc[[1]][grep(dname, names(logacc[[1]]), value=TRUE)][[1]][[1]])){
        COEFacc[c,] <- logacc[[1]][grep(dname, names(logacc[[1]]), value=TRUE)][[1]][[1]][effectsFull[c],]
      }
      if(effectsFull[c] %in% rownames(wfrt[[1]][grep(dname, names(wfrt[[1]]), value=TRUE)][[1]][[1]])){
        COEFrt[c,] <- wfrt[[1]][grep(dname, names(wfrt[[1]]), value=TRUE)][[1]][[1]][effectsFull[c],]
      }
      
    }
    
    collated[((i-1)*length(effects)+4):(i*length(effects)+3), 6:9] <- COEFacc
    collated[((i-1)*length(effects)+4):(i*length(effects)+3), 13:16] <- COEFrt
    collated[((i-1)*length(effects)+4):(i*length(effects)+3), 5] <- effects
    collated[((i-1)*length(effects)+4):(i*length(effects)+3), 12] <- effects
    
    #get VIF
    VIFacc <- list()
    VIFrt <- list()
    
    for(v in 1:length(effectsVIF)){
      VIFacc[v] <- logacc[[1]][grep(dname, names(logacc[[1]]), value=TRUE)][[1]][[4]][effectsVIF[v]]
      VIFrt[v] <- wfrt[[1]][grep(dname, names(wfrt[[1]]), value=TRUE)][[1]][[4]][effectsVIF[v]]
    }
    
    collated[((i-1)*length(effectsVIF)+4):(i*length(effectsVIF)+3), 18] <- unlist(VIFacc)
    collated[((i-1)*length(effectsVIF)+4):(i*length(effectsVIF)+3), 19] <- unlist(VIFrt)
  }
  
  #remove extraneous rows
  collated <- collated[rowSums(is.na(collated)) != ncol(collated),]
  
  #mark out significance
  for (r in 3:dim(collated)[1]){
    collated[r, 10] <- stars.pval(as.numeric(collated[r, 9]))
    collated[r, 17] <- stars.pval(as.numeric(collated[r, 16]))
  }
  
  #reorder VIF columns
  collated <- collated[c(1:7, 18, 8:14, 19, 15:17)]
  
  #write to csv
  write.table(collated, paste(paste(subfolderE,"\\", dflistname, sep = ""), "AllCovariates.csv", sep = "_"),
              sep = ",", na = "", row.names = F, col.names = F, append = T)
  
  
  #"formatted" tables
  collatedAcc <- collated[2:dim(collated)[1],1:11]
  collatedRT <- collated[2:dim(collated)[1],c(1:3,12:19)]
  
  colnames(collatedAcc) <- collatedAcc[2,]
  collatedAcc <- collatedAcc[-(1:2),]
  colnames(collatedRT) <- collatedRT[2,]
  collatedRT <- collatedRT[-(1:2),]
  
  #need to not have NA for rounding to work, change all NA values in columns for rounding to 999, and change back to NA after
  
  collatedAcc[c("adj.r.sq", "b", "SE", "t", "p", "VIF")][is.na(collatedAcc[c("adj.r.sq", "b", "SE", "t", "p","VIF")])] <- 999
  collatedRT[c("adj.r.sq", "b", "SE", "t", "p", "VIF")][is.na(collatedRT[c("adj.r.sq", "b", "SE", "t", "p","VIF")])] <- 999
  
  collatedAcc$p <- rd(as.numeric(collatedAcc$p), digits=3)
  collatedRT$p <- rd(as.numeric(collatedRT$p), digits=3)
  
  collatedAcc$p[as.numeric(collatedAcc$p)< 0.001] <- "<.001"
  collatedRT$p[as.numeric(collatedRT$p)< 0.001] <- "<.001"
  
  collatedAcc$t <- rd(abs(as.numeric(collatedAcc$t)), digits = 2)
  collatedRT$t <- rd(abs(as.numeric(collatedRT$t)), digits = 2)
  
  collatedAcc$adj.r.sq[!(is.na(collatedAcc$adj.r.sq))] <- 
    rd(as.numeric(collatedAcc$adj.r.sq[!(is.na(collatedAcc$adj.r.sq))]), digits = 2)
  collatedRT$adj.r.sq[!(is.na(collatedRT$adj.r.sq))] <- 
    rd(as.numeric(collatedRT$adj.r.sq[!(is.na(collatedRT$adj.r.sq))]), digits = 2)
  
  collatedAcc$b <- as.character(round(as.numeric(collatedAcc$b), 4)) #do not remove leading zero
  collatedRT$b <- as.character(round(as.numeric(collatedRT$b), 4))
  
  collatedAcc$SE <- as.character(round(as.numeric(collatedAcc$SE), 4)) #do not remove leading zero
  collatedRT$SE <- as.character(round(as.numeric(collatedRT$SE), 4))
  
  
  collatedAcc$VIF <- as.character(round(as.numeric(collatedAcc$VIF), 2))
  collatedRT$VIF <- as.character(round(as.numeric(collatedRT$VIF), 2))
  
  collatedAcc[c("adj.r.sq", "b", "SE", "t", "p", "VIF")][collatedAcc[c("adj.r.sq", "b", "SE", "t", "p", "VIF")]==999 | 
                                                           collatedAcc[c("adj.r.sq", "b", "SE", "t", "p", "VIF")]=="999.00" ] <- "NA"
  collatedRT[c("adj.r.sq", "b", "SE", "t", "p", "VIF")][collatedRT[c("adj.r.sq", "b", "SE", "t", "p", "VIF")]==999 | 
                                                          collatedRT[c("adj.r.sq", "b", "SE", "t", "p", "VIF")]=="999.00"] <- "NA"
  
  
  #print to xtable
  print(
    xtable(collatedAcc, type = "latex")
    , include.rownames = FALSE
    , include.colnames = TRUE
    , size = "normalsize"
    , digits = c(0, 0, 0, 5, 0 ,5 ,5 ,5 ,5, 5)
    , append = TRUE,
    file = paste(subfolderL,'\\', dflistname, "logitAcc_", gsub("[^[:alnum:]+]","",gsub("[*]", "X", model.list[m])), ".tex", sep = "")
  )
  print(
    xtable(as.data.frame(collatedRT), type = "latex", digits = 5)
    , include.rownames = FALSE
    , include.colnames = TRUE
    , size = "normalsize"
    , append = TRUE,
    file = paste(subfolderL,'\\', dflistname, "RT_", gsub("[^[:alnum:]+]","",gsub("[*]", "X", model.list[m])), ".tex", sep="")
  )
  
}

