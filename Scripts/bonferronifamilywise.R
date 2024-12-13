#family-wise corrections (bonferroni)
##1.read in output table for regressions
##2.for each set of analyses, 
##arrange p-value of interactions with NOS from smallest to largest and compare against alpha/(N-1),
##where N is number of remaining interactions to compare
##true significance is not acheived unless less than p < alpha/(N-1)
##3.sequential comparisons -- any failure to reach significance = all following comparisons fail to reach significance


library(tidyverse)


#read in output table for regressions


Modsumm = read.csv(paste(paste(subfolderE,"\\", dflistname, sep = ""), "AllCovariates.csv", sep = "_"), header = F)
Modsumm$V2[Modsumm$V2 == ""] = NA
RTsumm = Modsumm[paste("V", c(1:3, 12:19), sep = "")]
Accsumm = Modsumm[paste("V", c(1:11), sep = "")]


#for multiple regressions:
#look for indices where "+" is present in row (column V1) -- outputs are between these indices
#pick out only p-values of interactions
modelInd = row.names(Modsumm)[grepl("\\+", Modsumm$V1)] #if model contains no Adv take only AP; between 2-3, take without AP

#multiple PCA regression
startMod = which(grepl("RC", RTsumm$V1[as.numeric(modelInd)]))
startInd = as.numeric(modelInd[startMod])
endInd = ifelse (startMod == length(modelInd), nrow(Modsumm), (as.numeric(modelInd[startMod + 1])-1))
pca.acc.summ = Accsumm[startInd:endInd,]
pca.acc.summ = pca.acc.summ %>% fill(V2)
pca.rt.summ = RTsumm[startInd:endInd,]
pca.rt.summ = pca.rt.summ %>% fill(V2)

PCAadjP.rt = data.frame(dataset=character(), interaction=character(), b = numeric(), p=numeric(), adjpsig=character())
PCAadjP.acc = data.frame(dataset=character(), interaction=character(), b = numeric(), p=numeric(), adjpsig=character())

for (ds in modeldata){
  ACCintPs = pca.acc.summ[pca.acc.summ$V2 == ds & grepl("NOS:", pca.acc.summ$V5),c("V2", "V5", "V6", "V10")] %>% 
    add_column(X = NA) %>% `colnames<-`(c("dataset", "interactions", "b", "p", "adjpsig")) %>%
    mutate(p = as.numeric(p)) %>% arrange(p)
  for (r in 1:nrow(ACCintPs)){
    ACCintPs[r, "adjpsig"] = as.character(ACCintPs[r, "p"]<(0.05/(nrow(ACCintPs) - r + 1)))
    if(ACCintPs[r, "adjpsig"] == "FALSE"){
      ACCintPs = ACCintPs %>% fill(adjpsig)
      break
    }
  }
  ACCintPs = ACCintPs %>% arrange(interactions)
  PCAadjP.acc = rbind(PCAadjP.acc, ACCintPs)
  
  RTintPs = pca.rt.summ[pca.rt.summ$V2 == ds & grepl("NOS:", pca.rt.summ$V13),c("V2", "V13", "V14", "V18")] %>% 
    add_column(X = NA) %>% `colnames<-`(c("dataset", "interactions", "b", "p", "adjpsig")) %>%
    mutate(p = as.numeric(p)) %>% arrange(p)
  for (r in 1:nrow(RTintPs)){
    RTintPs[r, "adjpsig"] = as.character(RTintPs[r, "p"]<(0.05/(nrow(RTintPs) - r + 1)))
    if(RTintPs[r, "adjpsig"] == "FALSE"){
      RTintPs = RTintPs %>% fill(adjpsig)
      break
    }
  }
  RTintPs = RTintPs %>% arrange(interactions)
  PCAadjP.rt = rbind(PCAadjP.rt, RTintPs)
}

saveRDS(PCAadjP.rt, paste(subfolderE, "\\PCAbonferroniRT.rds", sep = ""))
saveRDS(PCAadjP.acc, paste(subfolderE, "\\PCAbonferroniAcc.rds", sep = ""))

#multiple cov regression with nAdv -- megastudies
startMod = which(grepl("nAdv", RTsumm$V1[as.numeric(modelInd)])&!grepl("RC", RTsumm$V1[as.numeric(modelInd)]))
startInd = as.numeric(modelInd[startMod])
endInd = ifelse (startMod == length(modelInd), nrow(Modsumm), (as.numeric(modelInd[startMod + 1])-1))
mega.acc.summ = Accsumm[startInd:endInd,]
mega.acc.summ = mega.acc.summ %>% fill(V2)
mega.rt.summ = RTsumm[startInd:endInd,]
mega.rt.summ = mega.rt.summ %>% fill(V2)

MEGAadjP.rt = data.frame(dataset=character(), interaction=character(), b = numeric(), p=numeric(), adjpsig=character())
MEGAadjP.acc = data.frame(dataset=character(), interaction=character(), b = numeric(), p=numeric(), adjpsig=character())

for (ds in modeldata[!grepl("AP", modeldata)]){
  ACCintPs = mega.acc.summ[mega.acc.summ$V2 == ds & grepl("NOS:", mega.acc.summ$V5),c("V2", "V5", "V6", "V10")] %>% 
    add_column(X = NA) %>% `colnames<-`(c("dataset", "interactions", "b", "p", "adjpsig")) %>%
    mutate(p = as.numeric(p)) %>% arrange(p)
  for (r in 1:nrow(ACCintPs)){
    ACCintPs[r, "adjpsig"] = as.character(ACCintPs[r, "p"]<(0.05/(nrow(ACCintPs) - r + 1)))
    if(ACCintPs[r, "adjpsig"] == "FALSE"){
      ACCintPs = ACCintPs %>% fill(adjpsig)
      break
    }
  }
  ACCintPs = ACCintPs %>% arrange(match(interactions, paste("NOS:", covariate.list, sep="")))
  MEGAadjP.acc = rbind(MEGAadjP.acc, ACCintPs)
  
  RTintPs = mega.rt.summ[mega.rt.summ$V2 == ds & grepl("NOS:", mega.rt.summ$V13),c("V2", "V13", "V14", "V18")] %>% 
    add_column(X = NA) %>% `colnames<-`(c("dataset", "interactions", "b", "p", "adjpsig")) %>%
    mutate(p = as.numeric(p)) %>% arrange(p)
  for (r in 1:nrow(RTintPs)){
    RTintPs[r, "adjpsig"] = as.character(RTintPs[r, "p"]<(0.05/(nrow(RTintPs) - r + 1)))
    if(RTintPs[r, "adjpsig"] == "FALSE"){
      RTintPs = RTintPs %>% fill(adjpsig)
      break
    }
  }
  RTintPs = RTintPs %>% arrange(match(interactions, paste("NOS:", covariate.list, sep="")))
  MEGAadjP.rt = rbind(MEGAadjP.rt, RTintPs)
}

saveRDS(MEGAadjP.rt, paste(subfolderE, "\\MultibonferroniRT.rds", sep = ""))
saveRDS(MEGAadjP.acc, paste(subfolderE, "\\MultibonferroniAcc.rds", sep = ""))


#multiple cov regression without nAdv -- AP
startMod = which(!grepl("nAdv", RTsumm$V1[as.numeric(modelInd)])&!grepl("RC", RTsumm$V1[as.numeric(modelInd)]))
startInd = as.numeric(modelInd[startMod])
endInd = ifelse (startMod == length(modelInd), nrow(Modsumm), (as.numeric(modelInd[startMod + 1])-1))
ap.acc.summ = Accsumm[startInd:endInd,]
ap.acc.summ = ap.acc.summ %>% fill(V2)
ap.rt.summ = RTsumm[startInd:endInd,]
ap.rt.summ = ap.rt.summ %>% fill(V2)

APadjP.rt = data.frame(dataset=character(), interaction=character(), b = numeric(), p=numeric(), adjpsig=character())
APadjP.acc = data.frame(dataset=character(), interaction=character(), b = numeric(), p=numeric(), adjpsig=character())

for (ds in modeldata[grepl("AP", modeldata)]){
  ACCintPs = ap.acc.summ[ap.acc.summ$V2 == ds & grepl("NOS:", ap.acc.summ$V5),c("V2", "V5", "V6", "V10")] %>% 
    add_column(X = NA) %>% `colnames<-`(c("dataset", "interactions", "b", "p", "adjpsig")) %>%
    mutate(p = as.numeric(p)) %>% arrange(p)
  for (r in 1:nrow(ACCintPs)){
    ACCintPs[r, "adjpsig"] = as.character(ACCintPs[r, "p"]<(0.05/(nrow(ACCintPs) - r + 1)))
    if(ACCintPs[r, "adjpsig"] == "FALSE"){
      ACCintPs = ACCintPs %>% fill(adjpsig)
      break
    }
  }
  ACCintPs = ACCintPs %>% arrange(match(interactions, paste("NOS:", covariate.list, sep="")))
  APadjP.acc = rbind(APadjP.acc, ACCintPs)
  
  RTintPs = mega.rt.summ[mega.rt.summ$V2 == ds & grepl("NOS:", ap.rt.summ$V13),c("V2", "V13", "V14", "V18")] %>% 
    add_column(X = NA) %>% `colnames<-`(c("dataset", "interactions", "b", "p", "adjpsig")) %>%
    mutate(p = as.numeric(p)) %>% arrange(p)
  for (r in 1:nrow(RTintPs)){
    RTintPs[r, "adjpsig"] = as.character(RTintPs[r, "p"]<(0.05/(nrow(RTintPs) - r + 1)))
    if(RTintPs[r, "adjpsig"] == "FALSE"){
      RTintPs = RTintPs %>% fill(adjpsig)
      break
    }
  }
  RTintPs = RTintPs %>% arrange(match(interactions, paste("NOS:", covariate.list, sep="")))
  APadjP.rt = rbind(APadjP.rt, RTintPs)
}

saveRDS(APadjP.rt, paste(subfolderE, "\\APMultibonferroniRT.rds", sep = ""))
saveRDS(APadjP.acc, paste(subfolderE, "\\APMultibonferroniAcc.rds", sep = ""))


#simple covariate regressions
modelIndS = row.names(Modsumm)[grepl("\\~", Modsumm$V1)&!grepl("\\+", Modsumm$V1)] #pick out models without additional interaction

#get NOS:cov interaction for each dataset -- dataframe with datasets as columns and covariates as rows; get row numbers for each covariate

simple.acc.summ = Accsumm[modelIndS[1]:modelInd[1],]
simple.acc.summ = simple.acc.summ %>% fill(V2)
simple.rt.summ = RTsumm[modelIndS[1]:modelInd[1],]
simple.rt.summ = simple.rt.summ %>% fill(V2)

SIMPLEadjP.rt = data.frame(dataset=character(), interaction=character(), b = numeric(), p=numeric(), adjpsig=character())
SIMPLEadjP.acc = data.frame(dataset=character(), interaction=character(), b = numeric(), p=numeric(), adjpsig=character())

for (ds in modeldata){
  ACCintPs = simple.acc.summ[simple.acc.summ$V2 == ds & grepl("NOS:", simple.acc.summ$V5),c("V2", "V5", "V6", "V10")] %>% 
    add_column(X = NA) %>% `colnames<-`(c("dataset", "interactions", "b", "p", "adjpsig")) %>%
    mutate(p = as.numeric(p)) %>% arrange(p)
  for (r in 1:nrow(ACCintPs)){
    ACCintPs[r, "adjpsig"] = as.character(ACCintPs[r, "p"]<(0.05/(nrow(ACCintPs) - r + 1)))
    if(ACCintPs[r, "adjpsig"] == "FALSE"){
      ACCintPs = ACCintPs %>% fill(adjpsig)
      break
    }
  }
  ACCintPs = ACCintPs %>% arrange(match(interactions, paste("NOS:", covariate.list, sep="")))
  SIMPLEadjP.acc = rbind(SIMPLEadjP.acc, ACCintPs)
  
  RTintPs = simple.rt.summ[simple.rt.summ$V2 == ds & grepl("NOS:", simple.rt.summ$V13),c("V2", "V13", "V14", "V18")] %>% 
    add_column(X = NA) %>% `colnames<-`(c("dataset", "interactions", "b", "p", "adjpsig")) %>%
    mutate(p = as.numeric(p)) %>% arrange(p)
  for (r in 1:nrow(RTintPs)){
    RTintPs[r, "adjpsig"] = as.character(RTintPs[r, "p"]<(0.05/(nrow(RTintPs) - r + 1)))
    if(RTintPs[r, "adjpsig"] == "FALSE"){
      RTintPs = RTintPs %>% fill(adjpsig)
      break
    }
  }
  RTintPs = RTintPs %>% arrange(match(interactions, paste("NOS:", covariate.list, sep="")))
  SIMPLEadjP.rt = rbind(SIMPLEadjP.rt, RTintPs)
}

saveRDS(SIMPLEadjP.rt, paste(subfolderE, "\\SimplebonferroniRT.rds", sep = ""))
saveRDS(SIMPLEadjP.acc, paste(subfolderE, "\\SimplebonferroniAcc.rds", sep = ""))
