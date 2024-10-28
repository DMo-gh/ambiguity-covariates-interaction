library(lavaan) # Structural Equation Modelling (SEM) package used for Confirmatory Factor Analysis (CFA)
library(semPlot) # Plots results of SEM/CFA
library(psych) # Reliability Analysis

png(file = paste(subfolder, "\\PCAscree_",dflistname,".png", sep = ""),   # The directory you want to save the file in
    width = 600, height = 600) # default unit is pixels
scree(na.omit(dataunion[, c(covariate.list)]),factors=FALSE,pc=TRUE,main=paste("Scree plot: PCA on union of datasets"),hline=NULL,add=FALSE) 
dev.off()


dataunion.pca <- principal(na.omit(dataunion[, c(covariate.list)]), nfactors = 4, rotate = "varimax")
dataunionPCA <- na.omit(dataunion[, c("Word", covariate.list)])
dataunionPCA <- cbind(dataunion, dataunion.pca$scores)

#sink(file= paste(subfolder, "\\PCAloadings_rotated_", dflistname, ".txt", sep = ""))
#print("PCA:")
#print(dataunion.pca)
#print("\n")
#print("loadings with no cutoff:")
#print("")
#print(dataunion.pca$loadings, digits = 5, cutoff = 0)
#print("")
#print("loadings with cutoff at 0.3:")
#print("")
#print(dataunion.pca$loadings, digits = 3, cutoff = 0.3)
#sink()

sink(file= paste(subfolder, "\\PCAloadings_rotated_", dflistname, ".txt", sep = ""))
print(dataunion.pca)
cat("\n\n")
cat("loadings with no cutoff:\n")
print(dataunion.pca$loadings, digits = 5, cutoff = 0)
cat("\n")
cat("loadings with cutoff at 0.3:\n")
print(dataunion.pca$loadings, digits = 3, cutoff = 0.3)
sink()


#add principle components to each dataset

for (i in modeldata){
  df.list[[i]] <- merge(df.list[[i]], dataunionPCA[, c("Word", "RC1", "RC2", "RC3", "RC4")], by = "Word")
}


#print correlogram of each principle component with NOS

pdf(paper="special", file=paste(paste(subfolder, "\\correlogram_pca_dataunion.pdf", sep=""), sep=""), height=10, width=10)
print(ggpairs(dataunionPCA[,c("NOS", "RC1", "RC2", "RC3", "RC4")],
              ggplot2::aes(alpha = 0.1, pch = "."),
              upper = list(continuous = wrap("cor", size = 5, hjust = 0.6, margin=margin(0,0,0,0))),
              lower = list(continuous = "smooth", size=0.5)) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.text = element_text(size=21),
              axis.text = element_text(size=17),
              axis.text.x = element_text(angle = 90, hjust = 1)
        ))
dev.off()
ggsave(paste(subfolder,"\\correlogram_pca_dataunion.png", sep=""),height=10, width=10)



####additional check: if NOS included in PCA, which component(s) will it load with?####
#dataunion.nos.pca <- principal(na.omit(dataunionPCA[, c("NOS", covariate.list)]), nfactors = 5, rotate = "varimax")
#sink(file= paste(subfolder, "\\PCAloadings5_wNOS_rotated_", dflistname, ".txt", sep = ""))
#print(dataunion.nos.pca)
#cat("\n\n")
#cat("loadings with no cutoff:\n")
#print(dataunion.nos.pca$loadings, digits = 5, cutoff = 0)
#cat("\n")
#cat("loadings with cutoff at 0.3:\n")
#print(dataunion.nos.pca$loadings, digits = 3, cutoff = 0.3)
#sink()
#####