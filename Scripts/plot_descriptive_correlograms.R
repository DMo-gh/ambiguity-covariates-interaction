#full versions
library("psych")


pdf(paper="special", file=paste(paste(subfolder, "\\correlogram_cov_dataunion.pdf", sep=""), sep=""), height=20, width=20)
print(ggpairs(dataunion[,c("NOS", covariate.list)],
        ggplot2::aes(alpha = 0.1, pch = "."),
        upper = list(continuous = wrap("cor", size = 5, hjust = 0.6, margin=margin(0,0,0,0))),
        lower = list(continuous = "smooth", size=0.5),
        columnLabels = c("NOS", covariate.list)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=21),
        axis.text = element_text(size=17),
        axis.text.x = element_text(angle = 90, hjust = 1)
  ))
dev.off()
ggsave(paste(subfolder,"\\correlogram_cov_dataunion.png", sep=""),height=20, width=20)

sink(file= paste(subfolder,"\\covariate_correlations.txt", sep = ""))
print(corr.test(dataunion[,c("NOS", covariate.list)]))
sink(file = NULL)

#correlogram for each dataset
for (i in modeldata) {
  pdf(paper="special", file=paste(subfolder, "\\correlogram_cov_",i,".pdf", sep=""), height=20, width=20)
  print(ggpairs(df.list[[i]][,c("NOS", covariate.list)], upper = list(continuous = wrap("cor", size = 5))) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
  dev.off()
}

#correlate across datasets for each dependent variable
source(".\\Scripts\\ggally_custom_cor.R") #for correlogram formatting

for (dv in c("RT", "Acc")){
  cdf <- data.frame(matrix(ncol = 1, nrow = nrow(dataunion)))
  colnames(cdf) <- c("Word")
  cdf$Word <- dataunion$Word
  cdf$type <- dataunion$type

  for (d in 1:length(df.list)){
    cdf = merge(cdf, df.list[[names(df.list)[d]]][c("Word",dv)], by="Word", all.x = T)
    names(cdf)[names(cdf) == dv] = names(df.list)[d]
  }
  pdf(paper="special", file=paste(subfolder, "\\correlogram_", dv, ".pdf", sep=""), height=8, width=8)
  print(ggpairs(cdf[,modeldata], 
          ggplot2::aes(alpha = 0.1, pch = "."),  
          upper = list(continuous = ggally_cor_New), 
          lower = list(continuous = "smooth", size=0.5)) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          strip.text = element_text(size=12), 
          axis.text.y = element_text(size=8),
          axis.text.x = element_text(size=8, angle = 90)
    ))
  dev.off()
  ggsave(paste(subfolder,"\\correlogram_", dv, ".png", sep=""),height=8.5, width=8.5)
  #dev.off()
}


#simplified versions
pdf(paper="special", file=paste(subfolder, "\\simplecorrelogram_cov_dataunion.pdf", sep=""), height=8, width=8)
print(ggcorr(dataunion[,c("NOS", covariate.list)], hjust = 0.75, size = 3, label = T))
dev.off()

ggsave(paste(subfolder,"\\simplecorrelogram_cov_dataunion.png", sep=""),height=8, width=8)
#dev.off()


for (dv in c("RT", "Acc")){
  cdf <- data.frame(matrix(ncol = 1, nrow = nrow(dataunion)))
  colnames(cdf) <- c("Word")
  cdf$Word <- dataunion$Word
  cdf$type <- dataunion$type
  
  for (d in 1:length(df.list)){
    cdf = merge(cdf, df.list[[names(df.list)[d]]][c("Word",dv)], by="Word", all.x = T)
    names(cdf)[names(cdf) == dv] = names(df.list)[d]
  }
  pdf(paper="special", file=paste(subfolder, "\\simplecorrelogram_", dv, ".pdf", sep=""), height=6.5, width=7)
  print(ggcorr(cdf[,modeldata], hjust = 0.75, size = 3, label = T, layout.exp = 1))
  dev.off()
  ggsave(paste(subfolder,"\\simplecorrelogram_", dv, ".png", sep=""),height=6.5, width=7)
  #dev.off()
}

