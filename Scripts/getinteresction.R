wordlist <- jager.full$Word

dfMain3 <- list()

for (i in 1:length(dfMain2)){
  
  df.intersect <- dfMain2[[i]]
  
  df.intersect <- subset(df.intersect, df.intersect$Word %in% wordlist)
  
  dfMain3[[i]] <- merge(df.intersect, jager.full[,c("Word", "Sense", "Freq")], by = "Word")
  
  dfMain3[[i]]$Sense <- relevel(as.factor(dfMain3[[i]]$Sense), "few")
  
  dfMain3[[i]]$Freq <- relevel(as.factor(dfMain3[[i]]$Freq), "low")
}  

names(dfMain3) <- names(dfMain2)