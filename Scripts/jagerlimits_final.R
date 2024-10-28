dfMain4 <- list()

for (i in 1:length(dfMain2)){
  
  df.lim <- dfMain2[[i]]
  
  df.lim <- subset(df.lim, df.lim$WF >= 2)
  df.lim <- subset(df.lim, df.lim$WF <= 353)
  
  df.lim <- subset(df.lim, df.lim$NOS >= 1)
  df.lim <- subset(df.lim, df.lim$NOS <= 21)
 
  dfMain4[[i]] <- df.lim
}  

names(dfMain4) <- names(dfMain2)