library(plyr)

load("./Input/StudyData/ld.RT.w.Rdata")
AP16RAW <-ld.RT.w
AP16RAW <-AP16RAW[AP16RAW$stimClass == 'u' | AP16RAW$stimClass == 'p',] #no homonyms
names(AP16RAW)[names(AP16RAW) == 'string'] <- 'Word'
AP16Summ <- ddply(AP16RAW, c("Word", "cont", "nwDiff"), summarise, Acc = mean(acc), RT = mean(RT))
AP16Summ <- AP16Summ[AP16Summ$Acc>=0.5,] 

fam<-ddply(AP16RAW, c("Word", "cont", "nwDiff"), summarise, fam = mean(fam))
AP16Summ <- merge(AP16Summ, fam, by = c("Word", "cont", "nwDiff"))
AP16Summ$cell <- paste(AP16Summ$cont, "_", AP16Summ$nwDiff, sep = "")
AP16Summ <- merge(AP16Summ, UNION, by = "Word")
AP16Summ <- merge(AP16Summ, WSstats, by = "Word")
AP16Summ <- merge(AP16Summ, Hoffman.items[, c("Word", "SemD")], by = "Word", all.x = TRUE)
AP16Summ <- merge(AP16Summ, SUBTL.items[c("Word", "SubCD")], by = "Word", all.x = T)
AP16Summ <- merge(AP16Summ, Johns.items[c("Word", "MemCD")], by = "Word", all.x = T)
AP16Summ <- merge(AP16Summ, Brysbaert.items[c("Word", "Conc")], by = "Word", all.x = T)


names(AP16Summ)[names(AP16Summ) == 'SUBTLwf'] <- 'WF'
AP16Summ$lnWF <- log(AP16Summ$WF+1)
AP16Summ$logWF <- log10(AP16Summ$WF+1)
AP16Summ$logsqWF <- (log10(AP16Summ$WF+1))*(log10(AP16Summ$WF+1))

AP16Summ <- merge(AP16Summ, unique(AP16RAW[, c("Word", "stimClass")]), by = "Word")
#splitarray <- split(AP16Summ[(AP16Summ$NOM == 1), ], 
#                    with(AP16Summ[(AP16Summ$NOM == 1), ], 
#                         cell), drop = TRUE)
splitarray <- split(AP16Summ, with(AP16Summ, cell), drop = TRUE) 

#rename to conditions used in paper; add task prefix

names(splitarray)[names(splitarray) == "deg_e"] <- "H-D"
names(splitarray)[names(splitarray) == "deg_h"] <- "V-D"
names(splitarray)[names(splitarray) == "deg_p"] <- "P-D"
names(splitarray)[names(splitarray) == "full_e"] <- "H-F"
names(splitarray)[names(splitarray) == "full_h"] <- "V-F"
names(splitarray)[names(splitarray) == "full_p"] <- "P-F"

names(splitarray) <- paste("VLD_AP_", names(splitarray), sep="")