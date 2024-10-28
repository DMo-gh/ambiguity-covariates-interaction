df_jager.list <- list(Jager.items, WSstats, celex, UNION)

jager.full <- Reduce(function(x, y) merge(x, y, by="Word", all.x = TRUE), df_jager.list)
jager.full$lnWF <- log(jager.full$WF+1)
jager.full$logWF <- log10(jager.full$WF+1)
jager.full$logsqWF <- (log10(jager.full$WF+1))*(log10(jager.full$WF+1))
jager.full <- merge(jager.full, Hoffman.items[, c("Word", "SemD")], by = "Word", all.x = TRUE)
jager.full <- merge(jager.full, Brysbaert.items[, c("Word", "Conc")], by = "Word", all.x = TRUE)
jager.full <- merge(jager.full, SUBTL.items[c("Word", "SubCD")], by = "Word", all.x = T)
jager.full <- merge(jager.full, Johns.items[c("Word", "MemCD")], by = "Word", all.x = T)


library("ggplot2")
ifelse(!dir.exists(file.path(getwd(), outpath, 'Misc')), dir.create(file.path(getwd(), outpath, 'Misc')), FALSE)

pdf(paper="special", file=paste("./", outpath, "/Misc/jager_freq_split.pdf", sep = ""), height=5, width=7)
print(ggplot(jager.full, aes(logWF)) + geom_histogram() + facet_grid(Freq ~ Sense))
dev.off()

pdf(paper="special", file=paste("./", outpath, "/Misc/jager_sense_split.pdf", sep = ""), height=5, width=7)
print(ggplot(jager.full, aes(NOS)) + geom_histogram() + facet_grid(Sense ~ Freq))
dev.off()
