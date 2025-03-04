# generate correlation graphs

library(car)
library(ggplot2)
library(weights)
library(tibble)
library(grid)
library(gridExtra)
library(ggplotify)
library(gtable)
library(RColorBrewer)



#function for plotting binned "coplot" of various covariates by NOS, 
#where c = covariate and b = number of bins

#subfunctions for plotting binned "coplot" -- plotting multiple vs single bin

#for multiple regression: plot on residuals after fitting all other predictors than target instead of RT or Acc
#for multiple regression, use interact_effects plot on binned covariate values

plot_multi_megafigure <- function(Cov, B_start, method, wAxisy, wAxisx){
  
  AccPlts = list()
  RTPlts = list()
  
  yAxis = wAxisy
  xAxis = wAxisx
  
  for(d in 1:length(modeldata)){
    df <- df.list[[modeldata[d]]]
    partialmod = paste(setdiff(covariate.list, Cov), collapse = "+")
    df$resRT = resid(lm(paste("RT~NOS*", partialmod, sep = ""), data = df))
    df$resAcc = resid(glm(paste("Acc~NOS*", partialmod, sep = ""), family = binomial(link = "logit"), data=df))
    df$resAcc_scaled = scales::rescale(df$resAcc)
    
    print(paste(d, ":", modeldata[d]))
    
    B= B_start
    
    if(B > 1 & method == "overlap"){
      while(class(try(
        as.numeric(cut(df[, Cov],breaks = c(quantile(df[, Cov], prob = seq(0,1, by = (1/(B+1))), na.rm = T)),
                       labels = seq(1,B+1,1), include.lowest = TRUE)))) =="try-error" &
        B > 1){
        B = B-1
      }
    }else if(B > 1 & method == "discrete"){
      while(class(try(
        as.numeric(cut(df[, Cov],breaks = c(quantile(df[, Cov], prob = seq(0,1, by = (1/B)), na.rm = T)),
                       labels = seq(1,B,1), include.lowest = TRUE)))) =="try-error" &
        B > 1){
        B = B-1
      }
    }
    print(B)
    
    #set custom colour scale -- divide same 
    lightend = "skyblue"
    darkend = "navy"
    darker_blues <- colorRampPalette(colors = c(lightend, darkend))(B) #defaults to lightest colour if B = 1
    
    #set custom linetype scale -- takes first B types from list of line types
    ltlist = c("solid", "longdash", "twodash", "dotted")[1:B]
    
    if(B > 1){
      if(method == "overlap"){
        
        
        df$binhalves <- as.numeric(cut(df[, Cov], 
                                       breaks = c(quantile(df[, Cov], prob = seq(0,1, by = (1/(B+1))), na.rm = T)), 
                                       labels = seq(1,B+1,1), include.lowest = TRUE)) #not overlapping
        df$bins <- NA
        
        binmin = list()
        binmax = list()
        for (h in 1:(B+1)){
          binmin[[h]] <- min(subset(df, df$binhalves == h)[, Cov])
          
          binmax[[h]] <- max(subset(df, df$binhalves == h)[, Cov])
          
          dfcoded = df[FALSE,]
        }
        
        for (b in 1:B){
          subdf <- subset(df, df$binhalves == b | df$binhalves == b+1)
          subdf$bins <- paste(#paste(Cov,":", sep = ""),
                              paste(round(binmin[[b]], digit = 2), "-", round(binmax[[b+1]], digit = 2), sep = ""),
                              sep = "")
          dfcoded <- rbind(dfcoded, subdf)
        }
      }
      if(method == "discrete"){
        df$binNs <- as.numeric(cut(df[, Cov], 
                                   breaks = c(quantile(df[, Cov], prob = seq(0,1, by = (1/B)), na.rm = T)), 
                                   labels = seq(1,B,1), include.lowest = TRUE)) #not overlapping
        df$bins <- NA
        
        binmin = list()
        binmax = list()
        for (h in 1:B){
          binmin[[h]] <- min(subset(df, df$binNs == h)[, Cov])
          
          binmax[[h]] <- max(subset(df, df$binNs == h)[, Cov])
          
          dfcoded = df[FALSE,]
        }
        
        for (b in 1:B){
          subdf <- subset(df, df$binNs == b)
          subdf$bins <- paste(#paste(Cov,":", sep = ""),
                              paste(round(binmin[[b]], digit = 2), "-", round(binmax[[b]], digit = 2), sep = ""),
                              sep = "")
          dfcoded <- rbind(dfcoded, subdf)
        }
      }
    }
    
    if (B == 1){
      print("singlebin")
      
      df$bins <- 1
      
      binmin = list()
      binmax = list()
      
      binmin[[1]] <- min(df[, Cov])
      binmax[[1]] <- max(df[, Cov])
      
      dfcoded = df
      dfcoded$bins <- paste(#paste(Cov,":", sep = ""),
                            paste(round(binmin[[1]], digit = 2), "-", 
                                  round(binmax[[1]], digit = 2), sep = ""),
                            sep = "")
    }
    
    #NOTE: stats are left out of output for neatness
    outAcc <- by(data = dfcoded, INDICES = list(dfcoded$bins), FUN = function(x) {
      model <- glm(resAcc_scaled~NOS, data = x, family = "binomial")
      data.frame(
        b = coef(model)["NOS"], 
        sig = coef(summary(model))["NOS", "Pr(>|z|)"]
      )
    })
    statsAcc <- do.call("rbind", outAcc)
    statsAcc$bcolour <- ifelse(statsAcc$b < 0, "red", "blue")
    statsAcc$b <- paste("b =", rd(statsAcc$b,3))
    statsAcc$issig <- ifelse(statsAcc$sig<0.05, "*", "")
    statsAcc$sig <- paste("sig =", ifelse(statsAcc$sig<.001, "<.001", rd(statsAcc$sig,3)))
    
    statsAccdf <- data.frame(statsAcc)
    statsAccdf <- rownames_to_column(statsAccdf, "bins")
    statsAccdf$binsig = paste(statsAccdf$bins, statsAccdf$issig, sep = "")
    print(statsAccdf)
    
    #"binsig" column marks statistical significance for bin label in plot
    if("binsig" %in% colnames(dfcoded)){
      dfcoded = subset(dfcoded, select=-c(binsig))
    }
    if("bcolour" %in% colnames(dfcoded)){
      dfcoded = subset(dfcoded, select=-c(bcolour))
    }
    
    dfcoded = merge(dfcoded, statsAccdf, by = "bins")
    #print(unique(dfcoded$binsig))
    
    if(B > 1){
      #arrange dfcoded$binsig by dfcoded$binNs
      dfcoded$binsig <- forcats::fct_reorder(dfcoded$binsig, dfcoded$binNs)
    }
    
    
    pltAcc <- ggplot(dfcoded, aes(NOS, resAcc_scaled, colour = binsig)) +
      #geom_point(alpha = 1, shape = ".", aes(colour = bins)) +
      coord_cartesian(ylim = c(0.25, 1), xlim = c(0, 40)) +
      stat_smooth(method = "glm", method.args = list(family = "binomial"), 
                  aes(fill=binsig, linetype = binsig), show.legend = TRUE, size = 0.75, se = FALSE) + 
      #scale_color_brewer()+
      #scale_color_manual(values=darker_blues)+
      {if(B==1)
        scale_colour_manual(values = unique(dfcoded$bcolour))
        else
          scale_colour_manual(values = dfcoded[,c("binNs", "bcolour")] %>% distinct() %>% 
                                arrange(binNs) %>% select(bcolour) %>% unlist %>% unname)} +
      scale_linetype_manual(values=ltlist)+
      guides(color = guide_legend(
        override.aes=list(shape = 19))) +      
      {if(!(Cov %in% c("Conc", "SemD", "SubCD", "MemCD", "logWF", "old20", "posUni")))
      guides(color = guide_legend(
        nrow = 2, byrow = TRUE
        ))} +
	  labs(fill=paste(Cov, "range:"), linetype=paste(Cov, "range:"), colour=paste(Cov, "range:")) +
      theme_bw() + 
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text = element_text(size=16),
            legend.title=element_text(size=14),
            legend.text=element_text(size=14),
            legend.position = c(1, 0),
            legend.justification = c("right", "bottom"),
            legend.background = element_rect(fill="transparent"),
            legend.key.width = unit(26, "pt"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    if(!(modeldata[d] %in% yAxis)){
      pltAcc <- pltAcc + theme(axis.text.y=element_blank(),
                               axis.ticks.y=element_blank())
    }
	if(!(Cov %in% xAxis)){
      pltAcc <- pltAcc + theme(axis.text.x=element_blank(),
                             axis.ticks.x=element_blank())
    }
    
    a <- as.grob(pltAcc)
    
    AccPlts[[paste(Cov, "|", modeldata[d], sep = "")]] <- a
    
    
    outRT <- by(data = dfcoded, INDICES = list(dfcoded$bins), FUN = function(x) {
      model <- lm(resRT ~ NOS, data = x)
      data.frame(
        b = coef(model)["NOS"], 
        sig = coef(summary(model))["NOS", "Pr(>|t|)"]
      )
    })
    statsRT <- do.call("rbind", outRT)
    statsRT$bcolour <- ifelse(statsRT$b < 0, "red", "blue")
    statsRT$b <- paste("b =", rd(statsRT$b,3))
    statsRT$issig <- ifelse(statsRT$sig < 0.05, "*", "")
    statsRT$sig <- paste("sig =", ifelse(statsRT$sig<0.001, "<.001", rd(statsRT$sig,3)))
    
    statsRTdf <- data.frame(statsRT)
    statsRTdf <- rownames_to_column(statsRTdf, "bins")
    
    #marks statistical significance for bin label in plot
    dfcoded = merge(dfcoded, statsRTdf, by = "bins")
    dfcoded$bins = paste0(dfcoded$bins, dfcoded$issig)
    statsRTdf$binsig = paste(statsRTdf$bins, statsRTdf$issig, sep = "")
    print(statsRTdf)
    
    #binsig column marks statistical significance for bin label in plot
    if("binsig" %in% colnames(dfcoded)){
      dfcoded = subset(dfcoded, select=-c(binsig))
    }
    if("bcolour" %in% colnames(dfcoded)){
      dfcoded = subset(dfcoded, select=-c(bcolour))
    }
    dfcoded = merge(dfcoded, statsRTdf, by = "bins")
    
    if(B > 1){
      #arrange dfcoded$binsig by dfcoded$binNs
      dfcoded$binsig <- forcats::fct_reorder(dfcoded$binsig, dfcoded$binNs)
    }
    
    pltRT <- ggplot(dfcoded, aes(NOS, resRT, colour = binsig)) +
      #geom_point(alpha = 1, shape = ".", aes(colour = bins)) +
      coord_cartesian(ylim = c(-50,100), xlim = c(0,40)) +
      stat_smooth(method = "lm", aes(fill=binsig, linetype = binsig), show.legend = TRUE, size = 0.75, se = FALSE) +
      #scale_color_brewer()+
      #scale_color_manual(values=darker_blues)+
      {if(B==1)
        scale_colour_manual(values = unique(dfcoded$bcolour))
        else
          scale_colour_manual(values = dfcoded[,c("binNs", "bcolour")] %>% distinct() %>% 
                                arrange(binNs) %>% select(bcolour) %>% unlist %>% unname)} +
      scale_linetype_manual(values=ltlist)+
      guides(color = guide_legend(
        override.aes=list(shape = 19))) +      
      {if(!(Cov %in% c("Conc", "SemD", "SubCD", "MemCD", "logWF", "old20", "posUni")))
      guides(color = guide_legend(
        nrow = 2, byrow = TRUE
        ))} +
	  labs(fill=paste(Cov, "range:"), linetype=paste(Cov, "range:"), colour=paste(Cov, "range:")) +
      theme_bw() + 
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size=16),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.position = c(1, 1),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill="transparent"),
        legend.key.width = unit(26, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
    
    
    if(!(modeldata[d] %in% yAxis)){
      pltRT <- pltRT + theme(axis.text.y=element_blank(),
                             axis.ticks.y=element_blank())
    }
	if(!(Cov %in% xAxis)){
      pltRT <- pltRT + theme(axis.text.x=element_blank(),
                             axis.ticks.x=element_blank())
    }
    
    b <- as.grob(pltRT)
    
    RTPlts[[paste(Cov, "|", modeldata[d], sep = "")]] <- b
    
  }
  
  outPlts = list()
  outPlts[[1]] = AccPlts
  outPlts[[2]] = RTPlts
  return(outPlts)
}


#run functions to generate plots

maxPerPg = 4
bottomCovs = unique(c(covariate.list[seq(0, length(covariate.list), maxPerPg)], covariate.list[length(covariate.list)]))

allAccPlts = list()
allRTPlts = list()

for (cov in covariate.list){
  if (cov %in% colnames(df.list[[1]])){
    print(cov)
    covPlts = try(plot_multi_megafigure(Cov=cov, B_start=4, method = "discrete", wAxisy = c("VLD_AP_H-F"),wAxisx = bottomCovs))
    
    allAccPlts = c(allAccPlts, covPlts[[1]])
    allRTPlts = c(allRTPlts, covPlts[[2]])
  }
}

#arrange plots to print
#collate as separate pages (to fit document)
pgs = ceiling(length(covariate.list)/maxPerPg) #max 
for (pg in 1:pgs){
  
  startCov = ((pg-1)*maxPerPg+1)
  endCov = ifelse(pg == pgs, length(covariate.list), pg*maxPerPg)
  pgCovs = covariate.list[startCov:endCov]
  allAccPlts1 <- allAccPlts[grepl(paste(pgCovs, collapse = "|"), names(allAccPlts))] #get graphs with names containing any of the covariates in the page
  allRTPlts1 <- allRTPlts[grepl(paste(pgCovs, collapse = "|"), names(allRTPlts))]
  
  # Add row titles
  rlen <- length(modeldata)
  
  allAccPlts1[seq(1, length(pgCovs)*rlen, by=rlen)] = lapply(seq(1, length(pgCovs)*rlen, by=rlen), function(idx){arrangeGrob(allAccPlts1[[idx]], left=textGrob(pgCovs[(idx+(rlen-1))/rlen], rot = 90, gp = gpar(fontsize = 24)))})
  
  allRTPlts1[seq(1, length(pgCovs)*rlen, by=rlen)] = lapply(seq(1, length(pgCovs)*rlen, by=rlen), function(idx){arrangeGrob(allRTPlts1[[idx]], left=textGrob(pgCovs[(idx+(rlen-1))/rlen], rot = 90, gp = gpar(fontsize = 24)))})
  
  WList = c(3,rep(2.5, length(modeldata)-1))
  HList = c(3.4, rep(3.2, length(pgCovs)-2), 3.5)
  
  # Add column titles and lay out plots
  allAccPlts1[1:rlen] = lapply(1:rlen, function(idx){arrangeGrob(allAccPlts1[[idx]], top=textGrob(modeldata[idx], gp = gpar(fontsize = 26)))})
  accp <- arrangeGrob(grobs = allAccPlts1, ncol = rlen, 
                      left = textGrob("Scaled Residuals of Accuracy (proportion) by Multiple Covariate Regression", gp=gpar(fontsize = 22), rot=90),
                      bottom = textGrob("NOS", gp=gpar(fontsize = 22)),
                      widths = WList, heights = HList)
  
  
  allRTPlts1[1:rlen] = lapply(1:rlen, function(idx){arrangeGrob(allRTPlts1[[idx]], top=textGrob(modeldata[idx], gp = gpar(fontsize = 24)))})
  
  rtp <- arrangeGrob(grobs = allRTPlts1, ncol = rlen, 
                     left = textGrob("Residuals of Reaction Time (ms) by Multiple Covariate Regression", gp=gpar(fontsize = 22), rot=90),
                     bottom = textGrob("NOS", gp=gpar(fontsize = 22)),
                     widths = WList, heights = HList)
  
  w <- 3 * length(df.list) #width and height of compiled plots
  h <- 4 * length(pgCovs)
  
  ggsave(paste(subfolder, "\\", dflistname, "multi_accp", pg, ".png", sep=""), accp, height = h, width = w) 
  ggsave(paste(subfolder, "\\", dflistname, "multi_rtp", pg, ".png", sep=""), rtp, height = h, width = w)
  
}   
