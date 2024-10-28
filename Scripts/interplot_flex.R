#check what the ylims are with finalised dataset
#figure out how to split into pages without manually doing so (change covlist, h, minmax, wAxis)

library(interplot)
library(grid)
library(gridExtra)
library(ggplotify)
library(ggpubr)

#functions for rounding

scaleFUN1dp <- function(x) {
  sub("^0+", "", sub(" ", "",format(round(x, digits = 1), nsmall = 1))) 
} #round ticks in scales to 1 decimal place without leading zeros
scaleFUN2dp <- function(x) {
  sub("^0+", "", sub(" ", "",format(round(x, digits = 2), nsmall = 2))) 
  } #round ticks in scales to 2 decimal places without leading zeros
scaleFUN3dp <- function(x) {
  sub("^0+", "", sub(" ", "",format(round(x, digits = 3), nsmall = 3))) 
} #round ticks in scales to 3 decimal places without leading zeros

no_leading_zero <- function(x) {
  y <- sprintf('%.1f',x)
  y[x > 0 & x < 1] <- sprintf('.%s',x[x > 0 & x < 1]*10)
  y[x == 0] <- '0'
  y[x > -1 & x < 0] <- sprintf('-.%s',x[x > -1 & x < 0]*-10)
  y
}

##set variables for formatting
dflist <- df.list
ylist <- c("RT", "Acc")
ticks <- c(0, .25, .5, .75, 1) #ticks for bottom x-axis (i.e. percentile)
covperpg = 8 #how many covariates per page (max)
####

#create and save all plots in list, split in collation
#need to first figure out how many per page -- hence which covariates should have x axis

if(ptype == "multi"|ptype == "simple"){
  covlist <- covariate.list
  
  #dflist <- df.list
}else if(ptype == "pca"){
  covlist <- pca.list
  
  #dflist <- df.list
}else{
  print("Error: ptype should be 'multi', 'simple' or 'pca'")
}

#limits for y-axis: current table sets scale of y axes to be consistent across each row
ylimstable <- read.csv(".\\Input\\Misc\\interplot_ylims.csv")
colnames(ylimstable) = c("model",	"predictor", "RT_min", "RT_max", "Acc_min", "Acc_max") #importing process can introduce strange characters
minmax <- ylimstable[ylimstable$model == ptype,]

#split into multiple lists for collation

pgs = ceiling(length(covlist)/covperpg) #how many pages needed per page
ind.bottomAxis = unique(c(seq(0,length(covlist),covperpg), length(covlist)))[-1] #for percentile plot, only last cov on the page has bottom x-axis (index)
#wAxis.x.bottom = covlist[ind.bottomAxis] #cov(s) with bottom x-axis
wAxis.x.bottom = covlist #cov(s) with bottom x-axis
wAxis.y <- c(modeldata[1]) #only first dataset on the page has y axis


RTInterplts <- list() #list of all plots
AccInterplts <- list() #list of all plots
for (j in covlist){
  for(k in modeldata){
    DF <- dflist[[k]][,covlist]
    for(cov in covlist){
      DF[,cov] <- ecdf(dflist[[k]][,cov])(dflist[[k]][,cov])
    }
    colnames(DF) <- paste(covlist, "P", sep = "")
    dflist[[k]] <- cbind(dflist[[k]], DF)
    
    for(i in ylist){
      if(ptype=="multi"){
        #plot by raw value
        m_model = ifelse ((k == "VLD_AP_H-F" | k == "VLD_AP_V-D"),
                              paste(i, paste("~ NOS * (", paste(covariate.list[-which(covariate.list=="nAdv")], collapse = ' + '), ")")),
                              paste(i, paste("~ NOS * (", paste(covlist, collapse = ' + '), ")")))
        if(!((k == "VLD_AP_H-F" | k == "VLD_AP_V-D") & j == "nAdv")){
          p <- interplot(lm(m_model, data = dflist[[k]]), j, var1 = "NOS", stats_cp = "ci")
        }
        #plot by percentile value
        #p <- interplot(lm(paste(i, paste("~ NOS * (", paste(paste(covlist,"P", sep =""), collapse = ' + '), ")")), 
        #                  data = dflist[[k]]), paste(j,"P",sep=""), var1 = "NOS", stats_cp = "ci")
      }else if(ptype=="pca"){
        #plot by raw value
        p <- interplot(lm(paste(i, paste("~ NOS * (", paste(covlist, collapse = ' + '), ")")), 
                          data = dflist[[k]]), j, var1 = "NOS", stats_cp = "ci")
        
        #plot by percentile value
        #p <- interplot(lm(paste(i, paste("~ NOS * (", paste(paste(covlist,"P", sep =""), collapse = ' + '), ")")), 
        #                  data = dflist[[k]]), paste(j,"P",sep=""), var1 = "NOS", stats_cp = "ci")
        #plot by raw value
      }else if(ptype=="simple"){
        #plot by raw value
        p <- interplot(lm(paste(i,"~NOS*",j,sep=""), data = dflist[[k]]), 
                       var2 = j, var1 = "NOS", stats_cp = "ci")
        
        #plot by percentile value
        #p <- interplot(lm(paste(i,"~NOS*",paste(j,"P",sep=""),sep=""), data = dflist[[k]]), 
        #               var2 = paste(j,"P",sep=""), var1 = "NOS", stats_cp = "ci")
     }
      
      if(ptype == "multi" & (k == "VLD_AP_H-F" | k == "VLD_AP_V-D") & j == "nAdv"){
        p <- ggplot(data = dflist[[k]][c("NOS", "nAdv")], aes(NOS,nAdv)) + 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"),
                axis.text.x = element_text(size=16), axis.text.y = element_text(size=16, angle = 90, hjust = 0.5),
                axis.title.y = element_blank())+
          theme(axis.title.x=element_blank(),axis.text.x.bottom = element_text(colour = "white"))
        
      }else{
        p <- p + #ylab("estimated coefficient of NOS") + xlab("percentile") + #"estimated coefficient of NOS"
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"),
                axis.text.x = element_text(size=16), axis.text.y = element_text(size=16),
                axis.title.x = element_text(size=24), 
                axis.title.y = element_text(size=24)) +
          theme(axis.text.x.top = element_text(angle = 0, hjust = 0.5), #45, 0.2
                axis.text.y = element_text(angle = 90, hjust = 0.5)) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          scale_x_continuous(guide = guide_axis(n.dodge = 2)) +
          #scale_x_continuous(labels = scales::percent_format(accuracy=1))+
          #scale_x_continuous(sec.axis=sec_axis(~., breaks=ticks, 
          #                                     #labels=sub("^0+", "", round(quantile(dflist[[k]][,j], probs = ticks, na.rm =T), digits = 2)),
          #                                     labels=scaleFUN1dp(quantile(dflist[[k]][,j], probs = ticks, na.rm =T)),
          #                                     name = j))+
          labs(caption=NULL)
      }
      
      if(!(k %in% wAxis.y)){
        p <- p + theme(axis.text.y=element_blank())
      }
      if(!(j %in% wAxis.x.bottom)){
        p <- p + theme(axis.title.x=element_blank(),axis.text.x.bottom = element_blank())
      }else{
        p <- p + theme(axis.title.x.top=element_blank())
      }

      if(i == "RT"){
        p <- p + ylim(minmax[minmax$predictor == j , "RT_min"], minmax[minmax$predictor == j , "RT_max"])
        
        RTInterplts[[paste(k, j, sep=":")]] <- as.grob(p) #saves plots by name
      }else{
        p <- p + ylim(minmax[minmax$predictor == j , "Acc_min"], minmax[minmax$predictor == j , "Acc_max"])# +
        #  scale_y_continuous(labels=scaleFUN3dp)
        AccInterplts[[paste(k, j, sep=":")]] <- as.grob(p)        
      }
      
      #ggsave(filename = paste(subfolder, "\\", ptype, k, j, i,".png", sep=""), plot = p, width = 3, height = 4) #uncomment if need individual plots
    }
  }
}

#collate as separate pages (to fit document)
for (pg in 1:pgs){
  
  startCov = ifelse(pg == 1, 1, ind.bottomAxis[pg-1]+1)
  endCov = ind.bottomAxis[pg]
  pgCovs = covlist[startCov:endCov]
  AccInterplts1 <- AccInterplts[grepl(paste(pgCovs, collapse = "|"), names(AccInterplts))] #get graphs with names containing any of the covariates in the page
  RTInterplts1 <- RTInterplts[grepl(paste(pgCovs, collapse = "|"), names(RTInterplts))]
  
  # Add row titles
  rlen <- length(modeldata)
  
  AccInterplts1[seq(1, length(pgCovs)*rlen, by=rlen)] = lapply(seq(1, length(pgCovs)*rlen, by=rlen), function(idx){arrangeGrob(AccInterplts1[[idx]], left=textGrob(pgCovs[(idx+(rlen-1))/rlen], rot = 90, gp = gpar(fontsize = 24)))})
  
  RTInterplts1[seq(1, length(pgCovs)*rlen, by=rlen)] = lapply(seq(1, length(pgCovs)*rlen, by=rlen), function(idx){arrangeGrob(RTInterplts1[[idx]], left=textGrob(pgCovs[(idx+(rlen-1))/rlen], rot = 90, gp = gpar(fontsize = 24)))})
  
  WList = c(3,rep(2.5, length(modeldata)-1))
  HList = c(3.5, rep(3, length(pgCovs)-2), 3.5)
  
  # Add column titles and lay out plots
  AccInterplts1[1:rlen] = lapply(1:rlen, function(idx){arrangeGrob(AccInterplts1[[idx]], top=textGrob(modeldata[idx], gp = gpar(fontsize = 24)))})
  accip <- arrangeGrob(grobs = AccInterplts1, ncol = rlen, 
                       left = textGrob("Estimated slope of regression with NOS", gp=gpar(fontsize = 22), rot=90),
                       #top = textGrob("Covariate values", gp=gpar(fontsize = 22)), #for percentile plot
                       #bottom = textGrob("Percentile values", gp=gpar(fontsize = 22)), #for percentile plot
                       bottom = textGrob("Covariate values", gp=gpar(fontsize = 22)),
                       widths = WList, heights = HList)
  
  
  RTInterplts1[1:rlen] = lapply(1:rlen, function(idx){arrangeGrob(RTInterplts1[[idx]], top=textGrob(modeldata[idx], gp = gpar(fontsize = 24)))})
  
  rtip <- arrangeGrob(grobs = RTInterplts1, ncol = rlen, 
                      left = textGrob("Estimated slope of regression with NOS", gp=gpar(fontsize = 22), rot=90),
                      #top = textGrob("Covariate values", gp=gpar(fontsize = 22)), #for percentile plot
                      #bottom = textGrob("Percentile values", gp=gpar(fontsize = 22)), #for percentile plot
                      bottom = textGrob("Covariate values", gp=gpar(fontsize = 22)),
                      widths = WList, heights = HList)
  
  w <- 2.75 * length(df.list) #width and height of compiled plots
  h <- 4 * length(pgCovs)
  
  ggsave(paste(subfolder, "\\", dflistname, ptype, "accip", pg, ".png", sep=""), accip, height = h, width = w) 
  ggsave(paste(subfolder, "\\", dflistname, ptype, "rtip", pg, ".png", sep=""), rtip, height = h, width = w)
  
}   